package wvlet.lang.cli

import wvlet.airframe.control.Control
import wvlet.airframe.launcher.argument
import wvlet.airframe.launcher.option
import wvlet.lang.api.StatusCode
import wvlet.lang.api.v1.query.QuerySelection
import wvlet.lang.catalog.Profile
import wvlet.lang.compiler.codegen.CodeFormatterConfig
import wvlet.lang.compiler.codegen.GenSQL
import wvlet.lang.compiler.codegen.WvletGenerator
import wvlet.lang.compiler.CompilationUnit
import wvlet.lang.compiler.CompileResult
import wvlet.lang.compiler.Compiler
import wvlet.lang.compiler.CompilerOptions
import wvlet.lang.compiler.Context
import wvlet.lang.compiler.DBType
import wvlet.lang.compiler.Phase
import wvlet.lang.compiler.Symbol
import wvlet.lang.compiler.WorkEnv
import wvlet.lang.compiler.analyzer.refactor.*
import wvlet.lang.runner.QueryExecutor
import wvlet.lang.runner.connector.DBConnector
import wvlet.lang.runner.connector.DBConnectorProvider
import wvlet.log.LogSupport

case class WvletCompilerOption(
    @option(prefix = "-w", description = "Working folder")
    workFolder: String = ".",
    @option(prefix = "-f,--file", description = "Read a query from the given .wv file")
    file: Option[String] = None,
    @argument(description = "query")
    query: Option[String] = None,
    @option(prefix = "-t,--target", description = "Target database type")
    targetDBType: Option[String] = None,
    @option(prefix = "--profile", description = "Profile to use")
    profile: Option[String] = None,
    @option(prefix = "--catalog", description = "Context database catalog to use")
    catalog: Option[String] = None,
    @option(prefix = "--schema", description = "Context database schema to use")
    schema: Option[String] = None
)

/**
  * Options for pattern analysis command
  */
case class PatternAnalysisOption(
    @option(prefix = "--min-occurrences", description = "Minimum occurrences to detect as duplicate")
    minOccurrences: Int = 2,
    @option(prefix = "--min-nodes", description = "Minimum node count for patterns")
    minNodes: Int = 3,
    @option(prefix = "--max-params", description = "Maximum parameters for model extraction")
    maxParams: Int = 5,
    @option(prefix = "--aggressive", description = "Use aggressive detection settings")
    aggressive: Boolean = false,
    @option(prefix = "--conservative", description = "Use conservative detection settings")
    conservative: Boolean = false,
    @option(prefix = "--top", description = "Number of top suggestions to show")
    top: Int = 10,
    @option(prefix = "--json", description = "Output results as JSON")
    json: Boolean = false
)

class WvletCompiler(
    opts: WvletGlobalOption,
    compilerOption: WvletCompilerOption,
    workEnv: WorkEnv,
    dbConnectorProvider: DBConnectorProvider
) extends LogSupport
    with AutoCloseable:

  private lazy val currentProfile: Profile =
    // Resolve the profile from DBType or profile name
    compilerOption.targetDBType match
      case Some(dbType) =>
        if compilerOption.profile.isDefined then
          throw StatusCode
            .INVALID_ARGUMENT
            .newException("Specify either -t (--target) or --profile")
        val resolvedDBType = DBType.fromString(dbType)
        debug(s"Using syntax for ${resolvedDBType}")
        Profile.defaultProfileFor(resolvedDBType)
      case _ =>
        Profile.getProfile(compilerOption.profile, compilerOption.catalog, compilerOption.schema)

  private var _dbConnector: DBConnector = null

  private def getDBConnector: DBConnector = synchronized {
    if _dbConnector == null then
      _dbConnector = dbConnectorProvider.getConnector(currentProfile)
    _dbConnector
  }

  override def close(): Unit = Option(_dbConnector).foreach(_.close())

  private def createCompiler(parseOnly: Boolean = false): Compiler =
    val dbType = compilerOption.targetDBType.map(DBType.fromString).getOrElse(currentProfile.dbType)

    val options = CompilerOptions(
      sourceFolders = List(compilerOption.workFolder),
      workEnv = workEnv,
      catalog = currentProfile.catalog,
      schema = currentProfile.schema,
      dbType = dbType
    )

    val compiler =
      if parseOnly then
        Compiler.parseOnly(options)
      else
        Compiler(options)
    // Set catalog from connector
    currentProfile
      .catalog
      .foreach { catalog =>
        val schema = currentProfile.schema.getOrElse("main")
        compiler.setDefaultCatalog(getDBConnector.getCatalog(catalog, schema))
      }

    currentProfile
      .schema
      .foreach { schema =>
        compiler.setDefaultSchema(schema)
      }

    compiler

  end createCompiler

  private def getInputUnit(forSQL: Boolean = false): CompilationUnit =
    (compilerOption.file, compilerOption.query) match
      case (Some(f), None) =>
        CompilationUnit.fromFile(s"${compilerOption.workFolder}/${f}".stripPrefix("./"))
      case (None, Some(q)) =>
        if forSQL then
          CompilationUnit.fromSqlString(q)
        else
          CompilationUnit.fromWvletString(q)
      case _ =>
        throw StatusCode.INVALID_ARGUMENT.newException("Specify either --file or a query argument")

  private def compile(inputUnit: CompilationUnit): CompileResult = createCompiler()
    .compileSingleUnit(inputUnit)

  private def compileInternal(inputUnit: CompilationUnit, parseOnly: Boolean = false): Context =
    val compileResult =
      if parseOnly then
        val parsingCompiler = createCompiler(parseOnly = true)
        parsingCompiler.compileSingleUnit(inputUnit)
      else
        compile(inputUnit)

    compileResult.reportAllErrors

    compileResult
      .context
      .withCompilationUnit(inputUnit)
      // Disable debug path as we can't run tests in plain SQL
      .withDebugRun(false)
      .newContext(Symbol.NoSymbol)

  def generateSQL: String =
    val inputUnit = getInputUnit(forSQL = false)
    val ctx       = compileInternal(inputUnit)
    GenSQL.generateSQL(inputUnit)(using ctx)

  def generateWvlet: String =
    val inputUnit = getInputUnit(forSQL = true)
    // For SQL to Wvlet conversion, we only need to parse the SQL, not run full compilation
    val ctx = compileInternal(inputUnit, parseOnly = inputUnit.sourceFile.isSQL)

    // Get the resolved logical plan from the compilation unit
    val logicalPlan = inputUnit.resolvedPlan

    // Create a WvletGenerator with the appropriate database type configuration
    val config    = CodeFormatterConfig(sqlDBType = ctx.dbType)
    val generator = WvletGenerator(config)(using ctx)

    // Convert the logical plan to Wvlet flow-style syntax
    generator.print(logicalPlan)

  def run(): Unit =
    val compiler = createCompiler()
    Control.withResource(
      QueryExecutor(dbConnectorProvider, currentProfile, compiler.compilerOptions.workEnv)
    ) { executor =>
      val inputUnit     = getInputUnit(forSQL = false)
      val compileResult = compile(inputUnit)
      given Context     = compileResult.context

      val queryResult = executor.executeSelectedStatement(
        inputUnit,
        QuerySelection.All,
        linePosition = inputUnit.resolvedPlan.sourceLocation.position,
        compileResult.context
      )
      println(queryResult.toPrettyBox())
    }

  /**
    * Analyze the query for duplicate patterns that could be refactored
    */
  def analyzePatterns(patternOption: PatternAnalysisOption): Unit =
    val inputUnit = getInputUnit(forSQL = false)
    val ctx       = compileInternal(inputUnit)

    // Get the resolved logical plan
    val logicalPlan = inputUnit.resolvedPlan

    // Configure pattern extraction based on options
    val refactorConfig =
      if patternOption.aggressive then
        RefactorConfig.aggressive
      else if patternOption.conservative then
        RefactorConfig.conservative
      else
        RefactorConfig(
          minOccurrences = patternOption.minOccurrences,
          minPatternSize = patternOption.minNodes,
          maxParameters = patternOption.maxParams
        )

    val collectorConfig =
      if patternOption.aggressive then
        CollectorConfig(minDepth = 1, minNodeCount = 2)
      else if patternOption.conservative then
        CollectorConfig(minDepth = 3, minNodeCount = 5)
      else
        CollectorConfig(minDepth = 2, minNodeCount = patternOption.minNodes)

    val detectorConfig =
      if patternOption.aggressive then
        DetectorConfig.aggressive
      else if patternOption.conservative then
        DetectorConfig(minOccurrences = 3, minNodeCount = 5, minDepth = 3)
      else
        DetectorConfig(
          minOccurrences = patternOption.minOccurrences,
          minNodeCount = patternOption.minNodes
        )

    val extractorConfig = PatternExtractorConfig(
      enabled = true,
      refactorConfig = refactorConfig,
      collectorConfig = collectorConfig,
      detectorConfig = detectorConfig,
      maxSuggestions = patternOption.top
    )

    // Run pattern extraction
    val result = PatternExtractor.analyze(logicalPlan, extractorConfig)

    // Output results
    if patternOption.json then
      printJsonResult(result)
    else
      printTextResult(result, patternOption.top)

  private def printTextResult(result: PatternExtractionResult, topN: Int): Unit =
    println("=" * 60)
    println("Pattern Analysis Report")
    println("=" * 60)
    println()

    println(s"Summary:")
    println(s"  - Subtrees analyzed: ${result.detectionResult.totalSubtrees}")
    println(s"  - Unique patterns: ${result.detectionResult.uniqueHashes}")
    println(s"  - Duplicate groups: ${result.detectionResult.groups.size}")
    println(s"  - Actionable suggestions: ${result.suggestions.size}")
    println(s"  - Total potential reduction: ${result.totalPotentialReduction} nodes")
    println()

    if result.suggestions.isEmpty then
      println("No refactoring suggestions found.")
      println("Try using --aggressive flag for more patterns, or check if the query has repeated structures.")
    else
      println(s"Top ${math.min(topN, result.suggestions.size)} Refactoring Suggestions:")
      println("-" * 60)

      result.suggestions.take(topN).foreach { suggestion =>
        println()
        println(s"#${suggestion.rank} ${suggestion.suggestedModelName}")
        println(s"  Decision: ${suggestion.decision.summary}")
        println(s"  Occurrences: ${suggestion.group.occurrences}")
        println(s"  Parameters needed: ${suggestion.parameterCount}")

        if suggestion.group.isCrossQuery then
          println(s"  Type: Cross-query pattern")
          println(s"  Sources: ${suggestion.group.sources.mkString(", ")}")
        else
          println(s"  Type: Single-query pattern")

        suggestion.unifyResult.foreach { unify =>
          if unify.variableParameters.nonEmpty then
            println(s"  Extracted parameters:")
            unify.variableParameters.foreach { param =>
              println(s"    - ${param.id}: ${param.inferredType} (${param.distinctValueCount} distinct values)")
            }
        }

        if suggestion.decision.warnings.nonEmpty then
          println(s"  Warnings: ${suggestion.decision.warnings.mkString(", ")}")
      }

    println()
    println("=" * 60)

  private def printJsonResult(result: PatternExtractionResult): Unit =
    import scala.collection.mutable.StringBuilder
    val json = new StringBuilder
    json.append("{\n")
    json.append(s"""  "totalSubtrees": ${result.detectionResult.totalSubtrees},\n""")
    json.append(s"""  "uniquePatterns": ${result.detectionResult.uniqueHashes},\n""")
    json.append(s"""  "duplicateGroups": ${result.detectionResult.groups.size},\n""")
    json.append(s"""  "actionableSuggestions": ${result.suggestions.size},\n""")
    json.append(s"""  "totalPotentialReduction": ${result.totalPotentialReduction},\n""")
    json.append("""  "suggestions": [""")

    result.suggestions.zipWithIndex.foreach { case (s, idx) =>
      if idx > 0 then json.append(",")
      json.append("\n    {\n")
      json.append(s"""      "rank": ${s.rank},\n""")
      json.append(s"""      "modelName": "${s.suggestedModelName}",\n""")
      json.append(s"""      "score": ${s.decision.score},\n""")
      json.append(s"""      "occurrences": ${s.group.occurrences},\n""")
      json.append(s"""      "parameters": ${s.parameterCount},\n""")
      json.append(s"""      "estimatedReduction": ${s.decision.estimatedReduction},\n""")
      json.append(s"""      "isCrossQuery": ${s.group.isCrossQuery},\n""")
      json.append(s"""      "reason": "${s.decision.reason.replace("\"", "\\\"")}"\n""")
      json.append("    }")
    }

    json.append("\n  ]\n")
    json.append("}\n")
    println(json.toString())

end WvletCompiler
