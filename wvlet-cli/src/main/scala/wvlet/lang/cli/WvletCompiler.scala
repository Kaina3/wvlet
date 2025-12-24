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
    json: Boolean = false,
    @option(prefix = "-o,--output", description = "Output file path (stdout if not specified)")
    output: Option[String] = None,
    @option(
      prefix = "--parse-only",
      description = "Parse-only mode (skip typing/analysis phases) for faster bulk processing"
    )
    parseOnly: Boolean = false,
    @option(prefix = "--dir", description = "Directory of .wv files to analyze (enables cross-query analysis)")
    dir: Option[String] = None,
    @option(prefix = "--offset", description = "Skip first N files in directory (for batch processing)")
    offset: Int = 0,
    @option(prefix = "--limit", description = "Limit number of files to analyze from directory")
    limit: Option[Int] = None,
    @option(prefix = "--pattern", description = "File pattern to match (default: *.wv)")
    pattern: String = "*.wv"
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
    // Build extraction config from options
    val extractorConfig = buildExtractorConfig(patternOption)

    // Check if directory mode or single file mode
    patternOption.dir match
      case Some(dirPath) =>
        analyzePatternsFromDirectory(dirPath, patternOption, extractorConfig)
      case None =>
        analyzePatternsFromSingleFile(patternOption, extractorConfig)

  /**
    * Analyze patterns from a single file
    */
  private def analyzePatternsFromSingleFile(
      patternOption: PatternAnalysisOption,
      extractorConfig: PatternExtractorConfig
  ): Unit =
    val inputUnit   = getInputUnit(forSQL = false)
    val ctx         = compileInternal(inputUnit, parseOnly = patternOption.parseOnly)
    val logicalPlan = inputUnit.resolvedPlan

    val result = PatternExtractor.analyze(logicalPlan, extractorConfig)

    val output = if patternOption.json then
      formatJsonResult(result, 1)
    else
      formatTextResult(result, patternOption.top, 1)

    writeOutput(output, patternOption.output)

  /**
    * Analyze patterns from multiple files in a directory
    */
  private def analyzePatternsFromDirectory(
      dirPath: String,
      patternOption: PatternAnalysisOption,
      extractorConfig: PatternExtractorConfig
  ): Unit =
    import java.io.File
    import java.nio.file.{Files, Paths, FileSystems}

    val dir = new File(dirPath)
    if !dir.exists() || !dir.isDirectory then
      throw StatusCode.INVALID_ARGUMENT.newException(s"Directory not found: $dirPath")

    // Find matching files
    val matcher   = FileSystems.getDefault.getPathMatcher(s"glob:${patternOption.pattern}")
    val allFiles  = dir.listFiles().filter(f => f.isFile && matcher.matches(Paths.get(f.getName))).toList.sortBy(_.getName)
    val filesAfterOffset =
      if patternOption.offset <= 0 then allFiles
      else allFiles.drop(patternOption.offset)
    val filesToProcess = patternOption.limit match
      case Some(n) => filesAfterOffset.take(n)
      case None    => filesAfterOffset

    if filesToProcess.isEmpty then
      println(s"No files matching '${patternOption.pattern}' found in $dirPath")
      return

    println(s"Analyzing ${filesToProcess.size} files from $dirPath...")
    println()

    // Compile all files and collect plans
    // NOTE: Even Compiler.parseOnly still parses *all* units in sourceFolders.
    // For bulk directory checks, we want a fast path that parses only the target file.
    val compiler          = if patternOption.parseOnly then null else createCompiler(parseOnly = false)
    var successCount      = 0
    var failCount         = 0
    var totalOriginalNodes = 0
    val plans             = scala.collection.mutable.ListBuffer[(String, wvlet.lang.model.plan.LogicalPlan)]()
    val failedFiles       = scala.collection.mutable.ListBuffer[(String, String)]()
    val startTime         = System.currentTimeMillis()

    filesToProcess.zipWithIndex.foreach { case (file, idx) =>
      if (idx + 1) % 10 == 0 || idx == 0 then
        print(s"\rProcessing file ${idx + 1}/${filesToProcess.size}...")
        System.out.flush()
      try
        val unit = CompilationUnit.fromFile(file.getAbsolutePath)
        val plan =
          if patternOption.parseOnly then
            // Parse-only fast path: parse only this file (no full compiler context)
            val p = wvlet.lang.compiler.parser.ParserPhase.parseOnly(unit)
            unit.unresolvedPlan = p
            unit.resolvedPlan = p
            p
          else
            compiler.compileSingleUnit(unit)
            unit.resolvedPlan
        if plan != null then
          plans += ((file.getName, plan))
          totalOriginalNodes += countNodes(plan)
          successCount += 1
      catch
        case e: Exception =>
          failedFiles += ((file.getName, e.getMessage))
          failCount += 1
    }

    val compileTime = System.currentTimeMillis() - startTime
    println(s"\rCompilation completed in ${compileTime}ms")
    println(s"Successfully compiled: $successCount files")
    if failCount > 0 then
      println(s"Failed to compile: $failCount files")
      if failedFiles.size <= 10 then
        failedFiles.foreach { case (name, msg) =>
          println(s"  - $name: ${msg.take(80)}")
        }
      else
        println(s"  (showing first 5 failures)")
        failedFiles.take(5).foreach { case (name, msg) =>
          println(s"  - $name: ${msg.take(80)}")
        }
    println(s"Total original nodes: $totalOriginalNodes")
    println()

    if plans.isEmpty then
      println("No valid plans to analyze")
      return

    println("Running cross-query pattern analysis...")
    val analysisStartTime = System.currentTimeMillis()

    // Run cross-query analysis
    val result = PatternExtractor.analyzeMultiple(plans.toList, extractorConfig)

    val analysisTime = System.currentTimeMillis() - analysisStartTime
    println(s"Analysis completed in ${analysisTime}ms")
    println()

    val output = if patternOption.json then
      formatJsonResult(result, plans.size, Some(totalOriginalNodes))
    else
      formatTextResult(result, patternOption.top, plans.size, Some(totalOriginalNodes))

    writeOutput(output, patternOption.output)

  /**
    * Count nodes in a LogicalPlan tree
    */
  private def countNodes(plan: wvlet.lang.model.plan.LogicalPlan): Int =
    1 + plan.children.map(countNodes).sum

  /**
    * Build PatternExtractorConfig from options
    */
  private def buildExtractorConfig(patternOption: PatternAnalysisOption): PatternExtractorConfig =
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

    PatternExtractorConfig(
      enabled = true,
      refactorConfig = refactorConfig,
      collectorConfig = collectorConfig,
      detectorConfig = detectorConfig,
      maxSuggestions = patternOption.top,
      crossQueryAnalysis = patternOption.dir.isDefined
    )

  private def formatTextResult(
      result: PatternExtractionResult,
      topN: Int,
      fileCount: Int,
      totalOriginalNodes: Option[Int] = None
  ): String =
    val sb = new StringBuilder()
    sb.append("=" * 60).append("\n")
    sb.append("Pattern Analysis Report\n")
    sb.append("=" * 60).append("\n")
    sb.append("\n")

    sb.append(s"Summary:\n")
    sb.append(s"  - Files analyzed: $fileCount\n")
    totalOriginalNodes.foreach { total =>
      sb.append(s"  - Total original nodes: $total\n")
    }
    sb.append(s"  - Subtrees analyzed: ${result.detectionResult.totalSubtrees}\n")
    sb.append(s"  - Unique patterns: ${result.detectionResult.uniqueHashes}\n")
    sb.append(s"  - Duplicate groups: ${result.detectionResult.groups.size}\n")

    // Show hierarchy information if available
    result.hierarchyResult match
      case Some(hierarchy) =>
        sb.append(s"  - All actionable suggestions: ${hierarchy.allSuggestions.size}\n")
        sb.append(s"  - Optimal suggestions (for modeling): ${hierarchy.optimalSuggestions.size}\n")
        sb.append(s"  - Subsumed (nested) patterns: ${result.subsumedCount}\n")
        sb.append(s"  - Optimal reduction: ${result.optimalReduction} nodes\n")
        totalOriginalNodes.foreach { total =>
          if total > 0 then
            val reductionPct = (result.optimalReduction.toDouble / total) * 100
            sb.append(f"  - Optimal reduction rate: $reductionPct%.2f%%\n")
        }
      case None =>
        sb.append(s"  - Actionable suggestions: ${result.suggestions.size}\n")
        sb.append(s"  - Total potential reduction: ${result.totalPotentialReduction} nodes\n")
        totalOriginalNodes.foreach { total =>
          if total > 0 then
            val reductionPct = (result.totalPotentialReduction.toDouble / total) * 100
            sb.append(f"  - Overall reduction rate: $reductionPct%.2f%%\n")
        }
    sb.append("\n")

    if result.suggestions.isEmpty then
      sb.append("No refactoring suggestions found.\n")
      sb.append("Try using --aggressive flag for more patterns, or check if the query has repeated structures.\n")
    else
      val optimalSuggestions = result.optimalSuggestions
      sb.append(s"Top ${math.min(topN, optimalSuggestions.size)} Optimal Refactoring Suggestions:\n")
      sb.append("-" * 60).append("\n")

      optimalSuggestions.take(topN).foreach { suggestion =>
        sb.append("\n")
        sb.append(s"#${suggestion.rank} ${suggestion.suggestedModelName}\n")
        sb.append(s"  Decision: ${suggestion.decision.summary}\n")
        sb.append(s"  Occurrences: ${suggestion.group.occurrences}\n")
        sb.append(s"  Parameters needed: ${suggestion.parameterCount}\n")

        if suggestion.group.isCrossQuery then
          sb.append(s"  Type: Cross-query pattern\n")
          sb.append(s"  Sources: ${suggestion.group.sources.mkString(", ")}\n")
        else
          sb.append(s"  Type: Single-query pattern\n")
          if suggestion.group.sources.nonEmpty then
            sb.append(s"  Source: ${suggestion.group.sources.head}\n")

        // Show hierarchy info
        if suggestion.subsumes.nonEmpty then
          sb.append(s"  Contains: ${suggestion.subsumes.size} nested pattern(s)\n")

        suggestion.unifyResult.foreach { unify =>
          if unify.variableParameters.nonEmpty then
            sb.append(s"  Extracted parameters:\n")
            unify.variableParameters.foreach { param =>
              sb.append(s"    - ${param.id}: ${param.inferredType} (${param.distinctValueCount} distinct values)\n")
            }
        }

        if suggestion.decision.warnings.nonEmpty then
          sb.append(s"  Warnings: ${suggestion.decision.warnings.mkString(", ")}\n")
      }

      // Show subsumed patterns summary
      result.hierarchyResult.foreach { hierarchy =>
        val subsumed = hierarchy.allSuggestions.filter(_.subsumedBy.isDefined)
        if subsumed.nonEmpty then
          sb.append("\n")
          sb.append(s"Note: ${subsumed.size} nested patterns were detected but are contained within\n")
          sb.append(s"      the optimal suggestions above. No separate modeling needed.\n")
      }

    sb.append("\n")
    sb.append("=" * 60).append("\n")
    sb.toString()

  private def formatJsonResult(
      result: PatternExtractionResult,
      fileCount: Int,
      totalOriginalNodes: Option[Int] = None
  ): String =
    import scala.collection.mutable.StringBuilder
    val json = new StringBuilder
    json.append("{\n")
    json.append(s"""  "filesAnalyzed": $fileCount,\n""")
    totalOriginalNodes.foreach { total =>
      json.append(s"""  "totalOriginalNodes": $total,\n""")
    }
    json.append(s"""  "totalSubtrees": ${result.detectionResult.totalSubtrees},\n""")
    json.append(s"""  "uniquePatterns": ${result.detectionResult.uniqueHashes},\n""")
    json.append(s"""  "duplicateGroups": ${result.detectionResult.groups.size},\n""")

    // Hierarchy info
    result.hierarchyResult match
      case Some(hierarchy) =>
        json.append(s"""  "allActionableSuggestions": ${hierarchy.allSuggestions.size},\n""")
        json.append(s"""  "optimalSuggestions": ${hierarchy.optimalSuggestions.size},\n""")
        json.append(s"""  "subsumedPatterns": ${result.subsumedCount},\n""")
        json.append(s"""  "optimalReduction": ${result.optimalReduction},\n""")
        totalOriginalNodes.foreach { total =>
          if total > 0 then
            val reductionPct = (result.optimalReduction.toDouble / total) * 100
            json.append(f"""  "optimalReductionRate": $reductionPct%.4f,\n""")
        }
      case None =>
        json.append(s"""  "actionableSuggestions": ${result.suggestions.size},\n""")
        json.append(s"""  "totalPotentialReduction": ${result.totalPotentialReduction},\n""")
        totalOriginalNodes.foreach { total =>
          if total > 0 then
            val reductionPct = (result.totalPotentialReduction.toDouble / total) * 100
            json.append(f"""  "overallReductionRate": $reductionPct%.4f,\n""")
        }

    json.append("""  "suggestions": [""")

    val optimalSuggestions = result.optimalSuggestions
    optimalSuggestions.zipWithIndex.foreach { case (s, idx) =>
      if idx > 0 then json.append(",")
      json.append("\n    {\n")
      json.append(s"""      "rank": ${s.rank},\n""")
      json.append(s"""      "modelName": "${s.suggestedModelName}",\n""")
      json.append(s"""      "score": ${s.decision.score},\n""")
      json.append(s"""      "occurrences": ${s.group.occurrences},\n""")
      json.append(s"""      "parameters": ${s.parameterCount},\n""")
      json.append(s"""      "estimatedReduction": ${s.decision.estimatedReduction},\n""")
      json.append(s"""      "isCrossQuery": ${s.group.isCrossQuery},\n""")
      if s.group.isCrossQuery then
        json.append(s"""      "sources": [${s.group.sources.map(src => s""""$src"""").mkString(", ")}],\n""")
      else if s.group.sources.nonEmpty then
        json.append(s"""      "source": "${s.group.sources.head}",\n""")
      json.append(s"""      "isOptimal": ${s.isOptimal},\n""")
      json.append(s"""      "containsNested": ${s.subsumes.size},\n""")
      json.append(s"""      "reason": "${s.decision.reason.replace("\"", "\\\"")}"\n""")
      json.append("    }")
    }

    json.append("\n  ]\n")
    json.append("}\n")
    json.toString()

  /**
    * Write output to file or stdout
    */
  private def writeOutput(content: String, outputPath: Option[String]): Unit =
    outputPath match
      case Some(path) =>
        import java.nio.file.{Files, Paths}
        import java.nio.charset.StandardCharsets
        val outputPath = Paths.get(path)
        // Create parent directories if they don't exist
        val parentDir = outputPath.getParent
        if parentDir != null && !Files.exists(parentDir) then
          Files.createDirectories(parentDir)
        Files.write(outputPath, content.getBytes(StandardCharsets.UTF_8))
        println(s"Results written to: $path")
      case None =>
        println(content)

end WvletCompiler
