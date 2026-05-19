package wvlet.lang.cli

import wvlet.airframe.control.Control
import wvlet.airframe.codec.MessageCodec
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
import wvlet.lang.compiler.analyzer.ReadabilityMetrics
import wvlet.lang.compiler.analyzer.refactor.*
import wvlet.lang.compiler.transform.ImplicitJoinRewriter
import wvlet.lang.compiler.transform.JoinFlattener
import wvlet.lang.compiler.transform.RewriteExpr
import wvlet.lang.model.plan.LogicalPlan
import wvlet.lang.model.plan.PackageDef
import wvlet.lang.model.expr.NameExpr
import wvlet.lang.runner.QueryExecutor
import wvlet.lang.runner.connector.DBConnector
import wvlet.lang.runner.connector.DBConnectorProvider
import wvlet.log.LogSupport

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.io.{FileWriter, PrintWriter}
import scala.jdk.CollectionConverters.*

case class WvletCompilerOption(
    @option(prefix = "-w", description = "Working folder")
    workFolder: String = ".",
    @option(prefix = "-f,--file", description = "Read a query from the given file (.wv or .sql)")
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
    @option(
      prefix = "-o,--output",
      description = "Output file path (stdout if not specified). If --apply is set, writes refactored code; otherwise writes the analysis report"
    )
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
    pattern: String = "*.wv",
    @option(prefix = "--apply", description = "Apply top suggestions and output refactored Wvlet code")
    apply: Boolean = false,
    @option(prefix = "--apply-top", description = "Number of top suggestions to apply (0 = all, default: 0)")
    applyTop: Int = 0,
    @option(prefix = "--model-prefix", description = "Prefix for generated model names (default: auto)")
    modelPrefix: String = "auto",
    @option(prefix = "--output-dir", description = "Output directory for refactored files (multi-file mode)")
    outputDir: Option[String] = None,
    @option(prefix = "--model-file", description = "File name for extracted model definitions (default: _models.wv)")
    modelFile: String = "_models.wv",
    @option(prefix = "--suffix", description = "Suffix for refactored file names (default: _refactored)")
    suffix: String = "_refactored",
    @option(prefix = "--overwrite", description = "Overwrite original files instead of creating new files with suffix")
    overwrite: Boolean = false,
    @option(prefix = "--selection", description = "Selection strategy for non-overlapping refactorings (greedy|simple-optimize)")
    selection: String = "greedy",
    @option(
      prefix = "--weights",
      description = "Score weights as 'default', 'equal', or comma-separated 4 values e.g. '0.4,0.3,0.2,0.1'"
    )
    weights: String = "default",
    @option(
      prefix = "--extract-only",
      description = "Output all actionable patterns (before selection) with structuralHash and occurrencePaths for external ILP solving"
    )
    extractOnly: Boolean = false,
    @option(
      prefix = "--stream-extract",
      description = "Stream-parse all files one-by-one (no LogicalPlan retained), accumulate global pattern metadata, and output JSON for external ILP solving. More memory-efficient and globally correct than --extract-only."
    )
    streamExtract: Boolean = false,
    @option(
      prefix = "--stream-apply",
      description = "Two-pass streaming apply: Pass-A re-parses only relevant files to collect exact LogicalPlan subtrees for AntiUnifier (exact paramCount), Pass-B applies selected patterns per file. Requires --extract-json and --ilp-json."
    )
    streamApply: Boolean = false,
    @option(
      prefix = "--extract-json",
      description = "Path to stream_global.json produced by --stream-extract. Required for --stream-apply."
    )
    extractJson: Option[String] = None,
    @option(
      prefix = "--ilp-json",
      description = "Path to ILP result JSON produced by solve_global_ilp.py. Required for --stream-apply."
    )
    ilpJson: Option[String] = None,
    @option(
      prefix = "--stream-update-params",
      description = "Re-parse sampled occurrences for every pattern in stream_global.json, run AntiUnifier to obtain real parameterCount, and rewrite the JSON. Run this AFTER --stream-extract and BEFORE solve_global_ilp.py. Requires --extract-json; writes to -o (default: in-place)."
    )
    streamUpdateParams: Boolean = false
)

/**
  * Options for converting a directory of SQL files into Wvlet files
  */
case class SqlToWvletDirOption(
    @option(prefix = "--sql-dir", description = "Directory of .sql files to convert")
    sqlDir: String,
    @option(prefix = "--output-dir", description = "Output directory for generated .wv files")
    outputDir: String,
    @option(prefix = "--sql-pattern", description = "File pattern to match (default: *.sql)")
    pattern: String = "*.sql",
    @option(prefix = "--offset", description = "Skip first N files (for batch processing)")
    offset: Int = 0,
    @option(prefix = "--limit", description = "Limit number of files to convert")
    limit: Option[Int] = None,
    @option(prefix = "--suffix", description = "Suffix for output file names (default: empty)")
    suffix: String = "",
    @option(prefix = "--overwrite", description = "Overwrite existing output files")
    overwrite: Boolean = false,
    @option(prefix = "--parse-only", description = "Parse-only mode for faster conversion")
    parseOnly: Boolean = true,
    @option(prefix = "--continue-on-error", description = "Continue converting other files when an error occurs")
    continueOnError: Boolean = true
)

/**
  * Options for exporting readability metrics (SN/PR/JI) for a directory of .sql/.wv files
  */
case class ReadabilityMetricsDirOption(
    @option(prefix = "--dir", description = "Directory of query files (.sql or .wv)")
    dir: String,
    @option(prefix = "--pattern", description = "File pattern to match (default: *.sql)")
    pattern: String = "*.sql",
    @option(prefix = "--offset", description = "Skip first N files (for batch processing)")
    offset: Int = 0,
    @option(prefix = "--limit", description = "Limit number of files to process")
    limit: Option[Int] = None,
    @option(prefix = "--out", description = "Output jsonl file path")
    out: String,
    @option(prefix = "--parse-only", description = "Parse-only mode for faster bulk processing")
    parseOnly: Boolean = true,
    @option(prefix = "--continue-on-error", description = "Continue processing other files when an error occurs")
    continueOnError: Boolean = true,
    @option(prefix = "--language", description = "Optional language label to emit in output")
    language: Option[String] = None
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
    createCompiler(parseOnly = parseOnly, sourceFolders = List(compilerOption.workFolder))

  private def createCompiler(parseOnly: Boolean, sourceFolders: List[String]): Compiler =
    val dbType = compilerOption.targetDBType.map(DBType.fromString).getOrElse(currentProfile.dbType)

    val options = CompilerOptions(
      sourceFolders = sourceFolders,
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

  private def compileInternal(
      inputUnit: CompilationUnit,
      parseOnly: Boolean = false,
      sourceFolders: List[String] = List(compilerOption.workFolder)
  ): Context =
    val compileResult =
      if parseOnly then
        val parsingCompiler = createCompiler(parseOnly = true, sourceFolders = sourceFolders)
        parsingCompiler.compileSingleUnit(inputUnit)
      else
        val compiler = createCompiler(parseOnly = false, sourceFolders = sourceFolders)
        compiler.compileSingleUnit(inputUnit)

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
    // For SQL to Wvlet conversion, parsing is usually sufficient.
    // We apply lightweight plan rewrites (e.g., join flattening) explicitly below.
    val ctx = compileInternal(inputUnit, parseOnly = inputUnit.sourceFile.isSQL)

    // Get the resolved logical plan from the compilation unit and apply minimal rewrites.
    // NOTE: Compiler.parseOnly does not run transform phases, so we run these explicitly.
    val logicalPlan =
      val unresolved = inputUnit.resolvedPlan
      val rewritten  = RewriteExpr.rewriteOnly(unresolved)
      val flattened  = JoinFlattener.rewriteOnly(rewritten)
      ImplicitJoinRewriter.rewriteOnly(flattened)

    // Create a WvletGenerator with the appropriate database type configuration
    val config    = CodeFormatterConfig(sqlDBType = ctx.dbType)
    val generator = WvletGenerator(config)(using ctx)

    // Convert the logical plan to Wvlet flow-style syntax
    generator.print(logicalPlan)

  def showLogicalPlan: String =
    val inputUnit = getInputUnit(forSQL = false)
    val ctx       = compileInternal(inputUnit, parseOnly = inputUnit.sourceFile.isSQL)
    
    // Get the resolved logical plan from the compilation unit
    val logicalPlan = inputUnit.resolvedPlan
    
    // Use LogicalPlanPrinter to generate a string representation of the logical plan
    import wvlet.lang.model.plan.LogicalPlanPrinter
    LogicalPlanPrinter.print(logicalPlan)(using ctx)

  def showReadabilityMetrics: ReadabilityMetrics.Metrics =
    val inputUnit = getInputUnit(forSQL = false)
    val ctx       = compileInternal(inputUnit, parseOnly = inputUnit.sourceFile.isSQL)

    val plan =
      val unresolved = inputUnit.resolvedPlan
      val rewritten  = RewriteExpr.rewriteOnly(unresolved)
      val flattened  = JoinFlattener.rewriteOnly(rewritten)
      ImplicitJoinRewriter.rewriteOnly(flattened)

    ReadabilityMetrics.compute(plan)

  def showDRYDebug(topK: Int = 10): ReadabilityMetrics.DryDebug =
    val inputUnit = getInputUnit(forSQL = false)
    val ctx       = compileInternal(inputUnit, parseOnly = inputUnit.sourceFile.isSQL)

    val plan =
      val unresolved = inputUnit.resolvedPlan
      val rewritten  = RewriteExpr.rewriteOnly(unresolved)
      val flattened  = JoinFlattener.rewriteOnly(rewritten)
      ImplicitJoinRewriter.rewriteOnly(flattened)

    ReadabilityMetrics.computeDRYDebug(plan, topK = topK)

  private def generateWvletFromSqlFilePath(sqlFilePath: String, parseOnly: Boolean): String =
    val inputUnit = CompilationUnit.fromFile(sqlFilePath)
    val parentDir = Option(Paths.get(sqlFilePath).getParent).map(_.toString).getOrElse(compilerOption.workFolder)

    // For SQL to Wvlet conversion, parsing is usually sufficient.
    // Some readability transforms are handled in codegen (e.g., join flattening) even in parse-only mode.
    val ctx = compileInternal(inputUnit, parseOnly = parseOnly, sourceFolders = List(parentDir))

    val logicalPlan =
      val unresolved = inputUnit.resolvedPlan
      val rewritten  = RewriteExpr.rewriteOnly(unresolved)
      val flattened  = JoinFlattener.rewriteOnly(rewritten)
      ImplicitJoinRewriter.rewriteOnly(flattened)
    val config      = CodeFormatterConfig(sqlDBType = ctx.dbType)
    val generator   = WvletGenerator(config)(using ctx)
    generator.print(logicalPlan)

  def convertSqlDirectoryToWvlet(opt: SqlToWvletDirOption): Unit =
    val inDir  = Paths.get(opt.sqlDir)
    val outDir = Paths.get(opt.outputDir)

    if !Files.exists(inDir) || !Files.isDirectory(inDir) then
      throw StatusCode.INVALID_ARGUMENT.newException(s"Invalid --sql-dir: ${opt.sqlDir}")

    Files.createDirectories(outDir)

    val matcher = inDir.getFileSystem.getPathMatcher(s"glob:${opt.pattern}")

    val allFiles =
      Control.withResource(Files.walk(inDir)) { stream =>
        stream
          .iterator()
          .asScala
          .filter(p => Files.isRegularFile(p))
          .filter(p => matcher.matches(p.getFileName))
          .toVector
          .sortBy(_.toString)
      }

    val sliced =
      val dropped = if opt.offset > 0 then allFiles.drop(opt.offset) else allFiles
      opt.limit match
        case Some(n) => dropped.take(n)
        case None    => dropped

    var converted = 0
    var skipped   = 0
    var failed    = 0

    info(s"Converting ${sliced.size} SQL files from ${inDir} -> ${outDir} (parseOnly=${opt.parseOnly})")

    sliced.foreach { sqlPath =>
      val baseName = sqlPath.getFileName.toString
      val stem     = if baseName.toLowerCase.endsWith(".sql") then baseName.dropRight(4) else baseName
      val outName  = s"${stem}${opt.suffix}.wv"
      val outPath  = outDir.resolve(outName)

      if Files.exists(outPath) && !opt.overwrite then
        skipped += 1
      else
        try
          val wvlet = generateWvletFromSqlFilePath(sqlPath.toString, parseOnly = opt.parseOnly)
          Files.writeString(outPath, wvlet + "\n", StandardCharsets.UTF_8)
          converted += 1
        catch
          case e: Throwable =>
            failed += 1
            warn(s"Failed to convert ${sqlPath}: ${e.getMessage}")
            if !opt.continueOnError then
              throw e
    }

    info(s"Done. converted=${converted}, skipped=${skipped}, failed=${failed}")

  case class ReadabilityMetricRecord(
      query_id: String,
      DRY: Double,
      SN: Double,
      PR: Double,
      JI: Double,
      parse_ok: Boolean,
      error: Option[String] = None,
      language: Option[String] = None
  )

  // ── Private case classes for --stream-apply JSON parsing ─────────────────
  private case class SaIlpEntry(structuralHash: Int = 0)
  private case class SaIlpJson(selected: List[SaIlpEntry] = Nil)
  private case class SaOccPath(sourceId: Option[String] = None, path: List[Int] = Nil)
  private case class SaExtPat(
      structuralHash: Int = 0,
      nodeType:       String = "",
      nodeCount:      Int = 0,
      occurrences:    Int = 0,
      occurrencePaths: List[SaOccPath] = Nil
  )
  private case class SaExtJson(filesAnalyzed: Int = 0, patterns: List[SaExtPat] = Nil)

  private def extractQueryIdFromFileName(fileName: String): String =
    val stem =
      if fileName.toLowerCase.endsWith(".sql") then fileName.dropRight(4)
      else if fileName.toLowerCase.endsWith(".wv") then fileName.dropRight(3)
      else fileName
    stem.stripPrefix("query_").stripSuffix("_refactored")

  def exportReadabilityMetrics(opt: ReadabilityMetricsDirOption): Unit =
    val inDir = Paths.get(opt.dir)
    if !Files.exists(inDir) || !Files.isDirectory(inDir) then
      throw StatusCode.INVALID_ARGUMENT.newException(s"Invalid --dir: ${opt.dir}")

    val matcher = inDir.getFileSystem.getPathMatcher(s"glob:${opt.pattern}")

    val allFiles =
      Control.withResource(Files.walk(inDir)) { stream =>
        stream
          .iterator()
          .asScala
          .filter(p => Files.isRegularFile(p))
          .filter(p => matcher.matches(p.getFileName))
          .toVector
          .sortBy(_.toString)
      }

    val sliced =
      val dropped = if opt.offset > 0 then allFiles.drop(opt.offset) else allFiles
      opt.limit match
        case Some(n) => dropped.take(n)
        case None    => dropped

    val outPath = Paths.get(opt.out)
    Option(outPath.getParent).foreach(p => Files.createDirectories(p))

    info(s"Exporting readability metrics for ${sliced.size} files from ${inDir} -> ${outPath} (parseOnly=${opt.parseOnly})")

    val codec = MessageCodec.of[ReadabilityMetricRecord]

    // IMPORTANT: Don't set sourceFolders to inDir. If inDir contains non-query artifacts
    // (e.g., files starting with "SyntaxError%"), the compiler may pick them up and fail
    // even when compiling a specific file.
    // Use the workFolder-based setup (same as single-file commands) and compile each unit by path.
    val compiler = createCompiler(parseOnly = opt.parseOnly, sourceFolders = List(compilerOption.workFolder))

    var processed = 0
    var failed = 0
    val total = sliced.size
    val progressInterval = Math.max(1, total / 100) // Log every 1%

    Control.withResource(new PrintWriter(new FileWriter(outPath.toString))) { writer =>
      sliced.foreach { filePath =>
        processed += 1
        val fileName = filePath.getFileName.toString
        val queryId = extractQueryIdFromFileName(fileName)

        // Progress logging
        if processed % progressInterval == 0 || processed == total then
          val pct = (processed * 100.0 / total).toInt
          info(s"Progress: ${processed}/${total} (${pct}%) failed=${failed}")

        try
          val unit = CompilationUnit.fromFile(filePath.toString)
          val result = compiler.compileSingleUnit(unit)

          if result.hasFailures then
            failed += 1
            val msg = result.failureReport.map(_._2.getMessage).mkString("; ")
            // Ensure jsonl is truly one-record-per-line (avoid raw newlines/tabs in error strings)
            val msg1 = msg.replaceAll("\\s+", " ").trim
            val rec = ReadabilityMetricRecord(
              query_id = queryId,
              DRY = 0.0,
              SN = 0.0,
              PR = 0.5,
              JI = 1.0,
              parse_ok = false,
              error = Some(msg1.take(500)),
              language = opt.language
            )
            writer.println(codec.toJson(rec))
            if !opt.continueOnError then
              throw StatusCode.INTERNAL_ERROR.newException(msg)
          else
            val unresolved = unit.resolvedPlan
            val rewritten  = RewriteExpr.rewriteOnly(unresolved)
            val flattened  = JoinFlattener.rewriteOnly(rewritten)
            val plan       = ImplicitJoinRewriter.rewriteOnly(flattened)
            val m          = ReadabilityMetrics.compute(plan)
            val rec = ReadabilityMetricRecord(
              query_id = queryId,
              DRY = m.DRY,
              SN = m.SN,
              PR = m.PR,
              JI = m.JI,
              parse_ok = true,
              error = None,
              language = opt.language
            )
            writer.println(codec.toJson(rec))
        catch
          case e: Throwable =>
            failed += 1
            val msg1 = Option(e.getMessage).getOrElse(e.getClass.getSimpleName).replaceAll("\\s+", " ").trim
            val rec = ReadabilityMetricRecord(
              query_id = queryId,
              DRY = 0.0,
              SN = 0.0,
              PR = 0.5,
              JI = 1.0,
              parse_ok = false,
              error = Some(msg1.take(500)),
              language = opt.language
            )
            writer.println(codec.toJson(rec))
            if !opt.continueOnError then
              throw e
      }
    }

    info(s"Done. processed=${processed}, failed=${failed}")

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
        import java.io.File
        val f = new File(dirPath)
        if f.exists && f.isFile then
          // Accept a file path passed to --dir for convenience
          analyzePatternsFromFilePath(dirPath, patternOption, extractorConfig)
        else if patternOption.streamUpdateParams then
          streamUpdateParamsGlobal(dirPath, patternOption, extractorConfig)
        else if patternOption.streamApply then
          streamApplyGlobal(dirPath, patternOption, extractorConfig)
        else if patternOption.streamExtract then
          streamExtractGlobal(dirPath, patternOption, extractorConfig)
        else
          analyzePatternsFromDirectory(dirPath, patternOption, extractorConfig)
      case None =>
        analyzePatternsFromSingleFile(patternOption, extractorConfig)

  /**
    * Analyze patterns from an explicit file path (used when --dir is given a file)
    */
  private def analyzePatternsFromFilePath(
      filePath: String,
      patternOption: PatternAnalysisOption,
      extractorConfig: PatternExtractorConfig
  ): Unit =
    import java.nio.file.Paths

    val p            = Paths.get(filePath)
    val resolvedPath = if p.isAbsolute then filePath else s"${compilerOption.workFolder}/${filePath}".stripPrefix("./")
    val inputUnit   = CompilationUnit.fromFile(resolvedPath)
    // In single-file analysis, do not scan source folders at all.
    // Otherwise, other .wv files in the same directory can break compilation.
    val ctx         = compileInternal(inputUnit, parseOnly = patternOption.parseOnly, sourceFolders = Nil)
    val logicalPlan = inputUnit.resolvedPlan

    val result = PatternExtractor.analyze(logicalPlan, extractorConfig)

    val effectiveOption: PatternAnalysisOption =
      // In file-path mode, support --output-dir as a convenience. (Historically, --output-dir
      // was documented as multi-file mode, but it's useful here too.)
      if patternOption.apply && patternOption.output.isEmpty && patternOption.outputDir.isDefined then
        val inputName = Paths.get(resolvedPath).getFileName.toString
        val baseName  = inputName.stripSuffix(".wv")
        val outName =
          if patternOption.overwrite then s"${baseName}.wv"
          else s"${baseName}${patternOption.suffix}.wv"
        val outPath = Paths.get(patternOption.outputDir.get).resolve(outName).toString
        patternOption.copy(output = Some(outPath))
      else
        patternOption

    if effectiveOption.apply then
      applyAndOutputRefactoring(logicalPlan, result, effectiveOption, ctx)
    else
      val output = if patternOption.json then
        formatJsonResult(result, 1)
      else
        formatTextResult(result, patternOption.top, 1)

      writeOutput(output, patternOption.output)

  /**
    * Analyze patterns from a single file
    */
  private def analyzePatternsFromSingleFile(
      patternOption: PatternAnalysisOption,
      extractorConfig: PatternExtractorConfig
  ): Unit =
    val inputUnit   = getInputUnit(forSQL = false)
    // In single-file analysis, compile only this unit (no folder scanning)
    val ctx         = compileInternal(inputUnit, parseOnly = patternOption.parseOnly, sourceFolders = Nil)
    val logicalPlan = inputUnit.resolvedPlan

    val result = PatternExtractor.analyze(logicalPlan, extractorConfig)

    // Check if we should apply refactorings
    if patternOption.apply then
      applyAndOutputRefactoring(logicalPlan, result, patternOption, ctx)
    else
      val output = if patternOption.json then
        formatJsonResult(result, 1)
      else
        formatTextResult(result, patternOption.top, 1)

      writeOutput(output, patternOption.output)

  /**
    * Apply refactoring suggestions and output the transformed Wvlet code
    */
  private def applyAndOutputRefactoring(
      originalPlan: LogicalPlan,
      extraction: PatternExtractionResult,
      patternOption: PatternAnalysisOption,
      ctx: Context
  ): Unit =
    import wvlet.lang.compiler.codegen.{WvletGenerator, CodeFormatterConfig}

    // Configure the applier
    // Use per-file selection mode to include subsumed patterns that can be applied
    // without conflicting with higher-scoring patterns
    val applyConfig = ApplyConfig(
      topK = patternOption.applyTop,
      useOptimalSuggestions = true,
      usePerFileSelection = true,  // Enable per-file greedy selection with all suggestions
      modelNamePrefix = patternOption.modelPrefix,
      skipIfOverlaps = true,
      requireVariableParams = false,  // Allow patterns without variable params
      selectionMode = RefactoringSelectionMode.fromString(patternOption.selection)
    )

    // Apply refactorings
    val applyResult = RefactoringApplier.applyRefactorings(originalPlan, extraction, applyConfig)

    if !applyResult.hasChanges then
      println("// No refactorings applied (no applicable suggestions found)")
      println()
      // Still output original code
      val config    = CodeFormatterConfig(sqlDBType = ctx.dbType)
      val generator = WvletGenerator(config)(using ctx)
      val output    = generator.print(originalPlan)
      writeOutput(output, patternOption.output)
      return

    // Generate Wvlet code from the transformed plan
    val config    = CodeFormatterConfig(sqlDBType = ctx.dbType)
    val generator = WvletGenerator(config)(using ctx)
    val output    = generator.print(applyResult.updatedPlan)

    // Add header comment with refactoring summary
    val header = new StringBuilder
    header.append(s"// Auto-refactored by Wvlet Pattern Analyzer\n")
    header.append(s"// Applied ${applyResult.applied.size} refactoring(s)\n")
    header.append(s"// Total occurrences replaced: ${applyResult.totalOccurrencesReplaced}\n")
    header.append(s"// Estimated node reduction: ${applyResult.totalNodeReduction}\n")
    header.append("//\n")
    applyResult.applied.foreach { applied =>
      header.append(s"// - ${applied.suggestionId}: ${applied.occurrencesReplaced} occurrences\n")
    }
    if applyResult.skipped.nonEmpty then
      header.append("// Skipped:\n")
      applyResult.skipped.foreach { skipped =>
        header.append(s"//   - ${skipped.suggestionId}: ${skipped.reason}\n")
      }
    header.append("\n")

    val fullOutput = header.toString + output
    writeOutput(fullOutput, patternOption.output)

  /**
    * Apply refactorings to multiple files and output to separate files.
    *
    * - Each input file is written as {basename}{suffix}.wv to outputDir
    * - Shared model definitions are written to {modelFile} in outputDir
    * - Analysis summary is written to _analysis_summary.txt in outputDir
    */
  private def applyAndOutputMultipleRefactorings(
      inputDir: String,
      namedPlans: List[(String, LogicalPlan)],
      extraction: PatternExtractionResult,
      patternOption: PatternAnalysisOption,
      fileCount: Int,
      totalOriginalNodes: Int
  ): Unit =
    import java.io.{File, PrintWriter}
    import java.nio.file.{Files, Paths}
    import wvlet.lang.compiler.codegen.{WvletGenerator, CodeFormatterConfig}
    import wvlet.lang.compiler.Context

    // Configure the applier
    // Use per-file selection mode to include subsumed patterns that can be applied
    // without conflicting with higher-scoring patterns
    val applyConfig = ApplyConfig(
      topK = patternOption.applyTop,
      useOptimalSuggestions = true,
      usePerFileSelection = RefactoringSelectionMode.fromString(patternOption.selection) == RefactoringSelectionMode.Greedy,
      modelNamePrefix = patternOption.modelPrefix,
      skipIfOverlaps = true,
      requireVariableParams = false,  // Allow patterns without variable params
      selectionMode = RefactoringSelectionMode.fromString(patternOption.selection)
    )

    // Apply refactorings across all files
    val multiResult = RefactoringApplier.applyRefactoringsMultiple(namedPlans, extraction, applyConfig)

    if !multiResult.hasChanges then
      println("// No refactorings applied (no applicable suggestions found)")
      return

    // Determine output directory
    val outputDir = patternOption.outputDir.getOrElse(inputDir)
    val outputPath = Paths.get(outputDir)
    if !Files.exists(outputPath) then
      Files.createDirectories(outputPath)

    // Create a minimal context for code generation
    given Context = Context.NoContext

    val config    = CodeFormatterConfig()
    val generator = WvletGenerator(config)

    // Write model file
    if multiResult.models.nonEmpty then
      val modelFilePath = outputPath.resolve(patternOption.modelFile)
      val modelHeader = new StringBuilder
      modelHeader.append(s"// Auto-generated model definitions by Wvlet Pattern Analyzer\n")
      modelHeader.append(s"// Generated: ${java.time.LocalDateTime.now()}\n")
      modelHeader.append(s"// Models: ${multiResult.models.size}\n")
      modelHeader.append(s"// Total occurrences replaced: ${multiResult.totalOccurrencesReplaced}\n")
      modelHeader.append("\n")

      val modelPackage = PackageDef(
        name = NameExpr.EmptyName,
        statements = multiResult.models,
        span = wvlet.lang.api.Span.NoSpan
      )
      val modelCode = generator.print(modelPackage)
      
      val pw = new PrintWriter(modelFilePath.toFile)
      try
        pw.print(modelHeader.toString + modelCode)
      finally
        pw.close()
      
      println(s"Wrote model definitions to: ${modelFilePath}")

    // Write each transformed file
    var filesWritten = 0
    multiResult.fileResults.foreach { case (originalName, plan) =>
      val baseName = originalName.stripSuffix(".wv")
      val outputFileName = 
        if patternOption.overwrite then s"${baseName}.wv"
        else s"${baseName}${patternOption.suffix}.wv"
      val outputFilePath = outputPath.resolve(outputFileName)
      
      val code = generator.print(plan)
      
      val pw = new PrintWriter(outputFilePath.toFile)
      try
        pw.print(code)
        filesWritten += 1
      finally
        pw.close()
    }

    println(s"Wrote ${filesWritten} refactored files to: ${outputDir}")

    // Write analysis summary
    val summaryFilePath = outputPath.resolve("_analysis_summary.txt")
    val summaryContent = formatTextResult(extraction, patternOption.top, fileCount, Some(totalOriginalNodes))
    val summaryPw = new PrintWriter(summaryFilePath.toFile)
    try
      summaryPw.print(summaryContent)
    finally
      summaryPw.close()
    println(s"Wrote analysis summary to: ${summaryFilePath}")

    println()
    println(multiResult.summary)

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
    val matcher = FileSystems.getDefault.getPathMatcher(s"glob:${patternOption.pattern}")
    val allFiles =
      dir
        .listFiles()
        .filter(f => f.isFile && matcher.matches(Paths.get(f.getName)))
        .toList
        .sortBy(_.getName)

    // When applying refactorings in directory mode, avoid re-processing generated artifacts.
    // Otherwise, running the command repeatedly (or pointing --dir to an output directory)
    // will generate files like *_refactored_refactored.
    val inputFiles =
      if patternOption.apply then
        allFiles.filterNot { f =>
          val name = f.getName
          val base = name.stripSuffix(".wv")
          name == patternOption.modelFile || name.startsWith("_") || base.endsWith(patternOption.suffix)
        }
      else
        allFiles
    val filesAfterOffset =
      if patternOption.offset <= 0 then inputFiles
      else inputFiles.drop(patternOption.offset)
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
    val compiler          = if patternOption.parseOnly then null else createCompiler(parseOnly = false, sourceFolders = List(dirPath))
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

    // Check if we should apply refactorings
    if patternOption.apply then
      applyAndOutputMultipleRefactorings(dirPath, plans.toList, result, patternOption, plans.size, totalOriginalNodes)
    else if patternOption.extractOnly then
      val output = formatExtractOnlyJsonResult(result, plans.size, patternOption.offset, patternOption.limit)
      writeOutput(output, patternOption.output)
    else
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
    val scoreWeights = ScoreWeights.fromString(patternOption.weights)
    val refactorConfig =
      if patternOption.aggressive then
        RefactorConfig.aggressive.copy(scoreWeights = scoreWeights)
      else if patternOption.conservative then
        RefactorConfig.conservative.copy(scoreWeights = scoreWeights)
      else
        RefactorConfig(
          minOccurrences = patternOption.minOccurrences,
          minPatternSize = patternOption.minNodes,
          maxParameters = patternOption.maxParams,
          scoreWeights = scoreWeights
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
      // When applying refactorings, don't limit suggestions; otherwise use --top
      maxSuggestions = if patternOption.apply then Int.MaxValue else patternOption.top,
      crossQueryAnalysis = patternOption.dir.isDefined,
      selectionMode = RefactoringSelectionMode.fromString(patternOption.selection)
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


  /**
    * Update parameterCount in stream_global.json with real AntiUnifier values.
    *
    * For each pattern, samples up to 50 occurrences, re-parses those files, navigates
    * to the subtrees via SubtreeCollector.navigatePath, runs AntiUnifier, and rewrites
    * parameterCount, estimatedReduction, and score in the output JSON.
    *
    * Run this AFTER --stream-extract and BEFORE solve_global_ilp.py so the ILP uses
    * correct parameterCount in its w_par score term and can pre-filter patterns that
    * exceed maxParameters.
    *
    * Requires:
    *   --extract-json  input  stream_global.json (from --stream-extract)
    *   -o / --output   output path (default: in-place overwrite of --extract-json)
    */
  private def streamUpdateParamsGlobal(
      dirPath:         String,
      patternOption:   PatternAnalysisOption,
      extractorConfig: PatternExtractorConfig
  ): Unit =
    import java.io.File
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Files, Paths, FileSystems}
    import scala.collection.mutable

    val extractJsonPath = patternOption.extractJson.getOrElse(
      throw StatusCode.INVALID_ARGUMENT.newException("--extract-json is required for --stream-update-params")
    )
    val outputJsonPath = patternOption.output.orElse(Some(extractJsonPath))

    val extractContent = new String(Files.readAllBytes(Paths.get(extractJsonPath)), StandardCharsets.UTF_8)
    val extractData    = MessageCodec.of[SaExtJson].fromJson(extractContent)
    println(s"[stream-update-params] Loaded ${extractData.patterns.size} patterns from $extractJsonPath")

    // ── Build file lookup ──────────────────────────────────────────────────
    val dir         = new File(dirPath)
    val globMatcher = FileSystems.getDefault.getPathMatcher(s"glob:${patternOption.pattern}")
    val allFilesMap: Map[String, File] = Option(dir.listFiles())
      .getOrElse(Array.empty[File])
      .collect { case f if f.isFile && globMatcher.matches(Paths.get(f.getName)) => f.getName -> f }
      .toMap

    // ── Sample up to N occurrences per pattern, group by file ──────────────
    val maxSample = 50
    case class UpNeed(hash: Int, path: List[Int], nodeCount: Int)
    val fileToNeeds    = mutable.HashMap.empty[String, mutable.ListBuffer[UpNeed]]
    val patternSamples = mutable.HashMap.empty[Int, mutable.ListBuffer[CollectedSubtree]]

    extractData.patterns.foreach { pat =>
      patternSamples(pat.structuralHash) = mutable.ListBuffer.empty
      pat.occurrencePaths.take(maxSample).foreach { occ =>
        occ.sourceId.foreach { fileName =>
          fileToNeeds
            .getOrElseUpdate(fileName, mutable.ListBuffer.empty)
            .+=(UpNeed(pat.structuralHash, occ.path, pat.nodeCount))
        }
      }
    }
    println(s"[stream-update-params] ${fileToNeeds.size} files to parse for param sampling")

    // ── Parse each file once, collect subtrees ─────────────────────────────
    var upParseOK = 0; var upParseFail = 0
    fileToNeeds.foreach { case (fileName, needs) =>
      allFilesMap.get(fileName) match
        case None => upParseFail += 1
        case Some(file) =>
          try
            val unit = CompilationUnit.fromFile(file.getAbsolutePath)
            val plan = wvlet.lang.compiler.parser.ParserPhase.parseOnly(unit)
            needs.foreach { need =>
              val buf = patternSamples(need.hash)
              if buf.size < maxSample then
                SubtreeCollector.navigatePath(plan, need.path).foreach { node =>
                  buf += CollectedSubtree(
                    root           = node,
                    depth          = 0,
                    nodeCount      = need.nodeCount,
                    structuralHash = need.hash,
                    path           = need.path,
                    sourceId       = Some(fileName)
                  )
                }
            }
            upParseOK += 1
          catch
            case _: Exception => upParseFail += 1
    }
    println(s"[stream-update-params] Parse: ok=$upParseOK, fail=$upParseFail")

    // ── Run AntiUnifier per pattern to get real paramCount ─────────────────
    AntiUnifier.resetCounter()

    // Helper: run AntiUnifier; returns (paramCount, Option[AntiUnifyResult]).
    def computeWithResult(
        subtrees:  List[CollectedSubtree],
        hash:      Int,
        nodeCount: Double
    ): (Int, Option[AntiUnifyResult]) =
      if subtrees.size >= 2 then
        val group  = DuplicateGroup(
          structuralHash = hash,
          subtrees       = subtrees,
          nodeCount      = nodeCount,
          depth          = 0.0
        )
        val result = AntiUnifier.unify(group)
        (result.map(_.variableParameters.size).getOrElse(0), result)
      else (0, None)

    // Helper: run AntiUnifier on a subset; returns paramCount only.
    def computeParamCount(subtrees: List[CollectedSubtree], hash: Int, nodeCount: Double): Int =
      computeWithResult(subtrees, hash, nodeCount)._1

    def absoluteGain(nodeCount: Int, occ: Int, params: Int): Double =
      nodeCount.toDouble * occ - nodeCount - occ * (1.0 + params) - 2.0 * params - 3.0

    def jsonEscape(s: String): String =
      s.flatMap {
        case '"'  => "\\\""
        case '\\' => "\\\\"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case c if c.isControl => f"\\u${c.toInt}%04x"
        case c => c.toString
      }

    def jsonString(s: String): String = s"\"${jsonEscape(s)}\""

    def occurrencePathJson(o: SaOccPath): String =
      val pathArr = o.path.mkString("[", ",", "]")
      val srcStr  = o.sourceId.map(jsonString).getOrElse("null")
      s"""{"sourceId":$srcStr,"path":$pathArr}"""

    def subtreeToOccPath(st: CollectedSubtree): SaOccPath =
      SaOccPath(st.sourceId, st.path)

    def valueKey(value: Any): String =
      Option(value).map(_.toString).getOrElse("null")

    // Given a full AntiUnifyResult, derive the indices of the "core cluster":
    // occurrences where EVERY variable parameter matches its most-common value.
    // This removes outlier occurrences that inflate paramCount (e.g. one odd
    // WHERE clause value that turns a constant into a parameter).
    //
    // Fallback cascade:
    //   1. All-match cluster  (strictest)
    //   2. Strict-majority cluster  (>50% of variable params match most-common)
    //   3. Full set  (no useful clustering found)
    def coreClusterIndices(result: AntiUnifyResult, n: Int): List[Int] =
      val varParams = result.variableParameters
      if varParams.isEmpty then return (0 until n).toList  // already fully uniform

      // Most-common value for each variable parameter
      val mostCommon: List[Any] = varParams.map { param =>
        param.values.groupBy(identity).maxBy(_._2.size)._1
      }

      // Strict: ALL variable params at most-common value
      val strictIndices = (0 until n).filter { i =>
        varParams.zip(mostCommon).forall { case (param, mv) =>
          i < param.values.size && param.values(i) == mv
        }
      }.toList
      if strictIndices.size >= 2 then return strictIndices

      // Relaxed: strict majority of variable params at most-common value
      val majorityIndices = (0 until n).filter { i =>
        val matches = varParams.zip(mostCommon).count { case (param, mv) =>
          i < param.values.size && param.values(i) == mv
        }
        matches * 2 > varParams.size
      }.toList
      if majorityIndices.size >= 2 then majorityIndices
      else (0 until n).toList  // can't cluster meaningfully

    case class SpgCluster(
        id:              String,
        key:             String,
        size:            Int,
        paramCount:      Int,
        gain:            Double,
        occIndices:      List[Int],
        occurrencePaths: List[SaOccPath]
    )

    def buildSpgClusters(
        result:    AntiUnifyResult,
        subtrees:  List[CollectedSubtree],
        hash:      Int,
        nodeCount: Int,
        fullParams: Int
    ): List[SpgCluster] =
      val n = subtrees.size
      if n < 2 then return Nil

      val varParams = result.variableParameters
      if varParams.isEmpty then return Nil

      val maxLowVariance = math.max(2, math.sqrt(n.toDouble).toInt)
      val lowSlots = varParams.zipWithIndex.filter { case (param, _) =>
        val distinct = param.values.take(n).distinct.size
        distinct > 1 && distinct <= maxLowVariance
      }
      if lowSlots.isEmpty then return Nil

      val grouped = (0 until n).groupBy { i =>
        lowSlots.map { case (param, _) =>
          if i < param.values.size then param.values(i) else null
        }.toList
      }

      grouped.toList.flatMap { case (keyValues, indices) =>
        val idxList = indices.toList.sorted
        if idxList.size < 2 then
          None
        else
          val clusterSubtrees = idxList.map(subtrees(_))
          val pc              = computeParamCount(clusterSubtrees, hash, nodeCount.toDouble)
          val gain            = absoluteGain(nodeCount, idxList.size, pc)
          if pc < fullParams && gain > 0.0 then
            val key = keyValues.map(valueKey).mkString("|")
            Some(
              SpgCluster(
                id              = s"spg_${math.abs(key.hashCode)}",
                key             = key,
                size            = idxList.size,
                paramCount      = pc,
                gain            = gain,
                occIndices      = idxList,
                occurrencePaths = clusterSubtrees.map(subtreeToOccPath)
              )
            )
          else
            None
      }
        .sortBy(c => (-c.gain, -c.size, c.key))
        .take(12)

    // UpResult carries:
    //   realParamCount   — LGG over all N sampled occurrences (full)
    //   paramCountCluster — LGG over the most-homogeneous core cluster
    //   clusterSize       — number of occurrences in that core cluster
    case class UpResult(
        pat:               SaExtPat,
        realParamCount:    Int,
        paramCountCluster: Int,   // real LGG on mode-cluster (data-driven subset)
        clusterSize:       Int,   // size of the core cluster
      clusterIndices:    List[Int],
      spgClusters:       List[SpgCluster],
        newReduction:      Int
    )

    val updatedList = extractData.patterns.map { pat =>
      val subtrees = patternSamples.getOrElse(pat.structuralHash, mutable.ListBuffer.empty).toList
      val n        = subtrees.size

      val (fullPC, fullResultOpt) =
        computeWithResult(subtrees, pat.structuralHash, pat.nodeCount.toDouble)

      // Build core cluster from full LGG result
      val (pcCluster, clusterSz, clusterIdx, spgClusters) = fullResultOpt match
        case None =>
          (fullPC, n, (0 until n).toList, Nil)  // no LGG result → cluster = full set
        case Some(fullResult) =>
          val coreIdx = coreClusterIndices(fullResult, n)
          val spg     = buildSpgClusters(fullResult, subtrees, pat.structuralHash, pat.nodeCount, fullPC)
          if coreIdx.size == n then
            (fullPC, n, coreIdx, spg)  // no outliers found → cluster = full set
          else
            val clusterSubtrees = coreIdx.map(subtrees(_))
            (computeParamCount(clusterSubtrees, pat.structuralHash, pat.nodeCount.toDouble), coreIdx.size, coreIdx, spg)

      val nc           = pat.nodeCount
      val occ          = pat.occurrences
      val newReduction = nc * occ - nc - occ * (1 + fullPC)
      UpResult(pat, fullPC, pcCluster, clusterSz, clusterIdx, spgClusters, newReduction)
    }

    // ── Recompute scores with global normalization ─────────────────────────
    val weights   = extractorConfig.refactorConfig.scoreWeights
    val maxParams = extractorConfig.refactorConfig.maxParameters.max(1)
    val upMaxRed  = updatedList.map(_.newReduction).max.max(1)
    val upMaxOcc  = updatedList.map(_.pat.occurrences).max.max(1)

    // ── Serialize to JSON ──────────────────────────────────────────────────
    val upJson = new StringBuilder
    upJson.append("{\n")
    upJson.append(s"""  "filesAnalyzed": ${extractData.filesAnalyzed},\n""")
    upJson.append(s"""  "patterns": [""")

    updatedList.zipWithIndex.foreach { case (up, idx) =>
      if idx > 0 then upJson.append(",")
      val pat       = up.pat
      val pc        = up.realParamCount
      val red       = up.newReduction
      val occ       = pat.occurrences
      val nc        = pat.nodeCount
      val origNodes = nc * occ
      val redRatio  = if origNodes > 0 then red.toDouble / origNodes else 0.0
      val score = {
        val s1 = (red.toDouble / upMaxRed).min(1.0) * weights.reduction
        val s2 = math.max(0.0, redRatio) * weights.reductionRatio
        val s3 = (occ.toDouble / upMaxOcc).min(1.0) * weights.occurrences
        val s4 = (1.0 - pc.toDouble / maxParams) * weights.params
        math.min(s1 + s2 + s3 + s4, 1.0)
      }
      val isCrossQuery = pat.occurrencePaths.flatMap(_.sourceId).toSet.size > 1
      val pathsJson = pat.occurrencePaths.map(occurrencePathJson).mkString("[", ",", "]")
      val sampledPaths = patternSamples
        .getOrElse(pat.structuralHash, mutable.ListBuffer.empty)
        .toList
        .map(subtreeToOccPath)
      val clusterPathsJson = up.clusterIndices
        .flatMap(sampledPaths.lift)
        .map(occurrencePathJson)
        .mkString("[", ",", "]")
      val clusterIndicesJson = up.clusterIndices.mkString("[", ",", "]")
      val spgClustersJson = up.spgClusters.zipWithIndex.map { case (cluster, clusterIdx) =>
        val occIdxJson = cluster.occIndices.mkString("[", ",", "]")
        val occJson    = cluster.occurrencePaths.map(occurrencePathJson).mkString("[", ",", "]")
        s"""{"id":"${cluster.id}","label":"C_${clusterIdx + 1}","key":${jsonString(cluster.key)},"size":${cluster.size},"paramCount":${cluster.paramCount},"gain":${cluster.gain},"occIndices":$occIdxJson,"occurrencePaths":$occJson}"""
      }.mkString("[", ",", "]")

      upJson.append("\n    {\n")
      upJson.append(s"""      "structuralHash": ${pat.structuralHash},\n""")
      upJson.append(s"""      "nodeType": "${pat.nodeType}",\n""")
      upJson.append(s"""      "nodeCount": ${pat.nodeCount},\n""")
      upJson.append(s"""      "occurrences": $occ,\n""")
      upJson.append(s"""      "parameterCount": $pc,\n""")
      upJson.append(s"""      "paramCount_cluster": ${up.paramCountCluster},\n""")
      upJson.append(s"""      "cluster_size": ${up.clusterSize},\n""")
      upJson.append(s"""      "cluster_indices": $clusterIndicesJson,\n""")
      upJson.append(s"""      "clusterOccurrencePaths": $clusterPathsJson,\n""")
      upJson.append(s"""      "spgClusters": $spgClustersJson,\n""")
      upJson.append(s"""      "estimatedReduction": $red,\n""")
      upJson.append(s"""      "score": $score,\n""")
      upJson.append(s"""      "isCrossQuery": $isCrossQuery,\n""")
      upJson.append(s"""      "occurrencePaths": $pathsJson\n""")
      upJson.append("    }")
    }
    upJson.append("\n  ]\n")
    upJson.append("}\n")

    writeOutput(upJson.toString(), outputJsonPath)

    val pcDist = updatedList.groupBy(_.realParamCount).view.mapValues(_.size).toList.sortBy(_._1)
    println(s"[stream-update-params] Done. paramCount distribution:")
    pcDist.foreach { case (k, v) => println(s"  paramCount=$k: $v patterns") }
    val overMax = updatedList.count(_.realParamCount > maxParams)
    if overMax > 0 then
      println(s"  [WARNING] $overMax patterns exceed maxParameters=$maxParams and will be rejected by --stream-apply (filter with --max-params in ILP).")

  /**
    * Two-pass streaming apply: 
    *
    * Pass A — Re-parse only the files referenced in occurrencePaths of ILP-selected patterns.
    *           Navigate to each occurrence's path to extract the actual LogicalPlan subtree.
    *           Feed these into AntiUnifier to compute exact parameterCount (no more param=0
    *           approximation) and obtain per-occurrence Substitutions.
    *
    * Pass B — Re-parse each relevant file again, construct per-file PatternExtractionResult
    *           using the pre-computed RefactoringSuggestions (with exact substitutions), and
    *           call RefactoringApplier.applyRefactorings → WvletGenerator to write output.
    *
    * Requires:
    *   --extract-json  path to stream_global.json  (from --stream-extract)
    *   --ilp-json      path to ILP result JSON      (from solve_global_ilp.py)
    *   --output-dir    directory to write refactored .wv files
    */
  private def streamApplyGlobal(
      dirPath:         String,
      patternOption:   PatternAnalysisOption,
      extractorConfig: PatternExtractorConfig
  ): Unit =
    import java.io.{File, PrintWriter}
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Files, Paths, FileSystems}
    import scala.collection.mutable

    val extractJsonPath = patternOption.extractJson.getOrElse(
      throw StatusCode.INVALID_ARGUMENT.newException("--extract-json is required for --stream-apply")
    )
    val ilpJsonPath = patternOption.ilpJson.getOrElse(
      throw StatusCode.INVALID_ARGUMENT.newException("--ilp-json is required for --stream-apply")
    )
    val outputDirStr = patternOption.outputDir.getOrElse(
      throw StatusCode.INVALID_ARGUMENT.newException("--output-dir is required for --stream-apply")
    )

    // ── Step 1: Load selected hashes from ILP result ────────────────────────
    val ilpContent = new String(Files.readAllBytes(Paths.get(ilpJsonPath)), StandardCharsets.UTF_8)
    val ilpResult  = MessageCodec.of[SaIlpJson].fromJson(ilpContent)
    val selectedHashes: Set[Int] = ilpResult.selected.map(_.structuralHash).toSet
    println(s"[stream-apply] ${selectedHashes.size} patterns selected by ILP")

    // ── Step 2: Load occurrencePaths from extract JSON ──────────────────────
    val extractContent = new String(Files.readAllBytes(Paths.get(extractJsonPath)), StandardCharsets.UTF_8)
    val extractData    = MessageCodec.of[SaExtJson].fromJson(extractContent)
    val selectedPats   = extractData.patterns.filter(p => selectedHashes.contains(p.structuralHash))
    println(s"[stream-apply] ${selectedPats.size} patterns found in extract JSON")

    // ── Step 3: Build per-file occurrence index ─────────────────────────────
    // occInfo holds the global index (position in occurrencePaths) so that the
    // corresponding AntiUnifier substitution can be retrieved by index later.
    case class OccInfo(hash: Int, path: List[Int], globalIdx: Int, nodeCount: Int)
    val fileToOccInfos = mutable.HashMap.empty[String, mutable.ListBuffer[OccInfo]]

    selectedPats.foreach { pat =>
      pat.occurrencePaths.zipWithIndex.foreach { case (occ, gIdx) =>
        occ.sourceId.foreach { fileName =>
          fileToOccInfos
            .getOrElseUpdate(fileName, mutable.ListBuffer.empty)
            .+=(OccInfo(pat.structuralHash, occ.path, gIdx, pat.nodeCount))
        }
      }
    }
    val relevantFileNames = fileToOccInfos.keySet
    println(s"[stream-apply] ${relevantFileNames.size} files referenced in occurrencePaths")

    // ── Build file lookup map ───────────────────────────────────────────────
    val dir         = new File(dirPath)
    val globMatcher = FileSystems.getDefault.getPathMatcher(s"glob:${patternOption.pattern}")
    val allFilesMap: Map[String, File] = Option(dir.listFiles())
      .getOrElse(Array.empty[File])
      .collect { case f if f.isFile && globMatcher.matches(Paths.get(f.getName)) => f.getName -> f }
      .toMap

    // ── Sub-pass A: Collect CollectedSubtrees at known paths ────────────────
    // Map: hash → Array[Option[CollectedSubtree]]  (indexed by globalIdx)
    val hashToSubArr = mutable.HashMap.empty[Int, Array[Option[CollectedSubtree]]]
    selectedPats.foreach { pat =>
      hashToSubArr(pat.structuralHash) = Array.fill(pat.occurrencePaths.size)(None)
    }

    var collectOK = 0; var collectFail = 0; var pathFail = 0
    val startA = System.currentTimeMillis()

    relevantFileNames.toList.sorted.zipWithIndex.foreach { case (fileName, idx) =>
      if (idx + 1) % 10000 == 0 then
        print(s"\r  [Sub-pass A] ${idx + 1}/${relevantFileNames.size} ...")
        System.out.flush()
      allFilesMap.get(fileName) match
        case None => collectFail += 1
        case Some(file) =>
          try
            val unit = CompilationUnit.fromFile(file.getAbsolutePath)
            val plan = wvlet.lang.compiler.parser.ParserPhase.parseOnly(unit)
            fileToOccInfos(fileName).foreach { oi =>
              SubtreeCollector.navigatePath(plan, oi.path) match
                case None =>
                  pathFail += 1
                case Some(subtreeNode) =>
                  val subtree = CollectedSubtree(
                    root           = subtreeNode,
                    depth          = 0,   // not used by applier
                    nodeCount      = oi.nodeCount,
                    structuralHash = oi.hash,
                    path           = oi.path,
                    sourceId       = Some(fileName)
                  )
                  hashToSubArr(oi.hash)(oi.globalIdx) = Some(subtree)
            }
            collectOK += 1
          catch
            case _: Exception => collectFail += 1
    }
    val timeA = System.currentTimeMillis() - startA
    println(
      s"\r  [Sub-pass A] Done in ${timeA}ms — ok=$collectOK, fail=$collectFail, pathFail=$pathFail"
    )

    // ── Step 4: Build DuplicateGroups and run AntiUnifier ───────────────────
    // Reset counter so parameter IDs are reproducible ($P1, $P2, ...).
    AntiUnifier.resetCounter()

    val suggestions = mutable.ListBuffer.empty[RefactoringSuggestion]
    selectedPats.foreach { pat =>
      val subtrees = hashToSubArr
        .getOrElse(pat.structuralHash, Array.empty[Option[CollectedSubtree]])
        .toList
        .flatten   // drop occurrences where Sub-pass A failed to navigate

      if subtrees.nonEmpty then
        val group = DuplicateGroup(
          structuralHash = pat.structuralHash,
          subtrees       = subtrees,
          nodeCount      = pat.nodeCount.toDouble,
          depth          = 0.0
        )
        val (decision, unifyResult) = RefactoringDecider.evaluate(group, extractorConfig.refactorConfig)
        suggestions += RefactoringSuggestion(
          group       = group,
          unifyResult = unifyResult,
          decision    = decision,
          rank        = suggestions.size + 1
        )
    }

    val applicableSuggs = suggestions.filter(_.decision.shouldRefactor).toList
    println(
      s"[stream-apply] ${suggestions.size} groups built; ${applicableSuggs.size} pass shouldRefactor"
    )
    // Log exact paramCount distribution
    val paramDist = applicableSuggs
      .groupBy(_.unifyResult.map(_.variableParameters.size).getOrElse(0))
      .view
      .mapValues(_.size)
      .toList
      .sortBy(_._1)
    println(s"  paramCount distribution: ${paramDist.map { case (k, v) => s"$k→$v" }.mkString(", ")}")

    // ── Pre-compute per-file suggestion views ────────────────────────────────
    // For each suggestion, create a per-file variant:
    //   group.subtrees      = only this file's CollectedSubtrees (in original order)
    //   unifyResult.substitutions = only the substitutions for this file's occurrences
    //                               (indexed 0..K-1 matching group.subtrees order)
    //
    // Why this works correctly in applyRefactorings:
    //   nonOverlapping.zipWithIndex → idx 0,1,2,...
    //   unifyResult.substitutions.lift(idx) → perFileSubs(idx) = correct substitution
    val fileToPerFileSuggs =
      mutable.HashMap.empty[String, mutable.ListBuffer[RefactoringSuggestion]]

    applicableSuggs.foreach { sugg =>
      val globalSubtrees = sugg.group.subtrees
      globalSubtrees
        .zipWithIndex
        .groupBy(_._1.sourceId)
        .foreach {
          case (Some(fileName), subtreesWithGlobalIdx) =>
            val fileSubtrees     = subtreesWithGlobalIdx.map(_._1)
            val globalIdxInList  = subtreesWithGlobalIdx.map(_._2)
            val perFileUR = sugg.unifyResult.map { ur =>
              val perFileSubs = globalIdxInList.flatMap(ur.substitutions.lift)
              ur.copy(substitutions = perFileSubs)
            }
            val perFileSugg = sugg.copy(
              group       = sugg.group.copy(subtrees = fileSubtrees),
              unifyResult = perFileUR
            )
            fileToPerFileSuggs
              .getOrElseUpdate(fileName, mutable.ListBuffer.empty)
              .+=(perFileSugg)
          case _ => ()
        }
    }
    println(s"[stream-apply] ${fileToPerFileSuggs.size} files will receive refactorings")

    // ── Sub-pass B: Re-parse each file, apply, and write ────────────────────
    val outputPath = Paths.get(outputDirStr)
    Files.createDirectories(outputPath)

    val applyConfig = ApplyConfig(
      topK                 = 0,
      useOptimalSuggestions = false,  // we manage the suggestion list directly
      requireVariableParams = false,   // apply paramCount=0 patterns too
      skipIfOverlaps       = true,
      modelNamePrefix      = patternOption.modelPrefix,
      selectionMode        = RefactoringSelectionMode.Greedy
    )

    // WvletGenerator works fine with Context.NoContext in parse-only refactoring.
    given ctx: Context = Context.NoContext
    val codeConfig = CodeFormatterConfig()
    val generator  = WvletGenerator(codeConfig)

    var applyOK = 0; var applyNoChange = 0; var applyFail = 0
    val startB = System.currentTimeMillis()

    fileToPerFileSuggs.keys.toList.sorted.zipWithIndex.foreach { case (fileName, idx) =>
      if (idx + 1) % 10000 == 0 then
        print(s"\r  [Sub-pass B] ${idx + 1}/${fileToPerFileSuggs.size} ...")
        System.out.flush()
      allFilesMap.get(fileName) match
        case None => applyFail += 1
        case Some(file) =>
          try
            val unit = CompilationUnit.fromFile(file.getAbsolutePath)
            val plan = wvlet.lang.compiler.parser.ParserPhase.parseOnly(unit)

            val fileSuggs = fileToPerFileSuggs(fileName).toList
            val detection = DuplicateDetectionResult(
              groups        = fileSuggs.map(_.group),
              totalSubtrees = fileSuggs.map(_.group.subtrees.size).sum,
              uniqueHashes  = fileSuggs.size
            )
            val extraction = PatternExtractionResult(
              suggestions      = fileSuggs,
              detectionResult  = detection,
              report           = "",
              hierarchyResult  = None
            )

            val applyResult = RefactoringApplier.applyRefactorings(plan, extraction, applyConfig)
            if applyResult.hasChanges then
              val code    = generator.print(applyResult.updatedPlan)
              val outFile = outputPath.resolve(fileName)
              Files.writeString(outFile, code, StandardCharsets.UTF_8)
              applyOK += 1
            else
              applyNoChange += 1
          catch
            case _: Exception => applyFail += 1
    }
    val timeB = System.currentTimeMillis() - startB
    println(
      s"\r  [Sub-pass B] Done in ${timeB}ms — written=$applyOK, noChange=$applyNoChange, fail=$applyFail"
    )
    println(s"[stream-apply] Refactored files written to $outputDirStr")

  /**
    * Memory-efficient global extraction: stream-parse every file one-by-one, accumulate
    * SubtreeMetadata without retaining any LogicalPlan, then apply a global minOccurrences
    * filter and output the same JSON schema as formatExtractOnlyJsonResult.
    *
    * Unlike --extract-only (which applies minOccurrences per-batch), this produces the
    * correct global candidate set for the external ILP solver.
    */
  private def streamExtractGlobal(
      dirPath:         String,
      patternOption:   PatternAnalysisOption,
      extractorConfig: PatternExtractorConfig
  ): Unit =
    import java.io.File
    import java.nio.file.{Paths, FileSystems}
    import scala.collection.mutable

    val dir = new File(dirPath)
    if !dir.exists() || !dir.isDirectory then
      throw StatusCode.INVALID_ARGUMENT.newException(s"Directory not found: $dirPath")

    val matcher = FileSystems.getDefault.getPathMatcher(s"glob:${patternOption.pattern}")
    val allFiles =
      dir
        .listFiles()
        .filter(f => f.isFile && matcher.matches(Paths.get(f.getName)))
        .toList
        .sortBy(_.getName)

    val filesAfterOffset =
      if patternOption.offset <= 0 then allFiles
      else allFiles.drop(patternOption.offset)
    val filesToProcess = patternOption.limit match
      case Some(n) => filesAfterOffset.take(n)
      case None    => filesAfterOffset

    if filesToProcess.isEmpty then
      println(s"No files matching '${patternOption.pattern}' found in $dirPath")
      return

    println(s"[stream-extract] Streaming ${filesToProcess.size} files from $dirPath ...")

    // Global accumulator: structuralHash -> list of SubtreeMetadata occurrences
    val acc          = mutable.HashMap.empty[Int, mutable.ListBuffer[SubtreeMetadata]]
    var successCount = 0
    var failCount    = 0
    val startTime    = System.currentTimeMillis()

    filesToProcess.zipWithIndex.foreach { case (file, idx) =>
      if (idx + 1) % 1000 == 0 then
        print(s"\r  Parsed ${idx + 1}/${filesToProcess.size} files, unique hashes: ${acc.size} ...")
        System.out.flush()
      try
        val unit = CompilationUnit.fromFile(file.getAbsolutePath)
        val plan = wvlet.lang.compiler.parser.ParserPhase.parseOnly(unit)
        unit.unresolvedPlan = plan
        unit.resolvedPlan   = plan
        SubtreeCollector.collectMetadataInto(plan, extractorConfig.collectorConfig, Some(file.getName), acc)
        successCount += 1
        // plan is now out of scope with respect to `acc` — eligible for GC
      catch
        case _: Exception => failCount += 1
    }

    val parseTime = System.currentTimeMillis() - startTime
    println(s"\r  Parse pass completed in ${parseTime}ms")
    println(s"  Parsed: $successCount, Failed: $failCount")
    println(s"  Unique structural hashes collected: ${acc.size}")

    // --- Global filtering ---
    val minOcc     = extractorConfig.detectorConfig.minOccurrences
    val minNodes   = extractorConfig.detectorConfig.minNodeCount
    val minPatSize = extractorConfig.refactorConfig.minPatternSize

    val filtered = acc.filter { case (_, list) =>
      val nc = list.headOption.map(_.nodeCount).getOrElse(0)
      list.size >= minOcc && nc >= minNodes && nc >= minPatSize
    }
    println(s"  After global minOccurrences($minOcc)/minNodes filter: ${filtered.size} patterns")

    // Remove descendant overlaps within each source
    val deoverlapped = mutable.HashMap.empty[Int, List[SubtreeMetadata]]
    filtered.foreach { case (hash, list) =>
      val pruned = SubtreeCollector.removeMetadataOverlapping(list.toList)
      if pruned.size >= minOcc then
        deoverlapped(hash) = pruned
    }
    println(s"  After overlap removal: ${deoverlapped.size} patterns")

    if deoverlapped.isEmpty then
      println("No qualifying patterns found.")
      writeOutput(
        """{"filesAnalyzed":0,"batchOffset":0,"allActionableSuggestions":0,"patterns":[]}"""
          + "\n",
        patternOption.output
      )
      return

    // --- Score computation (mirrors RefactoringDecider.calculateScore, paramCount=0) ---
    // paramCount=0 is an approximation (weight 0.1); exact value requires anti-unification
    // which is deferred to the apply phase for selected patterns only.
    val maxOcc = deoverlapped.values.map(_.size).max.max(1)
    val maxRed = deoverlapped.values.map { list =>
      val nc  = list.head.nodeCount
      val occ = list.size
      nc * occ - nc - occ // estimatedReduction with paramCount=0
    }.max.max(1)

    val weights   = extractorConfig.refactorConfig.scoreWeights
    val maxParams = extractorConfig.refactorConfig.maxParameters.max(1)

    // Sort descending by estimated reduction
    val patterns = deoverlapped.toList.sortBy { case (_, list) =>
      val nc  = list.head.nodeCount
      val occ = list.size
      -(nc * occ - nc - occ)
    }

    // --- JSON output (same schema as formatExtractOnlyJsonResult) ---
    val json = new StringBuilder
    json.append("{\n")
    json.append(s"""  "filesAnalyzed": $successCount,\n""")
    json.append(s"""  "batchOffset": ${patternOption.offset},\n""")
    patternOption.limit.foreach { n =>
      json.append(s"""  "batchLimit": $n,\n""")
    }
    json.append(s"""  "allActionableSuggestions": ${patterns.size},\n""")
    json.append(s"""  "patterns": [""")

    patterns.zipWithIndex.foreach { case ((hash, occurrences), idx) =>
      if idx > 0 then json.append(",")
      val sample    = occurrences.head
      val nc        = sample.nodeCount
      val occ       = occurrences.size
      val paramCount     = 0
      val originalNodes  = nc * occ
      val modelNodes     = nc
      val callNodes      = occ * (1 + paramCount)
      val reduction      = originalNodes - modelNodes - callNodes
      val reductionRatio = if originalNodes > 0 then reduction.toDouble / originalNodes else 0.0
      val score = {
        val s1 = (reduction.toDouble / maxRed).min(1.0) * weights.reduction
        val s2 = math.max(0.0, reductionRatio) * weights.reductionRatio
        val s3 = (occ.toDouble / maxOcc).min(1.0) * weights.occurrences
        val s4 = (1.0 - paramCount.toDouble / maxParams) * weights.params
        math.min(s1 + s2 + s3 + s4, 1.0)
      }
      val isCrossQuery = occurrences.map(_.sourceId).distinct.size > 1
      json.append("\n    {\n")
      json.append(s"""      "structuralHash": $hash,\n""")
      json.append(s"""      "nodeType": "${sample.nodeType}",\n""")
      json.append(s"""      "nodeCount": $nc,\n""")
      json.append(s"""      "occurrences": $occ,\n""")
      json.append(s"""      "parameterCount": $paramCount,\n""")
      json.append(s"""      "estimatedReduction": $reduction,\n""")
      json.append(s"""      "score": $score,\n""")
      json.append(s"""      "isCrossQuery": $isCrossQuery,\n""")
      val pathsJson = occurrences.map { st =>
        val pathArr = st.path.mkString("[", ",", "]")
        val srcStr  = st.sourceId.map(id => s""""${id.replace("\"", "\\\"")}"""").getOrElse("null")
        s"""{"sourceId":$srcStr,"path":$pathArr}"""
      }.mkString("[", ",", "]")
      json.append(s"""      "occurrencePaths": $pathsJson\n""")
      json.append("    }")
    }

    json.append("\n  ]\n")
    json.append("}\n")

    writeOutput(json.toString(), patternOption.output)

  /**
    * Format all actionable patterns (before selection) with structuralHash and occurrencePaths.
    * Used with --extract-only for external ILP solving across batches.
    */
  private def formatExtractOnlyJsonResult(
      result: PatternExtractionResult,
      fileCount: Int,
      batchOffset: Int,
      batchLimit: Option[Int]
  ): String =
    import scala.collection.mutable.StringBuilder
    val json = new StringBuilder

    // Use allSuggestions (pre-selection) from hierarchyResult if available
    val allSuggestions = result.hierarchyResult
      .map(_.allSuggestions)
      .getOrElse(result.suggestions)

    json.append("{\n")
    json.append(s"""  "filesAnalyzed": $fileCount,\n""")
    json.append(s"""  "batchOffset": $batchOffset,\n""")
    batchLimit.foreach { n =>
      json.append(s"""  "batchLimit": $n,\n""")
    }
    json.append(s"""  "allActionableSuggestions": ${allSuggestions.size},\n""")
    json.append(s"""  "patterns": [""")

    allSuggestions.zipWithIndex.foreach { case (s, idx) =>
      if idx > 0 then json.append(",")
      json.append("\n    {\n")
      json.append(s"""      "structuralHash": ${s.group.structuralHash},\n""")
      json.append(s"""      "nodeType": "${s.group.sample.root.getClass.getSimpleName.toLowerCase}",\n""")
      json.append(s"""      "nodeCount": ${s.group.nodeCount},\n""")
      json.append(s"""      "occurrences": ${s.group.occurrences},\n""")
      json.append(s"""      "parameterCount": ${s.parameterCount},\n""")
      json.append(s"""      "estimatedReduction": ${s.decision.estimatedReduction},\n""")
      json.append(s"""      "score": ${s.decision.score},\n""")
      json.append(s"""      "isCrossQuery": ${s.group.isCrossQuery},\n""")
      // Occurrence paths for conflict graph reconstruction in Python
      val pathsJson = s.group.subtrees.map { st =>
        val pathArr = st.path.mkString("[", ",", "]")
        val srcStr  = st.sourceId.map(id => s""""${id.replace("\"", "\\\"")}"""").getOrElse("null")
        s"""{"sourceId":$srcStr,"path":$pathArr}"""
      }.mkString("[", ",", "]")
      json.append(s"""      "occurrencePaths": $pathsJson\n""")
      json.append("    }")
    }

    json.append("\n  ]\n")
    json.append("}\n")
    json.toString()

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
