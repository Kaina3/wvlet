package wvlet.lang.native

import wvlet.lang.compiler.codegen.GenSQL
import wvlet.lang.compiler.codegen.WvletGenerator
import wvlet.lang.compiler.codegen.CodeFormatterConfig
import wvlet.lang.compiler.CompilationUnit
import wvlet.lang.compiler.Compiler
import wvlet.lang.compiler.CompilerOptions
import wvlet.lang.compiler.Symbol
import wvlet.lang.compiler.WorkEnv
import wvlet.lang.compiler.transform.JoinFlattener
import wvlet.lang.compiler.transform.RewriteExpr
import wvlet.log.LogLevel
import wvlet.log.LogSupport
import wvlet.log.Logger

object WvcMain extends LogSupport:

  def main(args: Array[String]): Unit =
    // Check if the first argument is "to_wvlet"
    if args.length > 0 && args(0) == "to_wvlet" then
      // Call convertSqlToWvlet for SQL to Wvlet conversion
      val (wvletResult, shouldReturn) = convertSqlToWvlet(args.drop(1))
      if !shouldReturn then
        println(wvletResult)
    else
      // Call compileWvletQuery to process the query and check if -x flag was set
      val (sqlResult, shouldReturn) = compileWvletQuery(args)
      if !shouldReturn then
        // If -x is not passed, print the result to stdout
        println(sqlResult) // Print to stdout as usual

  end main

  def compileWvletQuery(args: Array[String]): (String, Boolean) =
    var inputQuery: Option[String]     = None
    var workFolder                     = "."
    var displayHelp                    = false
    var logLevel: LogLevel             = LogLevel.INFO
    var logLevelPatterns: List[String] = List.empty[String]
    var remainingArgs: List[String]    = Nil
    var parseSuccess: Boolean          = false
    var returnResult                   = false // Flag for -x option

    // Option parsing
    def parseOption(lst: List[String]): Unit =
      lst match
        case h :: tail if h == "-h" || h == "--help" =>
          displayHelp = true
          parseOption(tail)
        case "-w" :: folder :: tail =>
          workFolder = folder
          parseOption(tail)
        case "-q" :: query :: tail =>
          inputQuery = Some(query.toString)
          parseOption(tail)
        case "-l" :: level :: tail =>
          logLevel = LogLevel(level.toString)
          parseOption(tail)
        case "-L" :: pattern :: tail =>
          logLevelPatterns = pattern.toString :: logLevelPatterns
          parseOption(tail)
        case "-x" :: tail =>
          returnResult = true // Set returnResult to true if -x is present
          parseOption(tail)
        case h :: tail if h.startsWith("-") || h.startsWith("--") =>
          warn(s"Unknown option: ${h}")
          parseSuccess = false
        case rest =>
          parseSuccess = true
          remainingArgs = rest

    parseOption(args.toList)

    val result: (String, Boolean) =
      if !parseSuccess then
        System.exit(1)
        ("", false)
      else if displayHelp then
        val helpMessage =
          """wvc (Wvlet Native Compiler)
            |  Compile Wvlet files and generate SQL queries
            |
            |[usage]:
            |  wvc [options] -q '(Wvlet query)'
            |  cat query.wv | wvc [options]
            |
            |[options]
            | -h, --help         Display help message
            | -w <folder>        Working folder
            | -q <query>         Query string
            | -l <level>         Log level (info, debug, trace, warn, error)
            | -L <pattern=level> Set log level for a class pattern
            | -x                 Return the result instead of printing it
            |""".stripMargin
        (helpMessage, returnResult)
      else
        // Set log levels
        Logger("wvlet.lang.compiler").setLogLevel(logLevel)
        Logger("wvlet.lang.runner").setLogLevel(logLevel)
        Logger("wvlet.lang.native").setLogLevel(logLevel)
        logLevelPatterns.foreach { p =>
          p.split("=") match
            case Array(pattern, level) =>
              debug(s"Set the log level for ${pattern} to ${level}")
              Logger.setLogLevel(pattern, LogLevel(level))
            case _ =>
              error(s"Invalid log level pattern: ${p}")
        }

        // Prepare a compiler and input source
        val compiler = Compiler(
          CompilerOptions(workEnv = WorkEnv(path = workFolder), sourceFolders = List(workFolder))
        )

        val query: String =
          inputQuery match
            case Some(q) =>
              q
            case None =>
              import scala.scalanative.meta.LinktimeInfo
              // On Windows, POSIX unistd is not available, so we skip the isatty check
              // and assume stdin is connected if no query is provided
              val connectedToStdin =
                if LinktimeInfo.isWindows then
                  true // On Windows, assume stdin is available when no -q is provided
                else
                  import scala.scalanative.posix.unistd
                  unistd.isatty(unistd.STDIN_FILENO) == 0
              if connectedToStdin then
                // Read from stdin
                Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != null).mkString("\n")
              else
                ""

        if query.trim.isEmpty then
          warn(s"No query is given. Use -q 'query' option or stdin to feed the query")
          ("", returnResult)
        else
          // Compile
          val inputUnit     = CompilationUnit.fromWvletString(query)
          val compileResult = compiler.compileSingleUnit(inputUnit)
          compileResult.reportAllErrors

          val ctx = compileResult
            .context
            .withCompilationUnit(inputUnit)
            .withDebugRun(false)
            .newContext(Symbol.NoSymbol)

          val sql = GenSQL.generateSQL(inputUnit)(using ctx)
          (sql, returnResult) // Return the SQL string and the flag

    result

  end compileWvletQuery

  def convertSqlToWvlet(args: Array[String]): (String, Boolean) =
    var inputQuery: Option[String]     = None
    var workFolder                     = "."
    var displayHelp                    = false
    var logLevel: LogLevel             = LogLevel.INFO
    var logLevelPatterns: List[String] = List.empty[String]
    var remainingArgs: List[String]    = Nil
    var parseSuccess: Boolean          = false
    var returnResult                   = false

    // Option parsing
    def parseOption(lst: List[String]): Unit =
      lst match
        case h :: tail if h == "-h" || h == "--help" =>
          displayHelp = true
          parseOption(tail)
        case "-w" :: folder :: tail =>
          workFolder = folder
          parseOption(tail)
        case "-q" :: query :: tail =>
          inputQuery = Some(query.toString)
          parseOption(tail)
        case "-l" :: level :: tail =>
          logLevel = LogLevel(level.toString)
          parseOption(tail)
        case "-L" :: pattern :: tail =>
          logLevelPatterns = pattern.toString :: logLevelPatterns
          parseOption(tail)
        case "-x" :: tail =>
          returnResult = true
          parseOption(tail)
        case h :: tail if h.startsWith("-") || h.startsWith("--") =>
          warn(s"Unknown option: ${h}")
          parseSuccess = false
        case rest =>
          parseSuccess = true
          remainingArgs = rest

    parseOption(args.toList)

    val result: (String, Boolean) =
      if !parseSuccess then
        System.exit(1)
        ("", false)
      else if displayHelp then
        val helpMessage =
          """wvc to_wvlet (SQL to Wvlet Converter)
            |  Convert SQL queries to Wvlet flow-style syntax
            |
            |[usage]:
            |  wvc to_wvlet [options] -q '(SQL query)'
            |  wvc to_wvlet [options] '(SQL query)'
            |  cat query.sql | wvc to_wvlet [options]
            |
            |[options]
            | -h, --help         Display help message
            | -w <folder>        Working folder
            | -q <query>         SQL query string
            | -l <level>         Log level (info, debug, trace, warn, error)
            | -L <pattern=level> Set log level for a class pattern
            | -x                 Return the result instead of printing it
            |""".stripMargin
        (helpMessage, returnResult)
      else
        // Set log levels
        Logger("wvlet.lang.compiler").setLogLevel(logLevel)
        Logger("wvlet.lang.runner").setLogLevel(logLevel)
        Logger("wvlet.lang.native").setLogLevel(logLevel)
        logLevelPatterns.foreach { p =>
          p.split("=") match
            case Array(pattern, level) =>
              debug(s"Set the log level for ${pattern} to ${level}")
              Logger.setLogLevel(pattern, LogLevel(level))
            case _ =>
              error(s"Invalid log level pattern: ${p}")
        }

        // Get query from -q option, positional argument, or stdin
        val query: String =
          inputQuery match
            case Some(q) =>
              q
            case None =>
              // Check if there's a positional argument (query without -q)
              if remainingArgs.nonEmpty then
                remainingArgs.mkString(" ")
              else
                // Try reading from stdin
                import scala.scalanative.posix.unistd
                val connectedToStdin = unistd.isatty(unistd.STDIN_FILENO) == 0
                if connectedToStdin then
                  Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != null).mkString("\n")
                else
                  ""

        if query.trim.isEmpty then
          warn(s"No SQL query is given. Use -q 'query' option, positional argument, or stdin")
          ("", returnResult)
        else
          try
            // Prepare a compiler with parse-only phases
            val compiler = Compiler(
              CompilerOptions(
                workEnv = WorkEnv(path = workFolder),
                sourceFolders = List(workFolder)
              ),
              phases = Compiler.parseOnlyPhases
            )

            // Parse SQL
            val inputUnit     = CompilationUnit.fromSqlString(query)
            val compileResult = compiler.compileSingleUnit(inputUnit)
            compileResult.reportAllErrors

            val ctx = compileResult
              .context
              .withCompilationUnit(inputUnit)
              .withDebugRun(false)
              .newContext(Symbol.NoSymbol)

            // Get the resolved logical plan and apply join flattening
            val logicalPlan =
              val unresolved = inputUnit.resolvedPlan
              val rewritten  = RewriteExpr.rewriteOnly(unresolved)
              JoinFlattener.rewriteOnly(rewritten)

            // Generate Wvlet code
            val config    = CodeFormatterConfig(sqlDBType = ctx.dbType)
            val generator = WvletGenerator(config)(using ctx)
            val wvlet     = generator.print(logicalPlan)

            (wvlet, returnResult)
          catch
            case e: Exception =>
              error(s"Error converting SQL to Wvlet: ${e.getMessage}")
              ("", returnResult)

    result

  end convertSqlToWvlet

end WvcMain
