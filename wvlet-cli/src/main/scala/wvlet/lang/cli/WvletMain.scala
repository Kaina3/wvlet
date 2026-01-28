package wvlet.lang.cli

import wvlet.airframe.Design
import wvlet.airframe.launcher.Launcher
import wvlet.airframe.launcher.command
import wvlet.lang.BuildInfo
import wvlet.lang.api.WvletLangException
import wvlet.lang.compiler.WorkEnv
import wvlet.lang.runner.connector.DBConnectorProvider
import wvlet.lang.server.WvletServer
import wvlet.lang.server.WvletServerConfig
import wvlet.log.LogSupport

object WvletMain:
  private def launcher: Launcher = Launcher.of[WvletMain]

  private def wrap(body: => Unit): Unit =
    def findCause(e: Throwable): Throwable =
      e match
        case e: IllegalArgumentException if e.getCause != null =>
          findCause(e.getCause)
        case other =>
          e

    try
      body
    catch
      case e: Throwable =>
        findCause(e) match
          case e: WvletLangException if e.statusCode.isSuccess =>
          // do nothing
          case other =>
            throw other

  def main(args: Array[String]): Unit = wrap(launcher.execute(args))
  def main(argLine: String): Unit     = wrap(launcher.execute(argLine))

  def isInSbt: Boolean = sys.props.getOrElse("wvlet.sbt.testing", "false").toBoolean

/**
  * 'wvlet' command line interface
  * @param opts
  */
class WvletMain(opts: WvletGlobalOption) extends LogSupport:

  @command(description = "show version", isDefault = true)
  def version: Unit = info(s"wvlet version ${BuildInfo.version}")

  @command(description = "Start a local WebUI server")
  def ui(serverConfig: WvletServerConfig): Unit = WvletServer.startServer(
    serverConfig,
    openBrowser = true
  )

  private def handleError[U](body: => U): U =
    try
      body
    catch
      case e: WvletLangException =>
        error(e.getMessage)
        throw e

  private def design(compilerOptions: WvletCompilerOption): Design =
    val workEnv = WorkEnv(compilerOptions.workFolder, opts.logLevel)
    Design
      .newSilentDesign
      .bindInstance(WvletCompiler(opts, compilerOptions, workEnv, DBConnectorProvider(workEnv)))

  private def withCompiler[A](compilerOption: WvletCompilerOption)(f: WvletCompiler => A): A =
    design(compilerOption).run[WvletCompiler, A] { compiler =>
      f(compiler)
    }

  @command(description = "Compile .wv files")
  def compile(compilerOption: WvletCompilerOption): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      val sql = compiler.generateSQL
      println(sql)
    }
  }

  @command(description = "Run a query")
  def run(compilerOption: WvletCompilerOption): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      compiler.run()
    }
  }

  @command(description = "Convert SQL to Wvlet query")
  def to_wvlet(compilerOption: WvletCompilerOption): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      val wvlet = compiler.generateWvlet
      println(wvlet)
    }
  }

  @command(description = "Convert SQL files in a directory to .wv files")
  def to_wvlet_dir(compilerOption: WvletCompilerOption, opt: SqlToWvletDirOption): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      compiler.convertSqlDirectoryToWvlet(opt)
    }
  }

  @command(description = "Export readability metrics (DRY/SN/PR/JI) as jsonl for query files in a directory")
  def export_readability_metrics(
      compilerOption: WvletCompilerOption,
      opt: ReadabilityMetricsDirOption
  ): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      compiler.exportReadabilityMetrics(opt)
    }
  }

  @command(description = "Show readability metrics (DRY/SN/PR/JI) for a query")
  def show_readability_metrics(compilerOption: WvletCompilerOption): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      val m = compiler.showReadabilityMetrics
      // Keep output stable and machine-readable without adding new dependencies.
      println(s"{\"DRY\":${m.DRY},\"SN\":${m.SN},\"PR\":${m.PR},\"JI\":${m.JI}}")
    }
  }

  @command(description = "Show DRY debug details (node counts, repeats) for a query")
  def show_dry_debug(compilerOption: WvletCompilerOption, top_k: Int = 10): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      val d = compiler.showDRYDebug(topK = top_k)
      val top = d.top_repeated.map { case (k, c) => s"{\"sig\":\"${k}\",\"count\":${c}}" }.mkString("[", ",", "]")
      println(
        s"{\"DRY\":${d.DRY},\"total_nodes\":${d.total_nodes},\"duplicate_nodes\":${d.duplicate_nodes},\"distinct_subplans\":${d.distinct_subplans},\"top_repeated\":${top}}"
      )
    }
  }

  @command(description = "Analyze query for refactoring opportunities")
  def analyze_patterns(compilerOption: WvletCompilerOption, patternOption: PatternAnalysisOption): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      compiler.analyzePatterns(patternOption)
    }
  }

  @command(description = "Show logical plan of a query")
  def show_plan(compilerOption: WvletCompilerOption): Unit = handleError {
    withCompiler(compilerOption) { compiler =>
      val plan = compiler.showLogicalPlan
      println(plan)
    }
  }

end WvletMain
