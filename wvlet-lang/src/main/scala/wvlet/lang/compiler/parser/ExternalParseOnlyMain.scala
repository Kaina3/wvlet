package wvlet.lang.compiler.parser

import wvlet.lang.compiler.CompilationUnit

import java.nio.file.{Files, Path, Paths}

/**
  * Parse-only checker for external directories.
  *
  * Usage:
  *   ExternalParseOnlyMain --dir <path> [--limit <n>]
  *   ExternalParseOnlyMain <path> [--limit <n>]
  */
object ExternalParseOnlyMain:

  def main(args: Array[String]): Unit =
    val dir = parseDirArg(args).orElse(sys.env.get("WVLET_EXTERNAL_DIR"))
    val limitOpt = parseLimitArg(args)

    dir match
      case None =>
        throw new IllegalArgumentException(
          "Specify a directory: --dir <path> (or set WVLET_EXTERNAL_DIR)"
        )

      case Some(path) =>
        val resolvedPath = resolveExistingDir(path).getOrElse {
          val cwd    = Paths.get("").toAbsolutePath.normalize
          val parent = Option(cwd.getParent).map(_.normalize).getOrElse(cwd)
          throw new IllegalArgumentException(
            s"Directory not found: ${path}\n" +
              s"Tried: ${cwd.resolve(path).normalize} and ${parent.resolve(path).normalize}\n" +
              s"Tip: Use an absolute path, or from the repo root use ../${path} when running inside wvlet/"
          )
        }

        val allUnits = CompilationUnit.fromPath(resolvedPath.toString)
        val units = limitOpt match
          case Some(n) => allUnits.take(n)
          case None    => allUnits

        if units.isEmpty then
          throw new IllegalArgumentException(
            s"No .wv/.sql files found under: ${resolvedPath.toString}"
          )

        var success = 0
        var failure = 0

        units.foreach { unit =>
          try
            ParserPhase.parseOnly(unit)
            success += 1
          catch
            case e: Throwable =>
              failure += 1
              System.err.println(s"[parse failed] ${unit.relativeFilePath}: ${Option(e.getMessage).getOrElse(e.getClass.getName)}")
        }

        println(s"Parsed: ${success}/${units.size}")
        if failure > 0 then
          throw new RuntimeException(s"Parse failed: ${failure}/${units.size}")

  private def parseDirArg(args: Array[String]): Option[String] =
    if args == null || args.isEmpty then None
    else
      val idx = args.indexOf("--dir")
      if idx >= 0 && idx + 1 < args.length then
        Option(args(idx + 1)).filter(_.nonEmpty)
      else
        // Support positional argument: ExternalParseOnlyMain <path>
        Option(args(0)).filter(a => a.nonEmpty && !a.startsWith("-"))

  private def parseLimitArg(args: Array[String]): Option[Int] =
    if args == null || args.isEmpty then None
    else
      val idx = args.indexOf("--limit")
      if idx >= 0 && idx + 1 < args.length then
        val raw = args(idx + 1)
        val n =
          try raw.toInt
          catch
            case _: NumberFormatException =>
              throw new IllegalArgumentException(s"Invalid --limit value: ${raw}")
        if n <= 0 then
          throw new IllegalArgumentException(s"--limit must be a positive integer, but got: ${n}")
        Some(n)
      else
        None

  private def resolveExistingDir(path: String): Option[Path] =
    val p = Paths.get(path)
    val candidates =
      if p.isAbsolute then
        Seq(p)
      else
        val cwd    = Paths.get("").toAbsolutePath.normalize
        val parent = Option(cwd.getParent).map(_.normalize)
        Seq(Some(cwd.resolve(p).normalize), parent.map(_.resolve(p).normalize)).flatten

    candidates.find { c =>
      Files.exists(c) && Files.isDirectory(c)
    }

end ExternalParseOnlyMain
