package wvlet.lang.compiler.codegen

import wvlet.airspec.AirSpec
import wvlet.lang.compiler.CompilationUnit
import wvlet.lang.compiler.parser.ParserPhase
import wvlet.lang.compiler.transform.JoinFlattener
import wvlet.lang.compiler.transform.RewriteExpr
import wvlet.lang.model.plan.*

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

class TreasureDataJoinFlattenReportSpec extends AirSpec:

  private def maxBraceDepth(s: String): Int =
    var depth    = 0
    var maxDepth = 0
    s.foreach { ch =>
      if ch == '{' then
        depth += 1
        if depth > maxDepth then maxDepth = depth
      else if ch == '}' then
        depth = math.max(0, depth - 1)
    }
    maxDepth

  private def planNodeCount(p: LogicalPlan)(pf: PartialFunction[LogicalPlan, Boolean]): Int =
    var n = 0
    def loop(x: LogicalPlan): Unit =
      if pf.applyOrElse(x, (_: LogicalPlan) => false) then n += 1
      x.children.foreach(loop)
    loop(p)
    n

  private def maxBracedDepthInPlan(p: LogicalPlan): Int =
    def loop(x: LogicalPlan, depth: Int): Int =
      val d = x match
        case _: BracedRelation => depth + 1
        case _                 => depth

      val childMax = x.children.map(c => loop(c, d)).foldLeft(0)(_ max _)
      d max childMax
    loop(p, 0)

  private def leftDeepJoinDepth(r: Relation): Int =
    def stripBraces(x: Relation): Relation =
      x match
        case b: BracedRelation => stripBraces(b.child)
        case other             => other

    def depth(x: Relation): Int =
      stripBraces(x) match
        case j: Join => 1 + depth(j.left)
        case _       => 1

    depth(r)

  private def maxLeftDeepJoinDepthInPlan(p: LogicalPlan): Int =
    var maxD = 0
    def loop(x: LogicalPlan): Unit =
      x match
        case r: Relation =>
          r match
            case j: Join =>
              maxD = maxD max leftDeepJoinDepth(j)
            case _ =>
              ()
        case _ =>
          ()
      x.children.foreach(loop)
    loop(p)
    maxD

  test("Report join nesting reduction for query_43519.sql") {
    val p = Paths.get("..", "data", "treasuredata", "sql", "query_43519.sql").normalize
    if !Files.exists(p) then
      ignore(s"Missing file: ${p.toAbsolutePath}")

    val sql = Files.readString(p)

    val unit       = CompilationUnit.fromSqlString(sql)
    val unresolved = ParserPhase.parseOnly(unit)
    val rewritten  = RewriteExpr.rewriteOnly(unresolved)

    val before = rewritten
    val after  = JoinFlattener.rewriteOnly(rewritten)

    val beforeBraced = planNodeCount(before) { case _: BracedRelation => true }
    val afterBraced  = planNodeCount(after) { case _: BracedRelation => true }

    val beforeBracedDepth = maxBracedDepthInPlan(before)
    val afterBracedDepth  = maxBracedDepthInPlan(after)

    val beforeJoinDepth = maxLeftDeepJoinDepthInPlan(before)
    val afterJoinDepth  = maxLeftDeepJoinDepthInPlan(after)

    val g           = WvletGenerator(CodeFormatterConfig())
    val wvletBefore = g.print(before)
    val wvletAfter  = g.print(after)

    // Write the flattened output for inspection
    val outDir  = Paths.get("..", "Output", "flatten").normalize
    Files.createDirectories(outDir)
    val outFile = outDir.resolve("query_43519.wv")
    Files.writeString(
      outFile,
      wvletAfter,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE
    )

    val beforeOutLines = wvletBefore.split("\\r?\\n").length
    val afterOutLines  = wvletAfter.split("\\r?\\n").length

    val beforeBraceDepth = maxBraceDepth(wvletBefore)
    val afterBraceDepth  = maxBraceDepth(wvletAfter)

    val beforeJoinLines = wvletBefore.split("\\r?\\n").count(_.trim.startsWith("join"))
    val afterJoinLines  = wvletAfter.split("\\r?\\n").count(_.trim.startsWith("join"))

    println("=" * 80)
    println(s"FILE: ${p.toAbsolutePath}")
    println("=" * 80)
    println(s"[Plan] BracedRelation count: ${beforeBraced} -> ${afterBraced}")
    println(s"[Plan] Max BracedRelation depth: ${beforeBracedDepth} -> ${afterBracedDepth}")
    println(s"[Plan] Left-deep join depth: ${beforeJoinDepth} -> ${afterJoinDepth}")
    println(s"[Wvlet] Lines: ${beforeOutLines} -> ${afterOutLines}")
    println(s"[Wvlet] Max { } depth: ${beforeBraceDepth} -> ${afterBraceDepth}")
    println(s"[Wvlet] join-lines: ${beforeJoinLines} -> ${afterJoinLines}")
    println(s"[Output] Wrote flattened wvlet: ${outFile.toAbsolutePath}")

    // Fail-safe: ensure we didn't increase nesting.
    assert(afterBracedDepth <= beforeBracedDepth)
    assert(afterBraceDepth <= beforeBraceDepth)
  }

end TreasureDataJoinFlattenReportSpec
