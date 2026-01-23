package wvlet.lang.compiler.codegen

import wvlet.airspec.AirSpec
import wvlet.lang.compiler.CompilationUnit
import wvlet.lang.compiler.parser.ParserPhase
import wvlet.lang.compiler.transform.JoinFlattener
import wvlet.lang.compiler.transform.RewriteExpr

class JoinFlattenSpec extends AirSpec:

  test("Flatten parenthesized left-deep join tree") {
    val sql = """
      |SELECT *
      |FROM (((orders o
      |  JOIN customers c ON o.customer_id = c.id)
      |  JOIN products p ON o.product_id = p.id)
      |  JOIN regions r ON o.region_id = r.id)
    """.stripMargin.trim

    val unit          = CompilationUnit.fromSqlString(sql)
    val unresolved    = ParserPhase.parseOnly(unit)
    val rewrittenExpr = RewriteExpr.rewriteOnly(unresolved)
    val flattened     = JoinFlattener.rewriteOnly(rewrittenExpr)

    val g   = WvletGenerator(CodeFormatterConfig())
    val out = g.print(flattened)

    // Expect a flat join sequence (no brace nesting from redundant parentheses)
    // and all joins to be present.
    assert(!out.contains("{"), s"Unexpected braces in output:\n$out")
    assert(out.split("\n").count(_.trim.startsWith("join")) == 3, s"Unexpected join lines:\n$out")
  }

end JoinFlattenSpec
