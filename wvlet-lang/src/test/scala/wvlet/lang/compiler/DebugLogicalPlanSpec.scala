package wvlet.lang.compiler

import wvlet.airspec.AirSpec
import wvlet.lang.compiler.parser.ParserPhase
import wvlet.lang.compiler.transform.RewriteExpr
import java.io.File

/**
  * A test to debug and print the logical plan of a specific query.
  * 
  * Run with: ./sbt 'langJVM/testOnly wvlet.lang.compiler.DebugLogicalPlanSpec'
  */
class DebugLogicalPlanSpec extends AirSpec:

  // ============================================================
  // ★★★ 見たいファイルのパスをここに指定 ★★★
  // ============================================================
  val targetFile = "/Users/andersonkaina/Desktop/rs_system/QueryReadability/query_readability_analyze/data/treasuredata/sql/query_43519.sql"
  // ============================================================

  test("Show logical plan from file") {
    val file = new File(targetFile)
    require(file.exists(), s"File not found: $targetFile")

    val content = scala.io.Source.fromFile(file).mkString
    
    // Determine if SQL or Wvlet based on extension
    val unit = 
      if targetFile.endsWith(".sql") then
        CompilationUnit.fromSqlString(content)
      else
        CompilationUnit.fromWvletString(content)

    val unresolvedPlan = ParserPhase.parseOnly(unit)
    val rewrittenPlan = RewriteExpr.rewriteOnly(unresolvedPlan)

    println("=" * 80)
    println(s"FILE: $targetFile")
    println("=" * 80)
    println("SOURCE:")
    println("-" * 80)
    println(content)
    println("-" * 80)
    println()
    println("LOGICAL PLAN:")
    println("=" * 80)
    println(rewrittenPlan.pp)
    println("=" * 80)
  }

  test("Show logical plan for inline Wvlet query") {
    // ============================================
    // インラインでクエリを指定する場合はこちら
    // ============================================
    val wvletQuery = """
      |from orders
      |join customers on orders.customer_id = customers.id
      |where orders.status = 'active'
      |select orders.id, customers.name
    """.stripMargin.trim

    val unit = CompilationUnit.fromWvletString(wvletQuery)
    val unresolvedPlan = ParserPhase.parseOnly(unit)
    val rewrittenPlan = RewriteExpr.rewriteOnly(unresolvedPlan)

    println("=" * 80)
    println("INLINE WVLET QUERY:")
    println("-" * 80)
    println(wvletQuery)
    println("-" * 80)
    println()
    println("LOGICAL PLAN:")
    println("=" * 80)
    println(rewrittenPlan.pp)
    println("=" * 80)
  }

  test("Show logical plan for inline SQL query") {
    // ============================================
    // SQLクエリを指定する場合はこちら
    // ============================================
    val sqlQuery = """
      |SELECT o.id, c.name
      |FROM orders o
      |JOIN customers c ON o.customer_id = c.id
      |WHERE o.status = 'active'
    """.stripMargin.trim

    val unit = CompilationUnit.fromSqlString(sqlQuery)
    val unresolvedPlan = ParserPhase.parseOnly(unit)
    val rewrittenPlan = RewriteExpr.rewriteOnly(unresolvedPlan)

    println("=" * 80)
    println("INLINE SQL QUERY:")
    println("-" * 80)
    println(sqlQuery)
    println("-" * 80)
    println()
    println("LOGICAL PLAN:")
    println("=" * 80)
    println(rewrittenPlan.pp)
    println("=" * 80)
  }

end DebugLogicalPlanSpec
