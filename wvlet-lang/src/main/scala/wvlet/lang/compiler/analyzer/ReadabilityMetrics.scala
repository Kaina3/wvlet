package wvlet.lang.compiler.analyzer

import wvlet.lang.model.plan.*
import wvlet.lang.model.expr.*
import wvlet.lang.api.Span

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.IdentityHashMap

object ReadabilityMetrics:

  case class DryDebug(
      DRY: Double,
      total_nodes: Int,
      duplicate_nodes: Int,
      distinct_subplans: Int,
      top_repeated: List[(String, Int)]
  )

  case class Metrics(
      DRY: Double,
      SN: Double,
      PR: Double,
      JI: Double
  )

  private def clamp01(x: Double): Double =
    if x < 0.0 then 0.0
    else if x > 1.0 then 1.0
    else x

  /**
    * Compute SN/PR/JI from a LogicalPlan.
    *
    * This is designed to be used with parse-only compilation (fast and stable).
    */
  def compute(plan: LogicalPlan): Metrics =
    val dry = computeDRY(plan)
    val sn = computeSN(plan)
    val pr = computePR(plan)
    val ji = computeJI(plan)
    Metrics(DRY = dry, SN = sn, PR = pr, JI = ji)

  // -----------------
  // DRY (Don't Repeat Yourself)
  // -----------------

  /**
    * DRY = 1 - duplicates / total_nodes
    *
    * duplicates: number of duplicated subplans (counting occurrences beyond the first)
    * total_nodes: number of unique LogicalPlan nodes (by reference identity) in the plan
    */
  def computeDRY(plan: LogicalPlan): Double =
    computeDRYDebug(plan).DRY

  def computeDRYDebug(plan: LogicalPlan, topK: Int = 10): DryDebug =
    // Collect unique LogicalPlan node instances to avoid double-counting shared nodes (DAG).
    val visited = new IdentityHashMap[LogicalPlan, java.lang.Boolean]()
    val nodes   = scala.collection.mutable.ArrayBuffer.empty[LogicalPlan]

    plan.traverse { case lp: LogicalPlan =>
      if !visited.containsKey(lp) then
        visited.put(lp, java.lang.Boolean.TRUE)
        nodes += lp
    }

    val total = nodes.size
    if total <= 0 then
      DryDebug(DRY = 1.0, total_nodes = 0, duplicate_nodes = 0, distinct_subplans = 0, top_repeated = Nil)
    else
      val counts  = scala.collection.mutable.HashMap.empty[String, Int]
      val samples = scala.collection.mutable.HashMap.empty[String, String]

      nodes.foreach { n =>
        val h = hashPlan(n)
        counts.update(h, counts.getOrElse(h, 0) + 1)
        if !samples.contains(h) then
          // productPrefix is stable and useful to understand what is repeating
          samples.update(h, n.productPrefix)
      }

      val duplicateNodes = counts.valuesIterator.map(c => Math.max(0, c - 1)).sum
      val dry            = clamp01(1.0 - (duplicateNodes.toDouble / total.toDouble))

      val topRepeated =
        counts.iterator
          .collect { case (h, c) if c > 1 =>
            val label = s"${samples.getOrElse(h, "?")}:${h.take(8)}"
            (label, c)
          }
          .toList
          .sortBy { case (_, c) => -c }
          .take(Math.max(0, topK))

      DryDebug(
        DRY = dry,
        total_nodes = total,
        duplicate_nodes = duplicateNodes,
        distinct_subplans = counts.size,
        top_repeated = topRepeated
      )

  /**
    * Compute a "structural hash" for a LogicalPlan.
    *
    * Key insight for DRY differentiation:
    * - Two subqueries with identical AST shape but different identifiers/literals
    *   (e.g., `f_0ac1e IS NOT NULL` vs `f_7e62e IS NOT NULL`) should be considered
    *   duplicates because they represent the SAME PATTERN.
    * - We achieve this by normalizing identifiers and literals to placeholder tokens
    *   before hashing, so only the structural skeleton matters.
    */
  private def hashPlan(plan: LogicalPlan): String =
    // Cache hashes per node instance to avoid repeated hashing on shared nodes.
    val cache = new IdentityHashMap[LogicalPlan, String]()

    def hashAny(x: Any): String =
      x match
        case null =>
          "null"
        case _: Span =>
          // Ignore source locations; they are not semantic and break dedup.
          ""

        // ========== STRUCTURAL NORMALIZATION ==========
        // Normalize identifiers to a placeholder so that different column/table names
        // with the same structure are considered duplicates.
        case id: Identifier =>
          // Use only the type name, not the actual identifier value
          id.productPrefix

        // Normalize literals to placeholders - same type = same structure
        case lit: Literal =>
          // Use only the type name (e.g., "LongLiteral", "StringLiteral")
          lit.productPrefix

        // Normalize NameExpr (used for column/table names) to placeholder
        case ne: NameExpr =>
          "NameExpr"

        // ========== END STRUCTURAL NORMALIZATION ==========

        case lp: LogicalPlan =>
          val prev = cache.get(lp)
          if prev != null then prev
          else
            val sig = productSignature(lp)
            val h   = sha256Hex(sig)
            cache.put(lp, h)
            h
        case e: Expression =>
          sha256Hex(productSignature(e))
        case Some(v) =>
          s"Some(${hashAny(v)})"
        case None =>
          "None"
        case it: Iterable[?] =>
          it.iterator.map(hashAny).mkString("[", ",", "]")
        case p: Product =>
          productSignature(p)
        case other =>
          other.toString

    def productSignature(p: Product): String =
      val prefix = p.productPrefix
      val parts = p.productIterator
        .filterNot(_.isInstanceOf[Span])
        .map(hashAny)
        .mkString(",")
      s"$prefix($parts)"

    hashAny(plan)

  private def sha256Hex(s: String): String =
    val md = MessageDigest.getInstance("SHA-256")
    val bs = md.digest(s.getBytes(StandardCharsets.UTF_8))
    bs.map(b => f"$b%02x").mkString

  // -----------------
  // SN (Subquery Nesting)
  // -----------------

  /**
    * SN = 1 / (1 + penalty / k)
    *
    * penalty = Σ depth (CTE contributes 0.5×depth)
    */
  def computeSN(plan: LogicalPlan, k: Double = 10.0): Double =
    val penalty = subqueryPenalty(plan)
    clamp01(1.0 / (1.0 + penalty / k))

  private def subqueryPenalty(plan: LogicalPlan): Double =
    def traverseAny(node: Any, depth: Int, insideQuery: Boolean): Double =
      node match
        case q: Query =>
          if insideQuery then
            val d = depth + 1
            d.toDouble + traverseAny(q.body, d, insideQuery = true)
          else
            // Root of a new query context (no penalty)
            traverseAny(q.body, depth, insideQuery = true)

        case w: WithQuery =>
          // CTE definitions are treated as subqueries with half penalty.
          val ctePenalty =
            w.queryDefs.map { qd =>
              val d = depth + 1
              (d.toDouble * 0.5) + traverseAny(qd.child, d, insideQuery = false)
            }.sum
          val bodyPenalty = traverseAny(w.queryBody, depth, insideQuery)
          ctePenalty + bodyPenalty

        case lp: LogicalPlan =>
          // Recurse through all constructor args (not just children) so we can
          // catch relations embedded in expressions (e.g., IN (subquery)).
          lp.productIterator.map(x => traverseAny(x, depth, insideQuery)).sum

        case e: Expression =>
          // Expressions can contain embedded relations (e.g., InRelation).
          e.productIterator.map(x => traverseAny(x, depth, insideQuery)).sum

        case Some(x) =>
          traverseAny(x, depth, insideQuery)

        case xs: Iterable[?] =>
          xs.iterator.map(x => traverseAny(x, depth, insideQuery)).sum

        case _ =>
          0.0

    traverseAny(plan, depth = 0, insideQuery = false)

  // -----------------
  // PR (Predicate Readability)
  // -----------------

  private case class PredicateStats(
      numClauses: Int = 0,
      numBoolOps: Int = 0,
      maxDepth: Int = 0,
      hasNot: Boolean = false,
      functionCalls: Int = 0,
      arithmeticOps: Int = 0,
      nodeCount: Int = 0,
      hasAnd: Boolean = false,
      hasOr: Boolean = false
  ):
    def +(o: PredicateStats): PredicateStats =
      PredicateStats(
        numClauses = this.numClauses + o.numClauses,
        numBoolOps = this.numBoolOps + o.numBoolOps,
        maxDepth = Math.max(this.maxDepth, o.maxDepth),
        hasNot = this.hasNot || o.hasNot,
        functionCalls = this.functionCalls + o.functionCalls,
        arithmeticOps = this.arithmeticOps + o.arithmeticOps,
        nodeCount = this.nodeCount + o.nodeCount,
        hasAnd = this.hasAnd || o.hasAnd,
        hasOr = this.hasOr || o.hasOr
      )

  def computePR(plan: LogicalPlan): Double =
    val filters = collectFilterExpressions(plan)
    if filters.isEmpty then
      1.0
    else
      val pc = predicateComplexity(filters)
      val ip = implicitPrecedenceRisk(filters)
      val t = -(pc + ip)
      val pr = 1.0 / (1.0 + Math.exp(-t))
      clamp01(pr)

  private def collectFilterExpressions(plan: LogicalPlan): List[Expression] =
    val buf = List.newBuilder[Expression]

    plan.traverse {
      case f: Filter =>
        buf += f.filterExpr
    }

    buf.result()

  private def predicateComplexity(filters: List[Expression]): Double =
    val stats = filters.map(summarizeExpr).foldLeft(PredicateStats())(_ + _)

    // Feature weights (kept consistent with Python core_metrics.py)
    val c1 = 1.0
    val c2 = 1.5
    val c3 = 2.0
    val c4 = 1.5
    val c5 = 3.0

    val funcOpDensity =
      (stats.functionCalls + stats.arithmeticOps).toDouble / Math.max(stats.nodeCount, 1)

    val rawPc =
      (c1 * stats.numClauses) +
        (c2 * stats.numBoolOps) +
        (c3 * stats.maxDepth) +
        (c4 * (if stats.hasNot then 1 else 0)) +
        (c5 * funcOpDensity)

    // Normalize raw_pc to 0-1 range using empirical max=20.0 (same as Python)
    clamp01(rawPc / 20.0)

  private def implicitPrecedenceRisk(filters: List[Expression]): Double =
    // Approximate IP from the AST:
    // 1) Split by top-level OR into groups
    // 2) Mark a group risky if it contains both AND and OR (i.e., nested OR under AND)
    val groups = filters.flatMap(splitTopLevelOr)
    if groups.isEmpty then
      0.0
    else
      val risky = groups.count { g =>
        val s = summarizeExpr(g)
        s.hasAnd && s.hasOr
      }
      risky.toDouble / groups.size

  private def splitTopLevelOr(e: Expression): List[Expression] =
    e match
      case Or(l, r, _) =>
        splitTopLevelOr(l) ++ splitTopLevelOr(r)
      case _ =>
        List(e)

  private def summarizeExpr(e: Expression): PredicateStats =
    def loop(expr: Expression, depth: Int): PredicateStats =
      val childStats = expr.children.toList.map(ch => loop(ch, depth + 1))
      val merged = childStats.foldLeft(PredicateStats())(_ + _)

      val base =
        expr match
          case And(_, _, _) =>
            PredicateStats(numBoolOps = 1, hasAnd = true)
          case Or(_, _, _) =>
            PredicateStats(numBoolOps = 1, hasOr = true)
          case Not(_, _) =>
            PredicateStats(numBoolOps = 1, hasNot = true)
          case _: LogicalConditionalExpression =>
            PredicateStats()
          case _: ConditionalExpression =>
            // Count non-logical conditional expressions as a clause
            PredicateStats(numClauses = 1)
          case _: FunctionApply =>
            PredicateStats(functionCalls = 1)
          case _: ArithmeticBinaryExpr | _: ArithmeticUnaryExpr =>
            PredicateStats(arithmeticOps = 1)
          case _ =>
            PredicateStats()

      // Every node contributes to nodeCount
      (merged + base).copy(maxDepth = Math.max(merged.maxDepth, depth), nodeCount = merged.nodeCount + 1)

    loop(e, depth = 1)

  // -----------------
  // JI (Join Intent Clarity)
  // -----------------

  def computeJI(plan: LogicalPlan): Double =
    val joins = collectJoins(plan)
    if joins.isEmpty then
      1.0
    else
      val J = joins.size.toDouble

      val implicitJoins = joins.count(_.joinType == JoinType.ImplicitJoin).toDouble
      val missingCriteria = joins.count(j => j.cond == NoJoinCriteria).toDouble

      val (equi, total) = joins.foldLeft((0, 0)) { case ((eqAcc, totAcc), j) =>
        val (eq, tot) = joinEquiStats(j.cond)
        (eqAcc + eq, totAcc + tot)
      }

      val equiRatio =
        if total > 0 then
          equi.toDouble / total
        else
          0.0

      val pImpl = implicitJoins / J
      val pMissing = missingCriteria / J
      val pOpaque = 1.0 - equiRatio

      val alpha = 0.4
      val beta = 0.4
      val gamma = 0.2

      val penalty = (alpha * pImpl) + (beta * pMissing) + (gamma * pOpaque)
      clamp01(1.0 - penalty)

  private def collectJoins(plan: LogicalPlan): List[Join] =
    val buf = List.newBuilder[Join]
    plan.traverse { case j: Join => buf += j }
    buf.result()

  private def joinEquiStats(cond: JoinCriteria): (Int, Int) =
    cond match
      case NoJoinCriteria =>
        (0, 0)
      case _: NaturalJoin =>
        (1, 1)
      case _: JoinOnTheSameColumns =>
        (1, 1)
      case _: JoinOnEq =>
        (1, 1)
      case JoinOn(expr, _) =>
        val s = summarizeJoinExpr(expr)
        (s.equiCount, s.totalCount)

  private case class JoinExprStats(equiCount: Int, totalCount: Int)

  private def summarizeJoinExpr(e: Expression): JoinExprStats =
    def loop(x: Expression): JoinExprStats =
      val children = x.children.toList.map(loop)
      val merged = children.foldLeft(JoinExprStats(0, 0)) { (a, b) =>
        JoinExprStats(a.equiCount + b.equiCount, a.totalCount + b.totalCount)
      }

      x match
        case Eq(_, _, _) =>
          JoinExprStats(merged.equiCount + 1, merged.totalCount + 1)
        case _: ConditionalExpression =>
          JoinExprStats(merged.equiCount, merged.totalCount + 1)
        case _ =>
          merged

    loop(e)
