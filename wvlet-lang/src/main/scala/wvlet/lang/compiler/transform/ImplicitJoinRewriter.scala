package wvlet.lang.compiler.transform

import wvlet.lang.api.Span
import wvlet.lang.compiler.*
import wvlet.lang.model.expr.*
import wvlet.lang.model.plan.*

/**
  * Rewrite implicit joins (comma-separated FROM tables with join conditions in WHERE) into explicit
  * INNER JOINs with ON conditions.
  *
  * Input pattern:
  * {{{
  *   Filter(
  *     Join(ImplicitJoin, ..., NoJoinCriteria),
  *     AND(filter_conds..., join_conds...)
  *   )
  * }}}
  *
  * Output pattern:
  * {{{
  *   Filter(
  *     Join(InnerJoin, ..., JoinOn(join_conds)),
  *     AND(filter_conds...)
  *   )
  * }}}
  *
  * This enables PatternExtractor to detect shared join patterns across queries, which is critical
  * for cross-query model extraction (e.g., JOB benchmark queries that exclusively use implicit
  * joins).
  */
object ImplicitJoinRewriter extends Phase("implicit-join-rewriter"):

  override def run(unit: CompilationUnit, context: Context): CompilationUnit =
    unit.resolvedPlan = rewriteOnly(unit.resolvedPlan)
    unit

  /** Standalone entry point for use outside the full compiler pipeline (e.g., WvletJS). */
  def rewriteOnly(plan: LogicalPlan): LogicalPlan = rewrite(plan)

  private def rewrite(plan: LogicalPlan): LogicalPlan =
    plan.transformUp {
      case f @ Filter(child: Join, filterExpr, span) if hasImplicitJoin(child) =>
        val preds = flattenAnd(filterExpr)
        pushDown(child, preds, span)
    }

  /** Check whether a relation tree contains any ImplicitJoin with NoJoinCriteria. */
  private def hasImplicitJoin(rel: Relation): Boolean =
    rel match
      case Join(JoinType.ImplicitJoin, left, right, NoJoinCriteria, _, _) =>
        true
      case Join(_, left, right, _, _, _) =>
        hasImplicitJoin(left) || hasImplicitJoin(right)
      case _ =>
        false

  /**
    * Recursively push predicates down into the ImplicitJoin tree, converting each level to
    * InnerJoin when cross-referencing join predicates are found.
    *
    * @param rel
    *   the current relation (typically a Join tree)
    * @param preds
    *   available predicates to distribute
    * @param span
    *   span for newly created nodes
    * @return
    *   the rewritten relation
    */
  private def pushDown(rel: Relation, preds: List[Expression], span: Span): Relation =
    rel match
      case Join(JoinType.ImplicitJoin, left, right, NoJoinCriteria, asof, jSpan) =>
        val leftTables  = collectTableAliases(left)
        val rightTables = collectTableAliases(right)
        val allTables   = leftTables ++ rightTables

        // Classify each predicate
        var joinPreds  = List.empty[Expression]
        var leftPreds  = List.empty[Expression]
        var rightPreds = List.empty[Expression]
        var remaining  = List.empty[Expression]

        for p <- preds do
          val refs = referencedTables(p)
          if refs.isEmpty then
            // Constant predicate like 1=1 — keep as filter
            remaining = p :: remaining
          else if !refs.subsetOf(allTables) then
            // References tables outside this join scope — keep as filter
            remaining = p :: remaining
          else
            val touchesLeft  = refs.exists(leftTables.contains)
            val touchesRight = refs.exists(rightTables.contains)
            if touchesLeft && touchesRight then
              joinPreds = p :: joinPreds
            else if touchesLeft then
              leftPreds = p :: leftPreds
            else if touchesRight then
              rightPreds = p :: rightPreds
            else
              remaining = p :: remaining

        // Recurse into left subtree with its predicates
        val newLeft = pushDown(left, leftPreds.reverse, span)

        // Wrap right with its filter predicates
        val newRight = wrapFilter(right, rightPreds.reverse, span)

        // Build the new join
        val (newJoinType, newCond) =
          if joinPreds.nonEmpty then
            (JoinType.InnerJoin, JoinOn(combineAnd(joinPreds.reverse, span), span))
          else
            (JoinType.ImplicitJoin, NoJoinCriteria)

        val newJoin = Join(newJoinType, newLeft, newRight, newCond, asof, jSpan)

        // Remaining predicates become a Filter above the join
        wrapFilter(newJoin, remaining.reverse, span)

      case _ =>
        // Leaf relation or non-implicit join — wrap with remaining predicates
        wrapFilter(rel, preds, span)

  // ── Helpers ──────────────────────────────────────────────────

  /** Flatten nested AND expressions into a flat list. */
  private def flattenAnd(expr: Expression): List[Expression] =
    expr match
      case And(left, right, _) =>
        flattenAnd(left) ++ flattenAnd(right)
      case other =>
        List(other)

  /** Combine a list of expressions into a nested AND tree. */
  private def combineAnd(exprs: List[Expression], span: Span): Expression =
    exprs match
      case Nil      => throw new IllegalArgumentException("Cannot combine empty predicate list")
      case e :: Nil => e
      case e :: rest =>
        And(e, combineAnd(rest, span), span)

  /** Wrap a relation with a Filter if there are predicates, otherwise return as-is. */
  private def wrapFilter(rel: Relation, preds: List[Expression], span: Span): Relation =
    if preds.isEmpty then rel
    else Filter(rel, combineAnd(preds, span), span)

  /**
    * Collect all table aliases (or table names) from a relation tree. For `FROM t1 AS a, t2 AS b`
    * this returns `Set("a", "b")`, and for `FROM t1, t2` returns `Set("t1", "t2")`.
    */
  private def collectTableAliases(rel: Relation): Set[String] =
    rel match
      case AliasedRelation(_, alias, _, _) =>
        Set(alias.leafName)
      case j: Join =>
        collectTableAliases(j.left) ++ collectTableAliases(j.right)
      case t: TableRef =>
        Set(t.name.leafName)
      case t: TableScan =>
        Set(t.name.name)
      case n: NamedRelation =>
        Set(n.name.leafName)
      case u: UnaryRelation =>
        collectTableAliases(u.child)
      case _ =>
        Set.empty

  /**
    * Extract the set of table aliases referenced by DotRef qualifiers in an expression. For
    * example, `t1.id = t2.id AND t1.name = 'foo'` returns `Set("t1", "t2")`.
    */
  private def referencedTables(expr: Expression): Set[String] =
    val refs = Set.newBuilder[String]
    expr.traverseExpressions { case d: DotRef =>
      d.qualifier match
        case i: Identifier =>
          refs += i.leafName
        case _ =>
    }
    refs.result()

end ImplicitJoinRewriter
