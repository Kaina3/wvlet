package wvlet.lang.compiler.transform

import wvlet.lang.compiler.*
import wvlet.lang.model.plan.*

/**
  * Flatten redundant parentheses around JOIN trees.
  *
  * SqlParser represents nested parenthesized relations as BracedRelation. When such braces wrap
  * left-deep JOIN trees, WvletGenerator ends up printing deeply nested braces. This phase removes
  * BracedRelation wrappers whose child is a Join (recursively), without changing join semantics.
  */
object JoinFlattener extends Phase("join-flattener"):

  override def run(unit: CompilationUnit, context: Context): CompilationUnit =
    unit.resolvedPlan = rewriteOnly(unit.resolvedPlan)
    unit

  def rewriteOnly(plan: LogicalPlan): LogicalPlan =
    plan.transformUp {
      case b: BracedRelation if b.child.isInstanceOf[Join] =>
        // Unwrap braces that only exist due to nested parentheses in SQL.
        stripJoinBraces(b.child)
      case j: Join =>
        // Ensure we also unwrap braces in join operands.
        val newLeft  = stripJoinBraces(j.left)
        val newRight = stripJoinBraces(j.right)
        if (newLeft eq j.left) && (newRight eq j.right) then
          j
        else
          j.copy(left = newLeft, right = newRight)
    }

  private def stripJoinBraces(r: Relation): Relation =
    r match
      case b: BracedRelation if b.child.isInstanceOf[Join] =>
        stripJoinBraces(b.child)
      case j: Join =>
        val newLeft  = stripJoinBraces(j.left)
        val newRight = stripJoinBraces(j.right)
        if (newLeft eq j.left) && (newRight eq j.right) then
          j
        else
          j.copy(left = newLeft, right = newRight)
      case other =>
        other

end JoinFlattener
