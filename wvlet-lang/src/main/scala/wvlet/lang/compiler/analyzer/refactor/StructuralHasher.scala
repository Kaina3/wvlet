/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package wvlet.lang.compiler.analyzer.refactor

import wvlet.lang.model.plan.*
import wvlet.lang.model.expr.*
import wvlet.log.LogSupport

import scala.util.hashing.MurmurHash3

/**
  * Configuration for structural hashing
  *
  * @param ignoreLiterals
  *   If true, literal values are ignored (treated as wildcards)
  * @param ignoreIdentifiers
  *   If true, identifier names are ignored (treated as wildcards)
  * @param ignoreAliases
  *   If true, alias names are ignored
  * @param normalizeCommutative
  *   If true, commutative operations are normalized (sorted)
  */
case class HashConfig(
    ignoreLiterals: Boolean = true,
    ignoreIdentifiers: Boolean = false,
    ignoreAliases: Boolean = true,
    normalizeCommutative: Boolean = true
)

object HashConfig:
  val default: HashConfig = HashConfig()

  // For exact matching (no wildcards)
  val exact: HashConfig = HashConfig(
    ignoreLiterals = false,
    ignoreIdentifiers = false,
    ignoreAliases = false,
    normalizeCommutative = false
  )

/**
  * Computes structural hash of LogicalPlan and Expression nodes.
  *
  * Structural hash is designed to identify structurally similar subtrees while ignoring certain
  * details like literal values and identifier names (which can be parameterized).
  */
object StructuralHasher extends LogSupport:

  /**
    * Compute structural hash of a LogicalPlan node
    */
  def hash(plan: LogicalPlan, config: HashConfig = HashConfig.default): Int =
    hashPlan(plan, config)

  /**
    * Compute structural hash of an Expression node
    */
  def hashExpr(expr: Expression, config: HashConfig = HashConfig.default): Int =
    hashExpression(expr, config)

  private def hashPlan(plan: LogicalPlan, config: HashConfig): Int =
    plan match
      // === Leaf nodes ===
      case t: TableRef =>
        combineHash("TableRef", if config.ignoreAliases then 0 else t.name.fullName.hashCode)

      case t: TableScan =>
        combineHash("TableScan", if config.ignoreAliases then 0 else t.name.fullName.hashCode)

      case t: FileScan =>
        combineHash("FileScan", if config.ignoreAliases then 0 else t.path.hashCode)

      case v: Values =>
        combineHash(
          "Values",
          v.rows.map(r => hashExpression(r, config)).hashCode
        )

      case e: EmptyRelation =>
        combineHash("EmptyRelation")

      // === Unary relations ===
      case f: Filter =>
        combineHash(
          "Filter",
          hashPlan(f.child, config),
          hashExpression(f.filterExpr, config)
        )

      case p: Project =>
        combineHash(
          "Project",
          hashPlan(p.child, config),
          p.selectItems.map(a => hashAttribute(a, config)).hashCode
        )

      case a: Agg =>
        combineHash(
          "Agg",
          hashPlan(a.child, config),
          a.keys.map(k => hashAttribute(k, config)).hashCode,
          a.aggExprs.map(e => hashAttribute(e, config)).hashCode
        )

      case g: GroupBy =>
        combineHash(
          "GroupBy",
          hashPlan(g.child, config),
          g.groupingKeys.map(k => hashExpression(k, config)).hashCode
        )

      case s: Sort =>
        combineHash(
          "Sort",
          hashPlan(s.child, config),
          s.orderBy.map(o => hashSortItem(o, config)).hashCode
        )

      case l: Limit =>
        combineHash(
          "Limit",
          hashPlan(l.child, config),
          if config.ignoreLiterals then 0 else hashExpression(l.limit, config)
        )

      case d: Distinct =>
        combineHash("Distinct", hashPlan(d.child, config))

      case a: AliasedRelation =>
        combineHash(
          "AliasedRelation",
          hashPlan(a.child, config),
          if config.ignoreAliases then 0 else a.alias.fullName.hashCode
        )

      // === Binary relations ===
      case j: Join =>
        val leftHash  = hashPlan(j.left, config)
        val rightHash = hashPlan(j.right, config)
        val condHash  = hashJoinCriteria(j.cond, config)
        val childHashes =
          if config.normalizeCommutative && isCommutativeJoin(j.joinType) then
            Seq(leftHash, rightHash).sorted
          else
            Seq(leftHash, rightHash)
        combineHash(
          "Join",
          j.joinType.toString.hashCode,
          childHashes(0),
          childHashes(1),
          condHash
        )

      case u: Union =>
        combineHash(
          "Union",
          hashPlan(u.left, config),
          hashPlan(u.right, config)
        )

      case i: Intersect =>
        combineHash(
          "Intersect",
          hashPlan(i.left, config),
          hashPlan(i.right, config)
        )

      case e: Except =>
        combineHash(
          "Except",
          hashPlan(e.left, config),
          hashPlan(e.right, config)
        )

      // === Other relations ===
      case m: ModelScan =>
        combineHash(
          "ModelScan",
          if config.ignoreAliases then 0 else m.name.fullName.hashCode,
          m.modelArgs.map(a => hashFunctionArg(a, config)).hashCode
        )

      case w: WithQuery =>
        combineHash(
          "WithQuery",
          w.queryDefs.map(d => hashPlan(d, config)).hashCode,
          hashPlan(w.queryBody, config)
        )

      case p: Pivot =>
        combineHash(
          "Pivot",
          hashPlan(p.child, config),
          p.pivotKeys.map(pk => hashPivotKey(pk, config)).hashCode,
          p.groupingKeys.map(gk => hashExpression(gk, config)).hashCode
        )

      // === Default fallback ===
      case other =>
        // For unknown nodes, hash based on class name and children
        val childHashes = other.children.map(c => hashPlan(c, config))
        combineHash(other.getClass.getSimpleName, childHashes.hashCode)

  private def hashExpression(expr: Expression, config: HashConfig): Int =
    expr match
      // === Leaf expressions ===
      case l: Literal =>
        if config.ignoreLiterals then
          combineHash("Literal", l.dataType.toString.hashCode)
        else
          combineHash("Literal", l.stringValue.hashCode)

      case w: Wildcard =>
        combineHash("Wildcard")

      case i: Identifier =>
        if config.ignoreIdentifiers then
          combineHash("Identifier")
        else
          combineHash("Identifier", i.unquotedValue.hashCode)

      case q: QualifiedName =>
        if config.ignoreIdentifiers then
          combineHash("QualifiedName", q.nameParts.length)
        else
          combineHash("QualifiedName", q.fullName.hashCode)

      case a: AllColumns =>
        combineHash("AllColumns", a.nameExpr.fullName.hashCode)

      case p: NamedParameter =>
        combineHash("NamedParameter", if config.ignoreIdentifiers then 0 else p.name.hashCode)

      // === Arithmetic expressions ===
      case arith: ArithmeticBinaryExpr =>
        val leftHash  = hashExpression(arith.left, config)
        val rightHash = hashExpression(arith.right, config)
        // Normalize commutative operations (Add, Multiply)
        val (lh, rh) = arith.exprType match
          case BinaryExprType.Add | BinaryExprType.Multiply =>
            if config.normalizeCommutative && leftHash > rightHash then (rightHash, leftHash)
            else (leftHash, rightHash)
          case _ =>
            (leftHash, rightHash)
        combineHash("ArithmeticBinaryExpr", arith.exprType.toString.hashCode, lh, rh)

      // === Comparison expressions ===
      case e: Eq =>
        val (lh, rh) = normalizeCommutative(e.left, e.right, config)
        combineHash("Eq", lh, rh)

      case n: NotEq =>
        val (lh, rh) = normalizeCommutative(n.left, n.right, config)
        combineHash("NotEq", lh, rh)

      case l: LessThan =>
        combineHash("LessThan", hashExpression(l.left, config), hashExpression(l.right, config))

      case l: LessThanOrEq =>
        combineHash(
          "LessThanOrEq",
          hashExpression(l.left, config),
          hashExpression(l.right, config)
        )

      case g: GreaterThan =>
        combineHash("GreaterThan", hashExpression(g.left, config), hashExpression(g.right, config))

      case g: GreaterThanOrEq =>
        combineHash(
          "GreaterThanOrEq",
          hashExpression(g.left, config),
          hashExpression(g.right, config)
        )

      // === Logical expressions ===
      case a: And =>
        val (lh, rh) = normalizeCommutative(a.left, a.right, config)
        combineHash("And", lh, rh)

      case o: Or =>
        val (lh, rh) = normalizeCommutative(o.left, o.right, config)
        combineHash("Or", lh, rh)

      case n: Not =>
        combineHash("Not", hashExpression(n.child, config))

      // === Other expressions ===
      case i: IsNull =>
        combineHash("IsNull", hashExpression(i.child, config))

      case i: IsNotNull =>
        combineHash("IsNotNull", hashExpression(i.child, config))

      case b: Between =>
        combineHash(
          "Between",
          hashExpression(b.e, config),
          hashExpression(b.a, config),
          hashExpression(b.b, config)
        )

      case i: In =>
        combineHash(
          "In",
          hashExpression(i.a, config),
          i.list.map(e => hashExpression(e, config)).hashCode
        )

      case l: Like =>
        combineHash(
          "Like",
          hashExpression(l.left, config),
          hashExpression(l.right, config)
        )

      case c: Cast =>
        combineHash(
          "Cast",
          hashExpression(c.expr, config),
          c.castType.toString.hashCode
        )

      case f: FunctionApply =>
        combineHash(
          "FunctionApply",
          hashExpression(f.base, config), // base is the function name expression
          f.args.map(a => hashFunctionArg(a, config)).hashCode
        )

      case c: CaseExpr =>
        combineHash(
          "CaseExpr",
          c.target.map(o => hashExpression(o, config)).getOrElse(0),
          c.whenClauses.map(w => hashWhenClause(w, config)).hashCode,
          c.elseClause.map(e => hashExpression(e, config)).getOrElse(0)
        )

      case s: SingleColumn =>
        combineHash(
          "SingleColumn",
          hashExpression(s.expr, config),
          if config.ignoreAliases then 0 else s.nameExpr.fullName.hashCode
        )

      case i: IfExpr =>
        combineHash(
          "IfExpr",
          hashExpression(i.cond, config),
          hashExpression(i.onTrue, config),
          hashExpression(i.onFalse, config)
        )

      // === Default fallback ===
      case other =>
        combineHash(other.getClass.getSimpleName)

  private def hashFunctionArg(arg: FunctionArg, config: HashConfig): Int =
    combineHash(
      "FunctionArg",
      arg.name.map(_.hashCode).getOrElse(0),
      hashExpression(arg.value, config),
      arg.isDistinct.hashCode
    )

  private def hashAttribute(attr: Attribute, config: HashConfig): Int =
    attr match
      case s: SingleColumn =>
        combineHash(
          "SingleColumn",
          hashExpression(s.expr, config),
          if config.ignoreAliases then 0 else s.nameExpr.fullName.hashCode
        )
      case a: AllColumns =>
        combineHash("AllColumns", a.nameExpr.fullName.hashCode)
      case other =>
        hashExpression(other, config)

  private def hashSortItem(item: SortItem, config: HashConfig): Int =
    combineHash(
      "SortItem",
      hashExpression(item.sortKey, config),
      item.ordering.map(_.toString.hashCode).getOrElse(0),
      item.nullOrdering.map(_.toString.hashCode).getOrElse(0)
    )

  private def hashWhenClause(clause: WhenClause, config: HashConfig): Int =
    combineHash(
      "WhenClause",
      hashExpression(clause.condition, config),
      hashExpression(clause.result, config)
    )

  private def hashJoinCriteria(criteria: JoinCriteria, config: HashConfig): Int =
    criteria match
      case j: JoinOn =>
        combineHash("JoinOn", hashExpression(j.expr, config))
      case j: JoinUsing =>
        combineHash("JoinUsing", j.columns.map(c => c.fullName.hashCode).hashCode)
      case _: NaturalJoin =>
        combineHash("NaturalJoin")
      case other =>
        combineHash(other.getClass.getSimpleName)

  private def hashPivotKey(pivotKey: PivotKey, config: HashConfig): Int =
    combineHash(
      "PivotKey",
      hashExpression(pivotKey.name, config),
      pivotKey.values.map(v => hashExpression(v, config)).hashCode
    )

  private def normalizeCommutative(
      left: Expression,
      right: Expression,
      config: HashConfig
  ): (Int, Int) =
    val lh = hashExpression(left, config)
    val rh = hashExpression(right, config)
    if config.normalizeCommutative then
      if lh <= rh then (lh, rh) else (rh, lh)
    else
      (lh, rh)

  private def isCommutativeJoin(joinType: JoinType): Boolean =
    joinType match
      case JoinType.InnerJoin | JoinType.CrossJoin => true
      case _                                       => false

  private def combineHash(parts: Any*): Int =
    MurmurHash3.orderedHash(parts.map {
      case s: String => s.hashCode
      case i: Int    => i
      case other     => other.hashCode
    })

end StructuralHasher
