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

import wvlet.lang.api.Span
import wvlet.lang.api.Span.NoSpan
import wvlet.lang.model.DataType
import wvlet.lang.model.plan.*
import wvlet.lang.model.expr.*
import wvlet.log.LogSupport

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger

/**
  * Represents an extracted parameter from Anti-Unification
  *
  * @param id
  *   Unique parameter identifier (e.g., "$P1")
  * @param inferredType
  *   Inferred data type of the parameter
  * @param values
  *   The actual values from each instance
  * @param positions
  *   Descriptions of where this parameter appears in the pattern
  */
case class ExtractedParameter(
    id: String,
    inferredType: DataType,
    values: List[Any],
    positions: List[String]
):
  /**
    * Number of distinct values
    */
  def distinctValueCount: Int = values.distinct.size

  /**
    * Whether all values are the same (no parameterization needed)
    */
  def isConstant: Boolean = distinctValueCount == 1

  /**
    * Whether values are string literals
    */
  def isStringType: Boolean = inferredType == DataType.StringType

  /**
    * Whether values are numeric
    */
  def isNumericType: Boolean =
    inferredType == DataType.IntType ||
      inferredType == DataType.LongType ||
      inferredType == DataType.DoubleType

end ExtractedParameter

/**
  * Represents a substitution for a single instance
  *
  * @param instanceId
  *   Index of the instance in the original list
  * @param bindings
  *   Map from parameter ID to actual value
  * @param sourceId
  *   Optional source identifier
  */
case class Substitution(
    instanceId: Int,
    bindings: Map[String, Any],
    sourceId: Option[String] = None
)

/**
  * Result of Anti-Unification
  *
  * @param pattern
  *   The generalized pattern with parameter placeholders
  * @param parameters
  *   List of extracted parameters
  * @param substitutions
  *   Substitutions for each original instance
  */
case class AntiUnifyResult(
    pattern: LogicalPlan,
    parameters: List[ExtractedParameter],
    substitutions: List[Substitution]
):
  /**
    * Number of original instances unified
    */
  def instanceCount: Int = substitutions.size

  /**
    * Total number of parameters extracted
    */
  def parameterCount: Int = parameters.size

  /**
    * Parameters that actually vary across instances
    */
  def variableParameters: List[ExtractedParameter] = parameters.filterNot(_.isConstant)

  /**
    * Check if the pattern is useful (has varying parameters)
    */
  def isUseful: Boolean = variableParameters.nonEmpty

  /**
    * Summary string
    */
  def summary: String =
    s"""AntiUnifyResult:
       |  Instances: $instanceCount
       |  Parameters: $parameterCount (${variableParameters.size} variable)
       |  Parameters: ${parameters.map(p => s"${p.id}: ${p.inferredType}").mkString(", ")}
       |""".stripMargin

end AntiUnifyResult

/**
  * Exception thrown when Anti-Unification is not possible
  */
class CannotUnifyException(message: String) extends Exception(message)

/**
  * Anti-Unification (Least General Generalization) for LogicalPlan trees.
  *
  * Given multiple structurally similar trees, extracts the common pattern and identifies differing
  * parts as parameters.
  */
object AntiUnifier extends LogSupport:

  private val paramCounter = new AtomicInteger(0)

  /**
    * Reset parameter counter (for testing)
    */
  def resetCounter(): Unit = paramCounter.set(0)

  /**
    * Anti-unify a group of duplicate plans
    *
    * @param group
    *   A DuplicateGroup containing structurally similar plans
    * @return
    *   AntiUnifyResult if successful
    */
  def unify(group: DuplicateGroup): Option[AntiUnifyResult] =
    unify(group.plans, group.subtrees.flatMap(_.sourceId))

  /**
    * Anti-unify a list of LogicalPlans
    *
    * @param plans
    *   List of structurally similar plans
    * @param sourceIds
    *   Optional source identifiers for each plan
    * @return
    *   AntiUnifyResult if successful
    */
  def unify(plans: List[LogicalPlan], sourceIds: List[String] = Nil): Option[AntiUnifyResult] =
    if plans.size < 2 then
      debug("Cannot unify fewer than 2 plans")
      return None

    try
      val context                         = new UnificationContext(plans.size, sourceIds)
      val pattern                         = unifyPlans(plans, context)
      val (parameters, substitutions)     = context.build()
      
      val variableParams = parameters.filterNot(_.isConstant)
      
      Some(AntiUnifyResult(pattern, parameters, substitutions))
    catch
      case e: CannotUnifyException =>
        debug(s"Cannot unify: ${e.getMessage}")
        None

  /**
    * Context for tracking parameters and substitutions during unification
    */
  private class UnificationContext(instanceCount: Int, sourceIds: List[String]):
    private val parameters        = mutable.ListBuffer[ExtractedParameter]()
    private val substitutions     = (0 until instanceCount).map { i =>
      val sourceId = if i < sourceIds.size then Some(sourceIds(i)) else None
      Substitution(i, mutable.Map.empty[String, Any].toMap, sourceId)
    }.toArray

    private val bindingsBuilders = (0 until instanceCount).map(_ =>
      mutable.Map.empty[String, Any]
    ).toArray

    def createParameter(values: List[Any], position: String): String =
      val paramId      = s"$$P${paramCounter.incrementAndGet()}"
      val inferredType = inferType(values)

      parameters += ExtractedParameter(
        id = paramId,
        inferredType = inferredType,
        values = values,
        positions = List(position)
      )

      // Record bindings for each instance
      values.zipWithIndex.foreach { case (value, idx) =>
        bindingsBuilders(idx)(paramId) = value
      }

      paramId

    def build(): (List[ExtractedParameter], List[Substitution]) =
      val subs = substitutions.zipWithIndex.map { case (sub, idx) =>
        sub.copy(bindings = bindingsBuilders(idx).toMap)
      }
      (parameters.toList, subs.toList)

  end UnificationContext

  /**
    * Unify a list of LogicalPlans
    */
  private def unifyPlans(plans: List[LogicalPlan], ctx: UnificationContext): LogicalPlan =
    // Check all plans have the same type
    val nodeTypes = plans.map(_.getClass).distinct
    if nodeTypes.size != 1 then
      throw new CannotUnifyException(s"Different node types: ${nodeTypes.map(_.getSimpleName).mkString(", ")}")

    // Debug: log node type being processed
    val nodeType = plans.head.getClass.getSimpleName
    
    plans.head match
      // === Unary relations ===
      case _: Filter =>
        val filters   = plans.map(_.asInstanceOf[Filter])
        val children  = unifyRelations(filters.map(_.child), ctx)
        val filterExprs = filters.map(_.filterExpr)
        val filterExpr = unifyExprs(filterExprs, ctx, "filter_expr")
        Filter(children, filterExpr, NoSpan)

      case _: Project =>
        val projects  = plans.map(_.asInstanceOf[Project])
        val children  = unifyRelations(projects.map(_.child), ctx)
        val items     = unifySelectItems(projects.map(_.selectItems), ctx)
        Project(children, items, NoSpan)

      case _: GroupBy =>
        val groupBys  = plans.map(_.asInstanceOf[GroupBy])
        val children  = unifyRelations(groupBys.map(_.child), ctx)
        val keys      = unifyGroupingKeys(groupBys.map(_.groupingKeys), ctx)
        GroupBy(children, keys, NoSpan)

      case _: Agg =>
        val aggs      = plans.map(_.asInstanceOf[Agg])
        val children  = unifyRelations(aggs.map(_.child), ctx)
        val keys      = unifySelectItems(aggs.map(_.keys), ctx)
        val aggExprs  = unifySelectItems(aggs.map(_.aggExprs), ctx)
        Agg(children, keys, aggExprs, NoSpan)

      case _: Sort =>
        val sorts     = plans.map(_.asInstanceOf[Sort])
        val children  = unifyRelations(sorts.map(_.child), ctx)
        val orderBy   = unifySortItems(sorts.map(_.orderBy), ctx)
        Sort(children, orderBy, NoSpan)

      case _: Limit =>
        val limits    = plans.map(_.asInstanceOf[Limit])
        val children  = unifyRelations(limits.map(_.child), ctx)
        // Limit requires LongLiteral
        val limitExprs = limits.map(_.limit)
        val limitExpr = if limitExprs.map(_.value).distinct.size == 1 then
          limitExprs.head
        else
          ctx.createParameter(limitExprs.map(_.value), "limit")
          limitExprs.head
        Limit(children, limitExpr, NoSpan)

      case _: Distinct =>
        val distincts = plans.map(_.asInstanceOf[Distinct])
        val children  = unifyProjections(distincts.map(_.child), ctx)
        Distinct(children, NoSpan)

      case _: Dedup =>
        val dedups   = plans.map(_.asInstanceOf[Dedup])
        val children = unifyRelations(dedups.map(_.child), ctx)
        Dedup(children, NoSpan)

      case _: Query =>
        val queries  = plans.map(_.asInstanceOf[Query])
        val children = unifyRelations(queries.map(_.child), ctx)
        Query(children, NoSpan)

      case _: AliasedRelation =>
        val aliased   = plans.map(_.asInstanceOf[AliasedRelation])
        val children  = unifyRelations(aliased.map(_.child), ctx)
        // Keep the first alias (or parameterize if needed)
        AliasedRelation(children, aliased.head.alias, aliased.head.columnNames, NoSpan)

      // === Binary relations ===
      case _: Join =>
        val joins     = plans.map(_.asInstanceOf[Join])
        // Check join types are the same
        val joinTypes = joins.map(_.joinType).distinct
        if joinTypes.size != 1 then
          throw new CannotUnifyException(s"Different join types: ${joinTypes.mkString(", ")}")
        val left      = unifyRelations(joins.map(_.left), ctx)
        val right     = unifyRelations(joins.map(_.right), ctx)
        val cond      = unifyJoinCriteria(joins.map(_.cond), ctx)
        Join(joinTypes.head, left, right, cond, false, NoSpan)

      case _: Union =>
        val unions    = plans.map(_.asInstanceOf[Union])
        val left      = unifyRelations(unions.map(_.left), ctx)
        val right     = unifyRelations(unions.map(_.right), ctx)
        Union(left, right, unions.head.isDistinct, NoSpan)

      case _: Intersect =>
        val intersects = plans.map(_.asInstanceOf[Intersect])
        val left       = unifyRelations(intersects.map(_.left), ctx)
        val right      = unifyRelations(intersects.map(_.right), ctx)
        Intersect(left, right, intersects.head.isDistinct, NoSpan)

      case _: Except =>
        val excepts   = plans.map(_.asInstanceOf[Except])
        val left      = unifyRelations(excepts.map(_.left), ctx)
        val right     = unifyRelations(excepts.map(_.right), ctx)
        Except(left, right, excepts.head.isDistinct, NoSpan)

      case _: Concat =>
        val concats   = plans.map(_.asInstanceOf[Concat])
        val left      = unifyRelations(concats.map(_.left), ctx)
        val right     = unifyRelations(concats.map(_.right), ctx)
        Concat(left, right, NoSpan)

      // === Write operations ===
      case _: AppendTo =>
        val appends   = plans.map(_.asInstanceOf[AppendTo])
        val children  = unifyRelations(appends.map(_.child), ctx)
        val targets   = appends.map(_.target.toString)
        if targets.distinct.size == 1 then
          AppendTo(children, appends.head.target, appends.head.columns, NoSpan)
        else
          val paramId = ctx.createParameter(targets, "append_target")
          // TableOrFileName is StringLiteral | QualifiedName, tableNameParamRef returns a QualifiedName
          AppendTo(children, tableNameParamRef(paramId), appends.head.columns, NoSpan)

      // === Leaf nodes ===
      case _: TableRef =>
        val refs      = plans.map(_.asInstanceOf[TableRef])
        val names     = refs.map(_.name.fullName)
        if names.distinct.size == 1 then
          refs.head
        else
          // Different table names - create parameter
          val paramId = ctx.createParameter(names, "table_ref")
          TableRef(tableNameParamRef(paramId), NoSpan)

      case _: TableScan =>
        val scans     = plans.map(_.asInstanceOf[TableScan])
        val names     = scans.map(_.name.fullName)
        if names.distinct.size == 1 then
          scans.head
        else
          val paramId = ctx.createParameter(names, "table_scan")
          // TableScan uses Catalog.TableName, which can't represent an interpolated identifier.
          // For generalized patterns, emit a TableRef with a backquote-interpolated identifier.
          TableRef(tableNameParamRef(paramId), NoSpan)

      // === Other nodes - try to unify children recursively ===
      case other =>
        debug(s"Unifying node type with generic handler: ${other.getClass.getSimpleName}")
        // For unknown nodes, attempt to unify children recursively
        val children = plans.map(_.children)
        if children.nonEmpty && children.map(_.size).distinct.size == 1 && children.head.nonEmpty then
          // All plans have same number of children
          val numChildren = children.head.size
          val unifiedChildren = (0 until numChildren).map { i =>
            val childPlans = children.map(_(i))
            unifyPlans(childPlans, ctx)
          }.toList
          
          // Reconstruct the node with unified children using mapChildren
          var childIdx = 0
          val result = plans.head.mapChildren { _ =>
            val unified = if childIdx < unifiedChildren.size then
              unifiedChildren(childIdx)
            else
              plans.head.children(childIdx)
            childIdx += 1
            unified
          }
          result
        else
          // Different child structures - return first as is
          plans.head

  /**
    * Unify a list of Relations (wrapper for type safety)
    */
  private def unifyRelations(relations: List[Relation], ctx: UnificationContext): Relation =
    unifyPlans(relations, ctx).asInstanceOf[Relation]

  /**
    * Create a QualifiedName that refers to a named parameter (e.g., $P3) as an interpolated identifier.
    * This is used for parameterizing table names in patterns so that generated models actually use
    * their table-name arguments instead of hard-coding the first occurrence.
    */
  private def tableNameParamRef(paramId: String): QualifiedName =
    val paramName = paramId.stripPrefix("$")
    BackquoteInterpolatedIdentifier(
      prefix = NameExpr.EmptyName,
      parts = List(NamedParameter(paramName, NoSpan)),
      dataType = DataType.UnknownType,
      span = NoSpan
    )

  /**
    * Unify a list of Project nodes (wrapper for Distinct)
    */
  private def unifyProjections(projects: List[Project], ctx: UnificationContext): Project =
    unifyPlans(projects, ctx).asInstanceOf[Project]

  /**
    * Unify JoinCriteria
    */
  private def unifyJoinCriteria(criteria: List[JoinCriteria], ctx: UnificationContext): JoinCriteria =
    val criteriaTypes = criteria.map(_.getClass).distinct
    if criteriaTypes.size != 1 then
      throw new CannotUnifyException(s"Different join criteria types: ${criteriaTypes.map(_.getSimpleName).mkString(", ")}")

    criteria.head match
      case _: JoinOn =>
        val joinOns = criteria.map(_.asInstanceOf[JoinOn])
        val exprs = unifyExprs(joinOns.map(_.expr), ctx, "join_on")
        JoinOn(exprs, NoSpan)
      case _: JoinUsing =>
        val joinUsings = criteria.map(_.asInstanceOf[JoinUsing])
        // Keep the first one
        joinUsings.head
      case _: NaturalJoin =>
        NaturalJoin(NoSpan)
      case other =>
        other

  /**
    * Unify grouping keys
    */
  private def unifyGroupingKeys(
      keyLists: List[List[GroupingKey]],
      ctx: UnificationContext
  ): List[GroupingKey] =
    if keyLists.isEmpty then return Nil
    val sizes = keyLists.map(_.size).distinct
    if sizes.size != 1 then
      throw new CannotUnifyException(s"Different grouping key counts: ${sizes.mkString(", ")}")

    keyLists.head.indices.map { i =>
      val keys = keyLists.map(_(i))
      // GroupingKey contains child Expression
      val childExprs = keys.map(_.child)
      val unified = unifyExprs(childExprs, ctx, s"group_key_$i")
      UnresolvedGroupingKey(keys.head.name, unified, NoSpan)
    }.toList

  /**
    * Unify a list of Expressions
    */
  private def unifyExprs(exprs: List[Expression], ctx: UnificationContext, position: String): Expression =
    val exprTypes = exprs.map(_.getClass).distinct
    if exprTypes.size != 1 then
      // Different expression types - cannot unify structurally, parameterize
      val paramId = ctx.createParameter(exprs, position)
      return NamedParameter(paramId.stripPrefix("$"), NoSpan)

    exprs.head match
      // === Literals ===
      case _: Literal =>
        val literals = exprs.map(_.asInstanceOf[Literal])
        val values   = literals.map(_.stringValue)
        if values.distinct.size == 1 then
          literals.head
        else
          val paramId = ctx.createParameter(values, position)
          NamedParameter(paramId.stripPrefix("$"), NoSpan)

      // === Identifiers ===
      case _: Identifier =>
        val ids    = exprs.map(_.asInstanceOf[Identifier])
        val values = ids.map(_.unquotedValue)
        if values.distinct.size == 1 then
          ids.head
        else
          val paramId = ctx.createParameter(values, position)
          NamedParameter(paramId.stripPrefix("$"), NoSpan)

      case _: QualifiedName =>
        val qnames = exprs.map(_.asInstanceOf[QualifiedName])
        val values = qnames.map(_.fullName)
        if values.distinct.size == 1 then
          qnames.head
        else
          val paramId = ctx.createParameter(values, position)
          NamedParameter(paramId.stripPrefix("$"), NoSpan)

      // === Binary expressions ===
      case _: Eq =>
        val eqs   = exprs.map(_.asInstanceOf[Eq])
        val left  = unifyExprs(eqs.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(eqs.map(_.right), ctx, s"${position}_right")
        Eq(left, right, NoSpan)

      case _: NotEq =>
        val neqs  = exprs.map(_.asInstanceOf[NotEq])
        val left  = unifyExprs(neqs.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(neqs.map(_.right), ctx, s"${position}_right")
        NotEq(left, right, NoSpan)

      case _: LessThan =>
        val lts   = exprs.map(_.asInstanceOf[LessThan])
        val left  = unifyExprs(lts.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(lts.map(_.right), ctx, s"${position}_right")
        LessThan(left, right, NoSpan)

      case _: LessThanOrEq =>
        val ltes  = exprs.map(_.asInstanceOf[LessThanOrEq])
        val left  = unifyExprs(ltes.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(ltes.map(_.right), ctx, s"${position}_right")
        LessThanOrEq(left, right, NoSpan)

      case _: GreaterThan =>
        val gts   = exprs.map(_.asInstanceOf[GreaterThan])
        val left  = unifyExprs(gts.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(gts.map(_.right), ctx, s"${position}_right")
        GreaterThan(left, right, NoSpan)

      case _: GreaterThanOrEq =>
        val gtes  = exprs.map(_.asInstanceOf[GreaterThanOrEq])
        val left  = unifyExprs(gtes.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(gtes.map(_.right), ctx, s"${position}_right")
        GreaterThanOrEq(left, right, NoSpan)

      case _: And =>
        val ands  = exprs.map(_.asInstanceOf[And])
        val left  = unifyExprs(ands.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(ands.map(_.right), ctx, s"${position}_right")
        And(left, right, NoSpan)

      case _: Or =>
        val ors   = exprs.map(_.asInstanceOf[Or])
        val left  = unifyExprs(ors.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(ors.map(_.right), ctx, s"${position}_right")
        Or(left, right, NoSpan)

      case _: ArithmeticBinaryExpr =>
        val ariths = exprs.map(_.asInstanceOf[ArithmeticBinaryExpr])
        val exprTypes = ariths.map(_.exprType).distinct
        if exprTypes.size != 1 then
          throw new CannotUnifyException(s"Different arithmetic types: ${exprTypes.mkString(", ")}")
        val left  = unifyExprs(ariths.map(_.left), ctx, s"${position}_left")
        val right = unifyExprs(ariths.map(_.right), ctx, s"${position}_right")
        ArithmeticBinaryExpr(exprTypes.head, left, right, NoSpan)

      // === Unary expressions ===
      case _: Not =>
        val nots  = exprs.map(_.asInstanceOf[Not])
        val child = unifyExprs(nots.map(_.child), ctx, s"${position}_child")
        Not(child, NoSpan)

      case _: IsNull =>
        val nulls = exprs.map(_.asInstanceOf[IsNull])
        val child = unifyExprs(nulls.map(_.child), ctx, s"${position}_child")
        IsNull(child, NoSpan)

      case _: IsNotNull =>
        val notnulls = exprs.map(_.asInstanceOf[IsNotNull])
        val child    = unifyExprs(notnulls.map(_.child), ctx, s"${position}_child")
        IsNotNull(child, NoSpan)

      case _: ParenthesizedExpression =>
        val parens = exprs.map(_.asInstanceOf[ParenthesizedExpression])
        val child  = unifyExprs(parens.map(_.child), ctx, s"${position}_child")
        ParenthesizedExpression(child, NoSpan)

      // === Function calls ===
      case _: FunctionApply =>
        val calls = exprs.map(_.asInstanceOf[FunctionApply])
        val bases = calls.map(_.base)
        // Check if bases are the same (function name)
        val baseTypes = bases.map(_.getClass).distinct
        if baseTypes.size != 1 then
          throw new CannotUnifyException(s"Different function base types: ${baseTypes.map(_.getSimpleName).mkString(", ")}")
        val unifiedBase = unifyExprs(bases, ctx, s"${position}_base")
        val args = unifyFunctionArgs(calls.map(_.args), ctx, position)
        FunctionApply(unifiedBase, args, calls.head.window, calls.head.filter, calls.head.columnAliases, NoSpan)

      // === Other expressions - return first ===
      case other =>
        debug(s"Unifying unknown expression type: ${other.getClass.getSimpleName}")
        exprs.head

  /**
    * Unify function arguments
    */
  private def unifyFunctionArgs(
      argsLists: List[List[FunctionArg]],
      ctx: UnificationContext,
      position: String
  ): List[FunctionArg] =
    val sizes = argsLists.map(_.size).distinct
    if sizes.size != 1 then
      throw new CannotUnifyException(s"Different argument counts: ${sizes.mkString(", ")}")
    
    if argsLists.head.isEmpty then return Nil
    
    argsLists.transpose.zipWithIndex.map { case (args, idx) =>
      val values = args.map(_.value)
      val unified = unifyExprs(values, ctx, s"${position}_arg_$idx")
      FunctionArg(args.head.name, unified, args.head.isDistinct, args.head.orderBy, NoSpan)
    }

  /**
    * Unify optional expressions
    */
  private def unifyOptionExprs(
      exprs: List[Option[Expression]],
      ctx: UnificationContext,
      position: String
  ): Option[Expression] =
    if exprs.forall(_.isEmpty) then None
    else if exprs.forall(_.isDefined) then
      Some(unifyExprs(exprs.flatten, ctx, position))
    else
      throw new CannotUnifyException("Inconsistent optional expressions")

  /**
    * Unify lists of expressions
    */
  private def unifyExprLists(
      lists: List[List[Expression]],
      ctx: UnificationContext,
      position: String
  ): List[Expression] =
    val sizes = lists.map(_.size).distinct
    if sizes.size != 1 then
      throw new CannotUnifyException(s"Different list sizes: ${sizes.mkString(", ")}")

    lists.transpose.zipWithIndex.map { case (exprs, idx) =>
      unifyExprs(exprs, ctx, s"${position}_$idx")
    }

  /**
    * Unify select items (attributes)
    */
  private def unifySelectItems(
      items: List[List[Attribute]],
      ctx: UnificationContext
  ): List[Attribute] =
    val sizes = items.map(_.size).distinct
    if sizes.size != 1 then
      throw new CannotUnifyException(s"Different select item counts: ${sizes.mkString(", ")}")

    items.transpose.zipWithIndex.map { case (attrs, idx) =>
      unifyAttribute(attrs, ctx, s"select_$idx")
    }

  /**
    * Unify attributes
    */
  private def unifyAttribute(
      attrs: List[Attribute],
      ctx: UnificationContext,
      position: String
  ): Attribute =
    attrs.head match
      case _: SingleColumn =>
        val cols  = attrs.map(_.asInstanceOf[SingleColumn])
        val expr  = unifyExprs(cols.map(_.expr), ctx, position)
        // Keep first nameExpr
        SingleColumn(cols.head.nameExpr, expr, NoSpan)
      case _: AllColumns =>
        attrs.head.asInstanceOf[AllColumns]
      case other =>
        unifyExprs(attrs, ctx, position).asInstanceOf[Attribute]

  /**
    * Unify sort items
    */
  private def unifySortItems(
      items: List[List[SortItem]],
      ctx: UnificationContext
  ): List[SortItem] =
    val sizes = items.map(_.size).distinct
    if sizes.size != 1 then
      throw new CannotUnifyException(s"Different sort item counts: ${sizes.mkString(", ")}")

    items.transpose.zipWithIndex.map { case (sortItems, idx) =>
      val keys = sortItems.map(_.sortKey)
      val key  = unifyExprs(keys, ctx, s"sort_key_$idx")
      SortItem(key, sortItems.head.ordering, sortItems.head.nullOrdering, NoSpan)
    }

  /**
    * Infer type from a list of values
    */
  private def inferType(values: List[Any]): DataType =
    val types = values.map {
      case _: String     => DataType.StringType
      case _: Int        => DataType.IntType
      case _: Long       => DataType.LongType
      case _: Double     => DataType.DoubleType
      case _: Float      => DataType.FloatType
      case _: Boolean    => DataType.BooleanType
      case _: Identifier => DataType.StringType
      case _: Literal    => DataType.StringType
      case _             => DataType.AnyType
    }
    val distinct = types.distinct
    if distinct.size == 1 then distinct.head
    else DataType.AnyType

end AntiUnifier
