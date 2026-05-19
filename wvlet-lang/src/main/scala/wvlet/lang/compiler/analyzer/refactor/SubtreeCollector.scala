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
import wvlet.lang.model.expr.{Expression, SubQueryExpression}
import wvlet.log.LogSupport

import scala.collection.mutable.ListBuffer

/**
  * Represents a collected subtree with its metadata
  *
  * @param root
  *   The root node of the subtree
  * @param depth
  *   The depth of the subtree (number of nodes from root to deepest leaf)
  * @param nodeCount
  *   The total number of nodes in the subtree
  * @param structuralHash
  *   The structural hash of the subtree
  * @param path
  *   The path from the original root to this subtree (indices of children)
  * @param sourceId
  *   Optional identifier of the source (e.g., query ID for cross-query analysis)
  */
case class CollectedSubtree(
    root: LogicalPlan,
    depth: Int,
    nodeCount: Int,
    structuralHash: Int,
    path: List[Int],
    sourceId: Option[String] = None
):
  /**
    * Check if this subtree is a descendant of another subtree
    */
  def isDescendantOf(other: CollectedSubtree): Boolean =
    path.length > other.path.length && path.startsWith(other.path)

  /**
    * Check if this subtree is an ancestor of another subtree
    */
  def isAncestorOf(other: CollectedSubtree): Boolean =
    path.length < other.path.length && other.path.startsWith(path)

end CollectedSubtree

/**
  * Lightweight metadata for a subtree occurrence.
  * Unlike CollectedSubtree, this does NOT hold a reference to the LogicalPlan root,
  * allowing the original AST to be garbage-collected immediately after traversal.
  * Used for memory-efficient global pattern extraction (streaming mode).
  */
case class SubtreeMetadata(
    structuralHash: Int,
    nodeType:       String,
    nodeCount:      Int,
    path:           List[Int],
    sourceId:       Option[String] = None
):
  def isDescendantOf(other: SubtreeMetadata): Boolean =
    path.length > other.path.length && path.startsWith(other.path)
end SubtreeMetadata

/**
  * Configuration for subtree collection
  *
  * @param minDepth
  *   Minimum depth of subtrees to collect
  * @param minNodeCount
  *   Minimum number of nodes in subtrees to collect
  * @param maxDepth
  *   Maximum depth to traverse (0 = unlimited)
  * @param includeLeafNodes
  *   Whether to include leaf nodes (tables, empty relations)
  * @param hashConfig
  *   Configuration for structural hashing
  */
case class CollectorConfig(
    minDepth: Int = 2,
    minNodeCount: Int = 3,
    maxDepth: Int = 0,
    includeLeafNodes: Boolean = false,
    hashConfig: HashConfig = HashConfig.default
)

object CollectorConfig:
  val default: CollectorConfig = CollectorConfig()

  // For collecting all subtrees
  val all: CollectorConfig = CollectorConfig(
    minDepth = 1,
    minNodeCount = 1,
    includeLeafNodes = true
  )

/**
  * Collects subtrees from LogicalPlan trees for duplicate detection
  */
object SubtreeCollector extends LogSupport:

  /**
    * Trie for checking whether a path has any ancestor path that is already registered.
    *
    * This supports fast "isDescendantOf" checks:
    *   - Insert ancestor paths
    *   - For a candidate path, if we encounter a terminal node while traversing,
    *     then the candidate has an ancestor in the set.
    */
  private class PathTrie:
    private class Node:
      var terminal: Boolean = false
      val children          = scala.collection.mutable.HashMap.empty[Int, Node]

    private val root = new Node

    def hasAncestor(path: List[Int]): Boolean =
      var node = root
      // We only care about strict descendants, so a terminal at any prefix implies overlap.
      // (Equal path is not a descendant; we check terminal before consuming the next element.)
      path.foreach { idx =>
        if node.terminal then
          return true
        node = node.children.getOrElseUpdate(idx, new Node)
      }
      false

    def add(path: List[Int]): Unit =
      var node = root
      path.foreach { idx =>
        node = node.children.getOrElseUpdate(idx, new Node)
      }
      node.terminal = true

  /**
    * Collect all subtrees from a single LogicalPlan
    *
    * @param plan
    *   The root plan to collect subtrees from
    * @param config
    *   Collection configuration
    * @param sourceId
    *   Optional source identifier
    * @return
    *   List of collected subtrees
    */
  def collect(
      plan: LogicalPlan,
      config: CollectorConfig = CollectorConfig.default,
      sourceId: Option[String] = None
  ): List[CollectedSubtree] =
    val result = ListBuffer[CollectedSubtree]()
    traverse(plan, 0, Nil, config, sourceId, result)
    result.toList

  /**
    * Collect subtrees from multiple LogicalPlans (for cross-query analysis)
    *
    * @param plans
    *   List of (sourceId, plan) pairs
    * @param config
    *   Collection configuration
    * @return
    *   List of collected subtrees from all plans
    */
  def collectFromMultiple(
      plans: List[(String, LogicalPlan)],
      config: CollectorConfig = CollectorConfig.default
  ): List[CollectedSubtree] =
    plans.flatMap { case (sourceId, plan) =>
      collect(plan, config, Some(sourceId))
    }

  private def traverse(
      node: LogicalPlan,
      currentDepth: Int,
      path: List[Int],
      config: CollectorConfig,
      sourceId: Option[String],
      result: ListBuffer[CollectedSubtree]
  ): (Int, Int) = // Returns (depth, nodeCount)
    // Check max depth limit
    if config.maxDepth > 0 && currentDepth > config.maxDepth then
      return (0, 0)

    // Calculate depth and node count for this subtree
    val childResults = node.children.zipWithIndex.map { case (child, idx) =>
      traverse(child, currentDepth + 1, path :+ idx, config, sourceId, result)
    }

    // Also traverse SubQueryExpression's inner queries (e.g., HAVING subqueries)
    // These are separate logical plans embedded in expressions, not in node.children
    val subQueryExprs = collectSubQueryExpressions(node)
    subQueryExprs.zipWithIndex.foreach { case (sq, sqIdx) =>
      // Use offset 1000 to avoid path collision with regular child indices
      traverse(sq.query, currentDepth + 1, path :+ (1000 + sqIdx), config, sourceId, result)
    }

    val depth =
      if childResults.isEmpty then 1
      else childResults.map(_._1).max + 1

    val nodeCount =
      if childResults.isEmpty then 1
      else childResults.map(_._2).sum + 1

    // Decide whether to collect this subtree
    val shouldCollect =
      isRefactorableNode(node, config) &&
        depth >= config.minDepth &&
        nodeCount >= config.minNodeCount

    if shouldCollect then
      val hash = StructuralHasher.hash(node, config.hashConfig)
      result += CollectedSubtree(
        root = node,
        depth = depth,
        nodeCount = nodeCount,
        structuralHash = hash,
        path = path,
        sourceId = sourceId
      )

    (depth, nodeCount)

  /**
    * Determine if a node is a candidate for refactoring
    */
  private def isRefactorableNode(node: LogicalPlan, config: CollectorConfig): Boolean =
    node match
      // Always refactorable relation nodes
      case _: Filter | _: Project | _: Join | _: GroupBy | _: Agg | _: Sort | _: Distinct |
          _: Union | _: Intersect | _: Except | _: Limit | _: Pivot | _: Concat | _: Dedup =>
        true

      // Leaf nodes (configurable)
      case _: TableRef | _: TableScan | _: FileScan | _: EmptyRelation | _: Values =>
        config.includeLeafNodes

      // Aliased relations
      case _: AliasedRelation | _: NamedRelation =>
        true

      // Model references and CTE/WithQuery
      case _: ModelScan | _: WithQuery =>
        true
        
      // Write operations (Append, Save, etc.) - collect the child query for pattern analysis
      case _: Save | _: AppendTo =>
        true

      // Other nodes - don't collect
      case _ =>
        false

  /**
    * Group subtrees by their structural hash
    *
    * @param subtrees
    *   List of collected subtrees
    * @param minOccurrences
    *   Minimum number of occurrences to include in result
    * @return
    *   Map from structural hash to list of subtrees
    */
  def groupByHash(
      subtrees: List[CollectedSubtree],
      minOccurrences: Int = 2
  ): Map[Int, List[CollectedSubtree]] =
    subtrees
      .groupBy(_.structuralHash)
      .filter { case (_, group) =>
        group.size >= minOccurrences
      }

  /**
    * Filter to keep only subtrees from different sources (for cross-query analysis)
    */
  def filterCrossSource(groups: Map[Int, List[CollectedSubtree]]): Map[Int, List[CollectedSubtree]] =
    groups.filter { case (_, subtrees) =>
      subtrees.map(_.sourceId).distinct.size >= 2
    }

  /**
    * Remove subtrees that are descendants of other subtrees in the same group
    *
    * This helps avoid extracting patterns that are parts of larger patterns
    */
  def removeOverlapping(subtrees: List[CollectedSubtree]): List[CollectedSubtree] =
    // Overlap (ancestor/descendant) can only happen within the same source plan.
    // In cross-query analysis, grouping by sourceId avoids quadratic blowups in huge hash groups.
    val bySource = subtrees.groupBy(_.sourceId)
    bySource.valuesIterator.flatMap(removeOverlappingWithinSource).toList

  private def removeOverlappingWithinSource(subtrees: List[CollectedSubtree]): List[CollectedSubtree] =
    if subtrees.size <= 1 then
      subtrees
    else
      // Shorter paths are ancestors of longer ones. Process ancestors first.
      val sorted = subtrees.sortBy(_.path.length)
      val trie   = new PathTrie
      val out    = ListBuffer.empty[CollectedSubtree]

      sorted.foreach { s =>
        if trie.hasAncestor(s.path) then
          ()
        else
          out += s
          trie.add(s.path)
      }
      out.toList

  /**
    * Print statistics about collected subtrees
    */
  def printStats(subtrees: List[CollectedSubtree]): Unit =
    debug(s"Collected ${subtrees.size} subtrees")
    debug(s"  - Average depth: ${subtrees.map(_.depth).sum.toDouble / subtrees.size}")
    debug(s"  - Average node count: ${subtrees.map(_.nodeCount).sum.toDouble / subtrees.size}")
    debug(s"  - Unique hashes: ${subtrees.map(_.structuralHash).distinct.size}")
    debug(s"  - Sources: ${subtrees.flatMap(_.sourceId).distinct.size}")

  /**
    * Collect SubQueryExpression instances from a LogicalPlan node's expressions.
    * This finds subqueries embedded in expressions (e.g., HAVING clause subqueries)
    * that are not reachable via node.children.
    */
  private def collectSubQueryExpressions(node: LogicalPlan): List[SubQueryExpression] =
    def findInExpr(expr: Expression): List[SubQueryExpression] =
      expr match
        case sq: SubQueryExpression => sq :: Nil
        case _ => expr.children.flatMap(findInExpr).toList
    node.childExpressions.flatMap(findInExpr).toList

  // ---------------------------------------------------------------------------
  // Streaming (memory-efficient) metadata-only collection
  // ---------------------------------------------------------------------------

  /**
    * Stream-friendly metadata collection. Traverses `plan` once, emitting SubtreeMetadata
    * entries into `acc`. The LogicalPlan is NOT retained anywhere in `acc` — drop your
    * reference to `plan` after calling this method and the AST is eligible for GC.
    *
    * @param plan     The root plan to traverse
    * @param config   Collection configuration
    * @param sourceId Optional source identifier (e.g. filename)
    * @param acc      Global accumulator: structuralHash → occurrences
    */
  def collectMetadataInto(
      plan:     LogicalPlan,
      config:   CollectorConfig = CollectorConfig.default,
      sourceId: Option[String]  = None,
      acc:      scala.collection.mutable.HashMap[Int, scala.collection.mutable.ListBuffer[SubtreeMetadata]]
  ): Unit =
    traverseMetadata(plan, 0, Nil, config, sourceId, acc)
    // `plan` reference is not captured anywhere in `acc` — eligible for GC

  private def traverseMetadata(
      node:         LogicalPlan,
      currentDepth: Int,
      path:         List[Int],
      config:       CollectorConfig,
      sourceId:     Option[String],
      acc:          scala.collection.mutable.HashMap[Int, scala.collection.mutable.ListBuffer[SubtreeMetadata]]
  ): (Int, Int) = // Returns (depth, nodeCount)
    if config.maxDepth > 0 && currentDepth > config.maxDepth then
      return (0, 0)

    val childResults = node.children.zipWithIndex.map { case (child, idx) =>
      traverseMetadata(child, currentDepth + 1, path :+ idx, config, sourceId, acc)
    }

    // Also traverse SubQueryExpression's inner queries (same logic as traverse)
    val subQueryExprs = collectSubQueryExpressions(node)
    subQueryExprs.zipWithIndex.foreach { case (sq, sqIdx) =>
      traverseMetadata(sq.query, currentDepth + 1, path :+ (1000 + sqIdx), config, sourceId, acc)
    }

    val depth =
      if childResults.isEmpty then 1
      else childResults.map(_._1).max + 1

    val nodeCount =
      if childResults.isEmpty then 1
      else childResults.map(_._2).sum + 1

    val shouldCollect =
      isRefactorableNode(node, config) &&
        depth >= config.minDepth &&
        nodeCount >= config.minNodeCount

    if shouldCollect then
      val hash = StructuralHasher.hash(node, config.hashConfig)
      val meta = SubtreeMetadata(
        structuralHash = hash,
        nodeType       = node.getClass.getSimpleName.toLowerCase,
        nodeCount      = nodeCount,
        path           = path,
        sourceId       = sourceId
      )
      acc.getOrElseUpdate(hash, scala.collection.mutable.ListBuffer.empty) += meta
      // `node` is NOT captured anywhere in `meta` — eligible for GC after this point

    (depth, nodeCount)

  /**
    * Remove SubtreeMetadata entries that are descendants of another entry from the same source.
    * Mirrors removeOverlapping for CollectedSubtree.
    */
  def removeMetadataOverlapping(metas: List[SubtreeMetadata]): List[SubtreeMetadata] =
    val bySource = metas.groupBy(_.sourceId)
    bySource.valuesIterator.flatMap(removeMetadataOverlappingMetaWithinSource).toList

  private def removeMetadataOverlappingMetaWithinSource(metas: List[SubtreeMetadata]): List[SubtreeMetadata] =
    if metas.size <= 1 then
      metas
    else
      val sorted = metas.sortBy(_.path.length)
      val trie   = new PathTrie
      val out    = ListBuffer.empty[SubtreeMetadata]
      sorted.foreach { s =>
        if trie.hasAncestor(s.path) then
          ()
        else
          out += s
          trie.add(s.path)
      }
      out.toList

  /**
    * Navigate to the subtree at the given path in a LogicalPlan tree.
    *
    * Returns None if the path is invalid (out-of-bounds index or missing subquery expression).
    * Mirrors the path encoding used by [[traverse]] and [[traverseMetadata]]:
    *   - Indices 0..N-1 address the N children of the node.
    *   - Indices >= 1000 address SubQueryExpression inner queries (1000+sqIdx).
    *
    * @param plan The root plan to navigate from.
    * @param path The path to follow (empty = return root).
    * @return The subtree at the path, or None if the path is invalid.
    */
  def navigatePath(plan: LogicalPlan, path: List[Int]): Option[LogicalPlan] =
    path match
      case Nil => Some(plan)
      case head :: tail =>
        if head >= 1000 then
          val sqIdx        = head - 1000
          val subQueryExprs = collectSubQueryExpressions(plan)
          if sqIdx >= 0 && sqIdx < subQueryExprs.size then
            navigatePath(subQueryExprs(sqIdx).query, tail)
          else
            None
        else
          val children = plan.children
          if head >= 0 && head < children.size then
            navigatePath(children(head), tail)
          else
            None

end SubtreeCollector
