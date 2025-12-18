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
          _: Union | _: Intersect | _: Except | _: Limit | _: Pivot =>
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
    subtrees.filterNot { subtree =>
      subtrees.exists(other => subtree != other && subtree.isDescendantOf(other))
    }

  /**
    * Print statistics about collected subtrees
    */
  def printStats(subtrees: List[CollectedSubtree]): Unit =
    debug(s"Collected ${subtrees.size} subtrees")
    debug(s"  - Average depth: ${subtrees.map(_.depth).sum.toDouble / subtrees.size}")
    debug(s"  - Average node count: ${subtrees.map(_.nodeCount).sum.toDouble / subtrees.size}")
    debug(s"  - Unique hashes: ${subtrees.map(_.structuralHash).distinct.size}")
    debug(s"  - Sources: ${subtrees.flatMap(_.sourceId).distinct.size}")

end SubtreeCollector
