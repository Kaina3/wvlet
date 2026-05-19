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

/**
  * Represents a group of duplicate subtrees
  *
  * @param structuralHash
  *   The structural hash shared by all subtrees
  * @param subtrees
  *   The list of structurally similar subtrees
  * @param nodeCount
  *   Average node count of subtrees in this group
  * @param depth
  *   Average depth of subtrees in this group
  */
case class DuplicateGroup(
    structuralHash: Int,
    subtrees: List[CollectedSubtree],
    nodeCount: Double,
    depth: Double
):
  /**
    * Number of occurrences
    */
  def occurrences: Int = subtrees.size

  /**
    * Total nodes that could be saved by refactoring (approximate)
    */
  def potentialSaving: Double = nodeCount * (occurrences - 1)

  /**
    * Sources involved in this duplicate group
    */
  def sources: Set[String] = subtrees.flatMap(_.sourceId).toSet

  /**
    * Whether this is a cross-query duplicate (appears in multiple queries)
    */
  def isCrossQuery: Boolean = sources.size > 1

  /**
    * Get sample subtree for pattern analysis
    */
  def sample: CollectedSubtree = subtrees.head

  /**
    * Get all root plans
    */
  def plans: List[LogicalPlan] = subtrees.map(_.root)

end DuplicateGroup

/**
  * Result of duplicate detection
  *
  * @param groups
  *   List of duplicate groups
  * @param totalSubtrees
  *   Total number of subtrees analyzed
  * @param uniqueHashes
  *   Number of unique structural hashes
  */
case class DuplicateDetectionResult(
    groups: List[DuplicateGroup],
    totalSubtrees: Int,
    uniqueHashes: Int
):
  /**
    * Get groups sorted by potential saving (highest first)
    */
  def byPotentialSaving: List[DuplicateGroup] = groups.sortBy(-_.potentialSaving)

  /**
    * Get groups sorted by occurrences (most frequent first)
    */
  def byOccurrences: List[DuplicateGroup] = groups.sortBy(-_.occurrences)

  /**
    * Get groups sorted by node count (largest first)
    */
  def byNodeCount: List[DuplicateGroup] = groups.sortBy(-_.nodeCount)

  /**
    * Get only cross-query duplicates
    */
  def crossQueryOnly: DuplicateDetectionResult = copy(groups = groups.filter(_.isCrossQuery))

  /**
    * Get only single-query duplicates (repeated within same query)
    */
  def singleQueryOnly: DuplicateDetectionResult = copy(groups = groups.filterNot(_.isCrossQuery))

  /**
    * Summary statistics
    */
  def summary: String =
    s"""Duplicate Detection Summary:
       |  Total subtrees analyzed: $totalSubtrees
       |  Unique structural patterns: $uniqueHashes
       |  Duplicate groups found: ${groups.size}
       |  Cross-query groups: ${groups.count(_.isCrossQuery)}
       |  Single-query groups: ${groups.count(!_.isCrossQuery)}
       |  Total potential node savings: ${groups.map(_.potentialSaving).sum.toInt}
       |""".stripMargin

end DuplicateDetectionResult

/**
  * Configuration for duplicate detection
  *
  * @param minOccurrences
  *   Minimum number of occurrences to consider as duplicate
  * @param minNodeCount
  *   Minimum node count for subtrees to consider
  * @param minDepth
  *   Minimum depth for subtrees to consider
  * @param requireCrossQuery
  *   If true, only detect duplicates across different queries
  * @param removeOverlapping
  *   If true, remove subtrees that are part of larger duplicate patterns
  */
case class DetectorConfig(
    minOccurrences: Int = 2,
    minNodeCount: Int = 3,
    minDepth: Int = 2,
    requireCrossQuery: Boolean = false,
    removeOverlapping: Boolean = true
)

object DetectorConfig:
  val default: DetectorConfig = DetectorConfig()

  // For cross-query analysis
  val crossQuery: DetectorConfig = DetectorConfig(requireCrossQuery = true)

  // For aggressive detection (smaller patterns)
  val aggressive: DetectorConfig = DetectorConfig(
    minOccurrences = 2,
    minNodeCount = 2,
    minDepth = 1
  )

/**
  * Detects duplicate patterns in LogicalPlan trees
  */
object DuplicateDetector extends LogSupport:

  /**
    * Detect duplicates in a single LogicalPlan
    *
    * @param plan
    *   The plan to analyze
    * @param config
    *   Detection configuration
    * @param collectorConfig
    *   Configuration for subtree collection
    * @return
    *   Detection result
    */
  def detect(
      plan: LogicalPlan,
      config: DetectorConfig = DetectorConfig.default,
      collectorConfig: CollectorConfig = CollectorConfig.default
  ): DuplicateDetectionResult =
    val subtrees = SubtreeCollector.collect(plan, collectorConfig)
    detectFromSubtrees(subtrees, config)

  /**
    * Detect duplicates across multiple LogicalPlans
    *
    * @param plans
    *   List of (sourceId, plan) pairs
    * @param config
    *   Detection configuration
    * @param collectorConfig
    *   Configuration for subtree collection
    * @return
    *   Detection result
    */
  def detectAcross(
      plans: List[(String, LogicalPlan)],
      config: DetectorConfig = DetectorConfig.default,
      collectorConfig: CollectorConfig = CollectorConfig.default
  ): DuplicateDetectionResult =
    val t0 = System.currentTimeMillis()
    val subtrees = SubtreeCollector.collectFromMultiple(plans, collectorConfig)
    val t1 = System.currentTimeMillis()
    println(s"    [DuplicateDetector] Subtree collection: ${t1 - t0}ms (${subtrees.size} subtrees)")
    detectFromSubtrees(subtrees, config)

  /**
    * Detect duplicates from pre-collected subtrees
    */
  def detectFromSubtrees(
      subtrees: List[CollectedSubtree],
      config: DetectorConfig
  ): DuplicateDetectionResult =
    // Filter by minimum criteria
    val t0 = System.currentTimeMillis()
    val filtered = subtrees.filter { s =>
      s.nodeCount >= config.minNodeCount && s.depth >= config.minDepth
    }
    val t1 = System.currentTimeMillis()
    println(s"    [DuplicateDetector] Filtering: ${t1 - t0}ms (${filtered.size} remain)")

    // Group by structural hash
    val t2 = System.currentTimeMillis()
    val grouped = SubtreeCollector.groupByHash(filtered, config.minOccurrences)
    val t3 = System.currentTimeMillis()
    println(s"    [DuplicateDetector] Hash grouping: ${t3 - t2}ms (${grouped.size} groups)")

    // Apply cross-query filter if required
    val t4 = System.currentTimeMillis()
    val filtered2 =
      if config.requireCrossQuery then SubtreeCollector.filterCrossSource(grouped)
      else grouped
    val t5 = System.currentTimeMillis()
    if config.requireCrossQuery then
      println(s"    [DuplicateDetector] Cross-query filter: ${t5 - t4}ms (${filtered2.size} groups)")

    // Remove overlapping subtrees within each group
    val t6 = System.currentTimeMillis()
    val cleaned =
      if config.removeOverlapping then
        filtered2.map { case (hash, subs) =>
          hash -> SubtreeCollector.removeOverlapping(subs)
        }.filter(_._2.size >= config.minOccurrences)
      else
        filtered2
    val t7 = System.currentTimeMillis()
    println(s"    [DuplicateDetector] Remove overlapping: ${t7 - t6}ms (${cleaned.size} groups)")

    // Create duplicate groups
    val t8 = System.currentTimeMillis()
    val groups = cleaned.map { case (hash, subs) =>
      DuplicateGroup(
        structuralHash = hash,
        subtrees = subs,
        nodeCount = subs.map(_.nodeCount).sum.toDouble / subs.size,
        depth = subs.map(_.depth).sum.toDouble / subs.size
      )
    }.toList
    val t9 = System.currentTimeMillis()
    println(s"    [DuplicateDetector] Group creation: ${t9 - t8}ms")

    DuplicateDetectionResult(
      groups = groups,
      totalSubtrees = subtrees.size,
      uniqueHashes = subtrees.map(_.structuralHash).distinct.size
    )

  /**
    * Find the most significant duplicate groups
    *
    * @param result
    *   Detection result
    * @param topN
    *   Number of top groups to return
    * @return
    *   Top N groups by potential saving
    */
  def findTopDuplicates(result: DuplicateDetectionResult, topN: Int = 10): List[DuplicateGroup] =
    result.byPotentialSaving.take(topN)

  /**
    * Pretty print duplicate groups for debugging/analysis
    */
  def printGroups(groups: List[DuplicateGroup]): Unit =
    groups.zipWithIndex.foreach { case (group, idx) =>
      info(s"""
        |Group ${idx + 1}:
        |  Hash: ${group.structuralHash}
        |  Occurrences: ${group.occurrences}
        |  Avg node count: ${group.nodeCount}
        |  Avg depth: ${group.depth}
        |  Potential saving: ${group.potentialSaving}
        |  Sources: ${group.sources.mkString(", ")}
        |  Sample structure: ${summarizePlan(group.sample.root)}
        |""".stripMargin)
    }

  /**
    * Create a brief summary of a plan structure
    */
  private def summarizePlan(plan: LogicalPlan, maxDepth: Int = 3): String =
    def summarize(p: LogicalPlan, depth: Int): String =
      if depth > maxDepth then "..."
      else
        val name = p.getClass.getSimpleName
        val children = p.children.map(c => summarize(c, depth + 1)).mkString(", ")
        if children.isEmpty then name
        else s"$name($children)"
    summarize(plan, 0)

end DuplicateDetector
