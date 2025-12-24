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

import wvlet.log.LogSupport

/**
  * Configuration for refactoring decisions
  *
  * @param minOccurrences
  *   Minimum number of occurrences to trigger refactoring
  * @param minPatternSize
  *   Minimum number of nodes in the pattern
  * @param maxParameters
  *   Maximum number of parameters allowed in the model
  * @param minReduction
  *   Minimum node reduction to justify refactoring
  * @param minReductionRatio
  *   Minimum reduction ratio (0.0 to 1.0)
  * @param preferCrossQuery
  *   Give higher priority to cross-query patterns
  */
case class RefactorConfig(
    minOccurrences: Int = 2,
    minPatternSize: Int = 3,
    maxParameters: Int = 5,
    minReduction: Int = 10,
    minReductionRatio: Double = 0.3,
    preferCrossQuery: Boolean = true
)

object RefactorConfig:
  val default: RefactorConfig = RefactorConfig()

  // More aggressive refactoring
  val aggressive: RefactorConfig = RefactorConfig(
    minOccurrences = 2,
    minPatternSize = 2,
    maxParameters = 8,
    minReduction = 5,
    minReductionRatio = 0.2
  )

  // Conservative refactoring (only significant patterns)
  val conservative: RefactorConfig = RefactorConfig(
    minOccurrences = 3,
    minPatternSize = 5,
    maxParameters = 3,
    minReduction = 20,
    minReductionRatio = 0.5
  )

/**
  * Represents a refactoring decision
  *
  * @param shouldRefactor
  *   Whether the pattern should be refactored
  * @param reason
  *   Human-readable explanation of the decision
  * @param score
  *   Numeric score for ranking (higher = better candidate)
  * @param estimatedReduction
  *   Estimated node reduction if refactored
  * @param warnings
  *   Any warnings about the refactoring
  */
case class RefactoringDecision(
    shouldRefactor: Boolean,
    reason: String,
    score: Double,
    estimatedReduction: Int,
    warnings: List[String] = Nil
):
  /**
    * Check if this is a high-priority refactoring candidate
    */
  def isHighPriority: Boolean = shouldRefactor && score > 0.8

  /**
    * Summary string
    */
  def summary: String =
    val status = if shouldRefactor then "✓ REFACTOR" else "✗ SKIP"
    val warningStr = if warnings.nonEmpty then s" ⚠ ${warnings.mkString(", ")}" else ""
    s"$status (score: ${f"$score%.2f"}, reduction: $estimatedReduction) - $reason$warningStr"

end RefactoringDecision

/**
  * Represents a ranked refactoring suggestion
  *
  * @param group
  *   The duplicate group
  * @param unifyResult
  *   Result of anti-unification (if successful)
  * @param decision
  *   The refactoring decision
  * @param rank
  *   Rank among all suggestions (1 = best)
  * @param subsumedBy
  *   If this suggestion is contained within another suggestion (parent hash)
  * @param subsumes
  *   List of suggestion hashes that this suggestion contains (children)
  */
case class RefactoringSuggestion(
    group: DuplicateGroup,
    unifyResult: Option[AntiUnifyResult],
    decision: RefactoringDecision,
    rank: Int = 0,
    subsumedBy: Option[Int] = None,
    subsumes: List[Int] = Nil
):
  /**
    * Generate a suggested model name
    */
  def suggestedModelName: String =
    val baseName = group.sample.root.getClass.getSimpleName.toLowerCase
    s"${baseName}_pattern_${group.structuralHash.abs % 10000}"

  /**
    * Number of model parameters needed
    */
  def parameterCount: Int = unifyResult.map(_.variableParameters.size).getOrElse(0)

  /**
    * Whether this is an optimal (root-level) suggestion not subsumed by another
    */
  def isOptimal: Boolean = subsumedBy.isEmpty

end RefactoringSuggestion

/**
  * Result of hierarchical suggestion analysis with optimal selections
  *
  * @param allSuggestions
  *   All actionable suggestions with hierarchy info
  * @param optimalSuggestions
  *   Only the optimal (root-level) suggestions for modeling
  * @param totalReductionAll
  *   Total reduction if all suggestions were applied (may double-count)
  * @param totalReductionOptimal
  *   Total reduction from optimal suggestions (no double-counting)
  */
case class HierarchicalSuggestionResult(
    allSuggestions: List[RefactoringSuggestion],
    optimalSuggestions: List[RefactoringSuggestion],
    totalReductionAll: Int,
    totalReductionOptimal: Int
):
  /**
    * Summary of the hierarchical analysis
    */
  def summary: String =
    s"""Hierarchical Analysis:
       |  - Total actionable suggestions: ${allSuggestions.size}
       |  - Optimal suggestions (for modeling): ${optimalSuggestions.size}
       |  - Reduction (all, may overlap): $totalReductionAll nodes
       |  - Reduction (optimal, no overlap): $totalReductionOptimal nodes
       |  - Subsumed suggestions: ${allSuggestions.count(_.subsumedBy.isDefined)}""".stripMargin

end HierarchicalSuggestionResult

/**
  * Decides whether duplicate patterns should be refactored into models
  */
object RefactoringDecider extends LogSupport:

  /**
    * Evaluate a single duplicate group for refactoring
    *
    * @param group
    *   The duplicate group to evaluate
    * @param config
    *   Refactoring configuration
    * @return
    *   (RefactoringDecision, Option[AntiUnifyResult]) - decision and unify result if computed
    */
  def evaluate(group: DuplicateGroup, config: RefactorConfig = RefactorConfig.default): (RefactoringDecision, Option[AntiUnifyResult]) =
    val warnings = scala.collection.mutable.ListBuffer[String]()

    // Check minimum occurrences
    if group.occurrences < config.minOccurrences then
      return (RefactoringDecision(
        shouldRefactor = false,
        reason = s"Occurrences (${group.occurrences}) < minimum (${config.minOccurrences})",
        score = 0.0,
        estimatedReduction = 0
      ), None)

    // Check minimum pattern size
    if group.nodeCount < config.minPatternSize then
      return (RefactoringDecision(
        shouldRefactor = false,
        reason = s"Pattern size (${group.nodeCount.toInt}) < minimum (${config.minPatternSize})",
        score = 0.0,
        estimatedReduction = 0
      ), None)

    // Try anti-unification to get parameter count
    val unifyResult = AntiUnifier.unify(group)
    val paramCount  = unifyResult.map(_.variableParameters.size).getOrElse(0)

    // Check parameter count
    if paramCount > config.maxParameters then
      return (RefactoringDecision(
        shouldRefactor = false,
        reason = s"Too many parameters ($paramCount > ${config.maxParameters})",
        score = 0.0,
        estimatedReduction = 0,
        warnings = List("Consider breaking down the pattern")
      ), unifyResult)

    // Calculate reduction
    val originalNodes   = (group.nodeCount * group.occurrences).toInt
    val modelNodes      = group.nodeCount.toInt  // Model definition
    val callNodes       = group.occurrences * (1 + paramCount)  // Model calls with params
    val refactoredNodes = modelNodes + callNodes
    val reduction       = originalNodes - refactoredNodes
    val reductionRatio  = reduction.toDouble / originalNodes

    // Check minimum reduction
    if reduction < config.minReduction then
      return (RefactoringDecision(
        shouldRefactor = false,
        reason = s"Reduction ($reduction) < minimum (${config.minReduction})",
        score = 0.0,
        estimatedReduction = reduction
      ), unifyResult)

    // Check minimum reduction ratio
    if reductionRatio < config.minReductionRatio then
      return (RefactoringDecision(
        shouldRefactor = false,
        reason = f"Reduction ratio (${reductionRatio * 100}%.1f%%) < minimum (${config.minReductionRatio * 100}%.0f%%)",
        score = 0.0,
        estimatedReduction = reduction
      ), unifyResult)

    // Add warnings for edge cases
    if paramCount == 0 then
      warnings += "Pattern has no parameters - consider using CTE instead"

    if group.isCrossQuery then
      // Good for shared model - no warning needed
      ()
    else if group.occurrences > 10 then
      warnings += "Many occurrences in single query - verify pattern correctness"

    // Calculate score
    val score = calculateScore(group, reduction, reductionRatio, paramCount, config)

    (RefactoringDecision(
      shouldRefactor = true,
      reason = f"${group.occurrences} occurrences, $reduction node reduction (${reductionRatio * 100}%.0f%%)",
      score = score,
      estimatedReduction = reduction,
      warnings = warnings.toList
    ), unifyResult)

  /**
    * Calculate refactoring score for ranking
    */
  private def calculateScore(
      group: DuplicateGroup,
      reduction: Int,
      reductionRatio: Double,
      paramCount: Int,
      config: RefactorConfig
  ): Double =
    var score = 0.0

    // Reduction contributes 40%
    score += math.min(reduction / 100.0, 1.0) * 0.4

    // Reduction ratio contributes 30%
    score += reductionRatio * 0.3

    // Occurrences contribute 20%
    score += math.min(group.occurrences / 10.0, 1.0) * 0.2

    // Fewer parameters is better (contributes 10%)
    val paramScore = 1.0 - (paramCount.toDouble / config.maxParameters)
    score += paramScore * 0.1

    // Bonus for cross-query patterns
    if config.preferCrossQuery && group.isCrossQuery then
      score *= 1.2

    math.min(score, 1.0)

  /**
    * Evaluate all duplicate groups and return ranked suggestions
    *
    * @param result
    *   Detection result containing all duplicate groups
    * @param config
    *   Refactoring configuration
    * @return
    *   List of suggestions, sorted by priority
    */
  def evaluateAll(
      result: DuplicateDetectionResult,
      config: RefactorConfig = RefactorConfig.default
  ): List[RefactoringSuggestion] =
    val suggestions = result.groups.map { group =>
      val (decision, unifyResult) = evaluate(group, config)
      RefactoringSuggestion(group, unifyResult, decision)
    }

    // Sort by score (descending) and filter to actionable items
    val sorted = suggestions
      .filter(_.decision.shouldRefactor)
      .sortBy(-_.decision.score)

    // Assign ranks
    sorted.zipWithIndex.map { case (suggestion, idx) =>
      suggestion.copy(rank = idx + 1)
    }

  /**
    * Get top N refactoring suggestions
    */
  def getTopSuggestions(
      result: DuplicateDetectionResult,
      topN: Int = 5,
      config: RefactorConfig = RefactorConfig.default
  ): List[RefactoringSuggestion] =
    evaluateAll(result, config).take(topN)

  /**
    * Print refactoring suggestions
    */
  def printSuggestions(suggestions: List[RefactoringSuggestion]): Unit =
    if suggestions.isEmpty then
      info("No refactoring suggestions")
      return

    info(s"Found ${suggestions.size} refactoring opportunities:")
    info("")

    suggestions.foreach { s =>
      info(s"""
        |#${s.rank} ${s.suggestedModelName}
        |  ${s.decision.summary}
        |  Occurrences: ${s.group.occurrences}
        |  Parameters: ${s.parameterCount}
        |  Sources: ${s.group.sources.mkString(", ")}
        |""".stripMargin)
    }

  /**
    * Generate a summary report
    */
  def generateReport(
      result: DuplicateDetectionResult,
      config: RefactorConfig = RefactorConfig.default
  ): String =
    val suggestions    = evaluateAll(result, config)
    val totalReduction = suggestions.map(_.decision.estimatedReduction).sum

    s"""
       |Refactoring Analysis Report
       |===========================
       |
       |Summary:
       |  - Total subtrees analyzed: ${result.totalSubtrees}
       |  - Unique patterns: ${result.uniqueHashes}
       |  - Duplicate groups: ${result.groups.size}
       |  - Actionable suggestions: ${suggestions.size}
       |  - Total potential reduction: $totalReduction nodes
       |
       |Top Suggestions:
       |${suggestions.take(10).map(s => s"  ${s.rank}. ${s.suggestedModelName}: ${s.decision.summary}").mkString("\n")}
       |""".stripMargin

  // ============================================================================
  // Hierarchical Analysis: Detect and handle nested suggestions
  // ============================================================================

  /**
    * Analyze suggestions hierarchically to find optimal modeling targets.
    * 
    * When multiple suggestions have containment relationships (e.g., a Project node
    * is inside an AliasedRelation), this method identifies which level is optimal
    * for modeling to avoid redundant/overlapping refactorings.
    *
    * @param result
    *   Detection result containing all duplicate groups
    * @param config
    *   Refactoring configuration
    * @return
    *   HierarchicalSuggestionResult with both all suggestions and optimal ones
    */
  def evaluateHierarchically(
      result: DuplicateDetectionResult,
      config: RefactorConfig = RefactorConfig.default
  ): HierarchicalSuggestionResult =
    // First, get all actionable suggestions
    val baseSuggestions = result.groups.map { group =>
      val (decision, unifyResult) = evaluate(group, config)
      RefactoringSuggestion(group, unifyResult, decision)
    }.filter(_.decision.shouldRefactor)

    if baseSuggestions.isEmpty then
      return HierarchicalSuggestionResult(Nil, Nil, 0, 0)

    // Build containment relationships between suggestions
    val withHierarchy = buildHierarchy(baseSuggestions)

    // Sort by score and assign ranks
    val sorted = withHierarchy.sortBy(-_.decision.score)
    val ranked = sorted.zipWithIndex.map { case (s, idx) =>
      s.copy(rank = idx + 1)
    }

    // Extract optimal suggestions (those not subsumed by any other)
    val optimal = ranked.filter(_.isOptimal).sortBy(-_.decision.score)
    val optimalReranked = optimal.zipWithIndex.map { case (s, idx) =>
      s.copy(rank = idx + 1)
    }

    val totalAll = ranked.map(_.decision.estimatedReduction).sum
    val totalOptimal = optimalReranked.map(_.decision.estimatedReduction).sum

    HierarchicalSuggestionResult(
      allSuggestions = ranked,
      optimalSuggestions = optimalReranked,
      totalReductionAll = totalAll,
      totalReductionOptimal = totalOptimal
    )

  /**
    * Build containment hierarchy between suggestions.
    *
    * For each source (query), we check if subtrees of one suggestion are
    * descendants of subtrees of another suggestion. If ALL subtrees of
    * suggestion A (within a source) are descendants of subtrees in suggestion B,
    * then A is "subsumed by" B.
    */
  private def buildHierarchy(
      suggestions: List[RefactoringSuggestion]
  ): List[RefactoringSuggestion] =
    if suggestions.size <= 1 then
      return suggestions

    // Group subtrees by source for each suggestion
    val suggestionsByHash = suggestions.map(s => s.group.structuralHash -> s).toMap

    // For each pair of suggestions, check containment
    val subsumptionMap = scala.collection.mutable.Map[Int, Option[Int]]()  // child hash -> parent hash
    val childrenMap = scala.collection.mutable.Map[Int, List[Int]]().withDefaultValue(Nil)  // parent hash -> child hashes

    for
      s1 <- suggestions
      s2 <- suggestions
      if s1.group.structuralHash != s2.group.structuralHash
    do
      if isSubsumedBy(s1.group, s2.group) then
        // s1 is contained within s2
        val childHash = s1.group.structuralHash
        val parentHash = s2.group.structuralHash
        
        // Only record if this is a more direct parent (smaller pattern preferred as parent)
        // or if no parent is recorded yet
        subsumptionMap.get(childHash) match
          case None =>
            subsumptionMap(childHash) = Some(parentHash)
            childrenMap(parentHash) = childHash :: childrenMap(parentHash)
          case Some(existingParent) =>
            // Prefer the smaller (more immediate) parent
            val existingParentGroup = suggestionsByHash.get(existingParent.get).map(_.group)
            val newParentGroup = suggestionsByHash.get(parentHash).map(_.group)
            (existingParentGroup, newParentGroup) match
              case (Some(ep), Some(np)) if np.nodeCount < ep.nodeCount =>
                // New parent is smaller (more immediate), use it instead
                childrenMap(existingParent.get) = childrenMap(existingParent.get).filterNot(_ == childHash)
                subsumptionMap(childHash) = Some(parentHash)
                childrenMap(parentHash) = childHash :: childrenMap(parentHash)
              case _ => // Keep existing parent

    // Build updated suggestions with hierarchy info
    suggestions.map { s =>
      val hash = s.group.structuralHash
      s.copy(
        subsumedBy = subsumptionMap.get(hash).flatten,
        subsumes = childrenMap(hash)
      )
    }

  /**
    * Check if group1's subtrees are all contained within group2's subtrees.
    *
    * For this to be true, for each source that appears in both groups,
    * every subtree from group1 must be a descendant of some subtree in group2.
    */
  private def isSubsumedBy(group1: DuplicateGroup, group2: DuplicateGroup): Boolean =
    // Group subtrees by sourceId
    val bySource1 = group1.subtrees.groupBy(_.sourceId)
    val bySource2 = group2.subtrees.groupBy(_.sourceId)

    // Find common sources
    val commonSources = bySource1.keySet.intersect(bySource2.keySet)
    
    if commonSources.isEmpty then
      return false

    // For each common source, check if all group1 subtrees are descendants of group2 subtrees
    commonSources.forall { source =>
      val subtrees1 = bySource1(source)
      val subtrees2 = bySource2(source)

      subtrees1.forall { st1 =>
        subtrees2.exists { st2 =>
          st1.isDescendantOf(st2)
        }
      }
    }

  /**
    * Get optimal refactoring suggestions (hierarchically filtered)
    */
  def getOptimalSuggestions(
      result: DuplicateDetectionResult,
      topN: Int = 10,
      config: RefactorConfig = RefactorConfig.default
  ): List[RefactoringSuggestion] =
    evaluateHierarchically(result, config).optimalSuggestions.take(topN)

  /**
    * Print hierarchical analysis results
    */
  def printHierarchicalSuggestions(hierarchyResult: HierarchicalSuggestionResult): Unit =
    if hierarchyResult.allSuggestions.isEmpty then
      info("No refactoring suggestions")
      return

    info(hierarchyResult.summary)
    info("")
    info("=== Optimal Suggestions (for modeling) ===")
    info("")

    hierarchyResult.optimalSuggestions.foreach { s =>
      val childInfo = if s.subsumes.nonEmpty then
        s"\n  Contains: ${s.subsumes.size} nested patterns"
      else ""
      info(s"""
        |#${s.rank} ${s.suggestedModelName}
        |  ${s.decision.summary}
        |  Occurrences: ${s.group.occurrences}
        |  Parameters: ${s.parameterCount}
        |  Sources: ${s.group.sources.mkString(", ")}$childInfo
        |""".stripMargin)
    }

    val subsumed = hierarchyResult.allSuggestions.filter(_.subsumedBy.isDefined)
    if subsumed.nonEmpty then
      info("")
      info(s"=== Subsumed Patterns (${subsumed.size} patterns, included in optimal suggestions) ===")
      subsumed.take(5).foreach { s =>
        info(s"  - ${s.suggestedModelName}: ${s.group.occurrences} occ, ${s.group.nodeCount.toInt} nodes (inside pattern ${s.subsumedBy.map(h => (h.abs % 10000).toString).getOrElse("?")})")
      }
      if subsumed.size > 5 then
        info(s"  ... and ${subsumed.size - 5} more")

  /**
    * Generate hierarchical report
    */
  def generateHierarchicalReport(
      result: DuplicateDetectionResult,
      config: RefactorConfig = RefactorConfig.default
  ): String =
    val hierarchy = evaluateHierarchically(result, config)

    s"""
       |Hierarchical Refactoring Analysis Report
       |========================================
       |
       |${hierarchy.summary}
       |
       |Optimal Suggestions (model these):
       |${hierarchy.optimalSuggestions.take(10).map(s => 
         s"  ${s.rank}. ${s.suggestedModelName}: ${s.decision.summary}" +
         (if s.subsumes.nonEmpty then s" [contains ${s.subsumes.size} nested]" else "")
       ).mkString("\n")}
       |
       |Note: Optimal suggestions are root-level patterns that are not contained
       |within other patterns. Modeling these will automatically cover the nested
       |patterns, maximizing reduction without redundancy.
       |""".stripMargin

end RefactoringDecider
