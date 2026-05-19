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
  * Score weights for the four components of the ranking formula.
  *
  * @param reduction
  *   Weight for the relative reduction term (default 0.4)
  * @param reductionRatio
  *   Weight for the reduction-ratio term (default 0.3)
  * @param occurrences
  *   Weight for the relative occurrence term (default 0.2)
  * @param params
  *   Weight for the parameter-count penalty term (default 0.1)
  */
case class ScoreWeights(
    reduction: Double = 0.4,
    reductionRatio: Double = 0.3,
    occurrences: Double = 0.2,
    params: Double = 0.1
)

object ScoreWeights:
  val default: ScoreWeights = ScoreWeights()
  val equal: ScoreWeights   = ScoreWeights(0.25, 0.25, 0.25, 0.25)

  /** Parse from a comma-separated string, e.g. "0.4,0.3,0.2,0.1" */
  def fromString(s: String): ScoreWeights =
    s.trim.toLowerCase match
      case "default" => default
      case "equal"   => equal
      case other =>
        val parts = other.split(",").map(_.trim.toDouble)
        require(parts.length == 4, s"Expected 4 comma-separated weights, got: $s")
        ScoreWeights(parts(0), parts(1), parts(2), parts(3))

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
  * @param scoreWeights
  *   Weights for the four scoring components
  */
case class RefactorConfig(
    minOccurrences: Int = 2,
    minPatternSize: Int = 3,
    maxParameters: Int = 6,
    minReduction: Int = 10,
    minReductionRatio: Double = 0.3,
    preferCrossQuery: Boolean = true,
    scoreWeights: ScoreWeights = ScoreWeights.default
)

enum RefactoringSelectionMode:
  case Greedy, OptimizeScore

object RefactoringSelectionMode:
  def fromString(value: String): RefactoringSelectionMode =
    value.toLowerCase.replace("_", "-") match
      case "greedy" | "outside-in" => Greedy
      case "simple-optimize" | "optimized" | "mwis" => OptimizeScore
      case other =>
        throw new IllegalArgumentException(
          s"Unknown refactoring selection mode: $other (expected: greedy or simple-optimize)"
        )

object RefactorConfig:
  val default: RefactorConfig = RefactorConfig()

  // More aggressive refactoring
  val aggressive: RefactorConfig = RefactorConfig(
    minOccurrences = 2,
    minPatternSize = 2,
    maxParameters = 20,  // Higher limit for cross-query patterns with many table/column differences
    minReduction = Int.MinValue,    // Allow any reduction (even negative) for aggressive cross-query refactoring
    minReductionRatio = -1.0  // No minimum ratio for aggressive mode
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

  /** Normalization reference values for relative score computation. */
  private case class ScoreNorm(maxReduction: Int, maxOccurrences: Int)
  private object ScoreNorm:
    /** Fallback used when no corpus statistics are available (single-group evaluation). */
    val default: ScoreNorm = ScoreNorm(100, 10)
    def fromSuggestions(suggestions: Iterable[RefactoringSuggestion]): ScoreNorm =
      val maxR = suggestions.map(_.decision.estimatedReduction).maxOption.getOrElse(100).max(1)
      val maxO = suggestions.map(_.group.occurrences).maxOption.getOrElse(10).max(1)
      ScoreNorm(maxR, maxO)

  /** Re-score a suggestion using corpus-relative normalization. */
  private def rescoreWith(s: RefactoringSuggestion, config: RefactorConfig, norm: ScoreNorm): RefactoringSuggestion =
    val paramCount    = s.parameterCount
    val originalNodes = (s.group.nodeCount * s.group.occurrences).max(1.0).toInt
    val reductionRatio = s.decision.estimatedReduction.toDouble / originalNodes
    val newScore = calculateScore(s.group, s.decision.estimatedReduction, reductionRatio, paramCount, config, norm)
    s.copy(decision = s.decision.copy(score = newScore))

  /**
    * Calculate refactoring score for ranking.
    * Reduction and occurrence terms are normalized relative to `norm` so that
    * the highest-valued candidate in the current batch always receives the
    * full weight for those components (no arbitrary hard clip).
    */
  private def calculateScore(
      group: DuplicateGroup,
      reduction: Int,
      reductionRatio: Double,
      paramCount: Int,
      config: RefactorConfig,
      norm: ScoreNorm = ScoreNorm.default
  ): Double =
    var score = 0.0

    // Reduction contributes — normalized by max reduction in this batch
    score += (reduction.toDouble / norm.maxReduction).min(1.0) * config.scoreWeights.reduction

    // Reduction ratio contribution
    score += reductionRatio * config.scoreWeights.reductionRatio

    // Occurrences — normalized by max occurrences in this batch
    score += (group.occurrences.toDouble / norm.maxOccurrences).min(1.0) * config.scoreWeights.occurrences

    // Fewer parameters is better
    val paramScore = 1.0 - (paramCount.toDouble / config.maxParameters)
    score += paramScore * config.scoreWeights.params

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

    // Two-pass: recompute scores relative to actual data distribution, then rank
    val feasible = suggestions.filter(_.decision.shouldRefactor)
    val norm     = ScoreNorm.fromSuggestions(feasible)
    val sorted   = feasible.map(s => rescoreWith(s, config, norm)).sortBy(-_.decision.score)

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
      config: RefactorConfig = RefactorConfig.default,
      selectionMode: RefactoringSelectionMode = RefactoringSelectionMode.Greedy
  ): HierarchicalSuggestionResult =
    // First, evaluate all suggestions and track why they're rejected
    val allEvaluated = result.groups.map { group =>
      val (decision, unifyResult) = evaluate(group, config)
      RefactoringSuggestion(group, unifyResult, decision)
    }
    
    // Debug: show why groups were rejected
    val rejected = allEvaluated.filterNot(_.decision.shouldRefactor)
    if rejected.nonEmpty then
      println(s"  [RefactoringDecider] ${rejected.size} groups rejected:")
      rejected.take(5).foreach { s =>
        println(s"    - nodes=${s.group.nodeCount.toInt}, occ=${s.group.occurrences}, " +
                s"cross=${s.group.isCrossQuery}: ${s.decision.reason}")
      }
      if rejected.size > 5 then
        println(s"    ... and ${rejected.size - 5} more")
    
    val feasible = allEvaluated.filter(_.decision.shouldRefactor)

    if feasible.isEmpty then
      return HierarchicalSuggestionResult(Nil, Nil, 0, 0)

    // Two-pass: recompute scores relative to actual data distribution
    val norm           = ScoreNorm.fromSuggestions(feasible)
    val baseSuggestions = feasible.map(s => rescoreWith(s, config, norm))

    // Build containment relationships between suggestions
    val withHierarchy = buildHierarchy(baseSuggestions)

    // Sort by score and assign ranks
    val sorted = withHierarchy.sortBy(-_.decision.score)
    val ranked = sorted.zipWithIndex.map { case (s, idx) =>
      s.copy(rank = idx + 1)
    }

    // Select optimal suggestions using outside-in evaluation:
    // Start from outermost patterns, if they are good candidates (positive reduction),
    // adopt them and exclude their children. If not, consider their children instead.
    val optimal = selectionMode match
      case RefactoringSelectionMode.Greedy =>
        selectOptimalOutsideIn(ranked, config)
      case RefactoringSelectionMode.OptimizeScore =>
        selectOptimalByScore(ranked, config)
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
    * Select optimal suggestions using greedy score-based selection.
    *
    * This method mirrors --apply's per-file greedy selection logic:
    * 1. Sort all suggestions by score (descending)
    * 2. For each suggestion, check if it conflicts with already-selected patterns
    * 3. A pattern conflicts if any of its occurrences overlap (ancestor/descendant) with selected ones
    * 4. Select non-conflicting patterns
    *
    * This ensures the report shows exactly which patterns would be applied with --apply.
    */
  private def selectOptimalOutsideIn(
      suggestions: List[RefactoringSuggestion],
      config: RefactorConfig
  ): List[RefactoringSuggestion] =
    if suggestions.isEmpty then
      return Nil

    // Note: variableParameters filtering is handled by RefactoringApplier via ApplyConfig.requireVariableParams.
    // Do NOT filter here to allow patterns without variable params (e.g., exact duplicates) through.
    
    // Sort by score descending, then by name for deterministic ordering
    val sorted = suggestions.sortBy(s => (-s.decision.score, s.suggestedModelName))
    
    // Track covered paths across all files: Set[path]
    // In cross-query analysis, we need to track by (sourceId, path)
    val coveredPaths = scala.collection.mutable.Set[(Option[String], List[Int])]()
    val selected = scala.collection.mutable.ListBuffer[RefactoringSuggestion]()
    
    // Helper to check path overlap (ancestor/descendant relationship)
    def isPathOverlap(path1: List[Int], path2: List[Int]): Boolean =
      if path1.size <= path2.size then
        path2.startsWith(path1)
      else
        path1.startsWith(path2)
    
    sorted.foreach { s =>
      // Get all occurrence paths for this pattern
      val occurrencePaths = s.group.subtrees.map(st => (st.sourceId, st.path))
      
      // Check if any occurrence conflicts with already-selected patterns
      val hasConflict = occurrencePaths.exists { case (sourceId, path) =>
        coveredPaths.exists { case (coveredSourceId, coveredPath) =>
          sourceId == coveredSourceId && isPathOverlap(path, coveredPath)
        }
      }
      
      if !hasConflict then
        // Select this pattern and mark its paths as covered
        selected += s
        occurrencePaths.foreach { case (sourceId, path) =>
          coveredPaths += ((sourceId, path))
        }
    }
    
    selected.toList

  /**
    * Select suggestions by maximizing the existing score subject to path-overlap constraints.
    * Small connected components are solved exactly by branch-and-bound; very large components
    * fall back to the existing greedy selector to keep bulk runs bounded.
    */
  private def selectOptimalByScore(
      suggestions: List[RefactoringSuggestion],
      config: RefactorConfig
  ): List[RefactoringSuggestion] =
    if suggestions.isEmpty then
      return Nil

    val adjacency = buildConflictGraph(suggestions)
    val components = connectedComponents(adjacency)
    val exactComponentLimit = 28

    val selectedIndexes = components.flatMap { component =>
      if component.size <= exactComponentLimit then
        solveExactMaximumWeightIndependentSet(component, adjacency, suggestions)
      else
        val greedySubset = selectOptimalOutsideIn(component.map(suggestions), config)
        val hashes = greedySubset.map(_.group.structuralHash).toSet
        component.filter(i => hashes.contains(suggestions(i).group.structuralHash))
    }.distinct

    selectedIndexes
      .map(suggestions)
      .sortBy(s => (-s.decision.score, s.suggestedModelName))

  private def buildConflictGraph(suggestions: List[RefactoringSuggestion]): Array[scala.collection.mutable.Set[Int]] =
    val adjacency = Array.fill(suggestions.size)(scala.collection.mutable.Set.empty[Int])

    def isPathOverlap(path1: List[Int], path2: List[Int]): Boolean =
      if path1.size <= path2.size then path2.startsWith(path1)
      else path1.startsWith(path2)

    val occurrencesBySource = suggestions.zipWithIndex.flatMap { case (suggestion, idx) =>
      suggestion.group.subtrees.map(st => (st.sourceId, st.path, idx))
    }.groupBy(_._1)

    occurrencesBySource.values.foreach { occurrences =>
      val items = occurrences.toArray
      var i = 0
      while i < items.length do
        var j = i + 1
        while j < items.length do
          val (_, path1, idx1) = items(i)
          val (_, path2, idx2) = items(j)
          if idx1 != idx2 && isPathOverlap(path1, path2) then
            adjacency(idx1) += idx2
            adjacency(idx2) += idx1
          j += 1
        i += 1
    }

    adjacency

  private def connectedComponents(adjacency: Array[scala.collection.mutable.Set[Int]]): List[List[Int]] =
    val visited = Array.fill(adjacency.length)(false)
    val components = scala.collection.mutable.ListBuffer.empty[List[Int]]

    adjacency.indices.foreach { start =>
      if !visited(start) then
        val stack = scala.collection.mutable.Stack(start)
        val component = scala.collection.mutable.ListBuffer.empty[Int]
        visited(start) = true

        while stack.nonEmpty do
          val current = stack.pop()
          component += current
          adjacency(current).foreach { next =>
            if !visited(next) then
              visited(next) = true
              stack.push(next)
          }

        components += component.toList
    }

    components.toList

  private def solveExactMaximumWeightIndependentSet(
      component: List[Int],
      adjacency: Array[scala.collection.mutable.Set[Int]],
      suggestions: List[RefactoringSuggestion]
  ): List[Int] =
    var bestWeight = Double.NegativeInfinity
    var bestSelected = Set.empty[Int]
    val weightByIndex = component.map(i => i -> suggestions(i).decision.score).toMap

    def search(candidates: Set[Int], selected: Set[Int], selectedWeight: Double): Unit =
      // Use only positive weights for upper bound: including negative scores would deflate
      // the bound and cause valid branches to be pruned prematurely.
      val upperBound = selectedWeight + candidates.iterator.map(weightByIndex).filter(_ > 0).sum
      if upperBound <= bestWeight + 1e-12 then
        return

      if candidates.isEmpty then
        if selectedWeight > bestWeight + 1e-12 ||
            (math.abs(selectedWeight - bestWeight) <= 1e-12 && selected.size > bestSelected.size) then
          bestWeight = selectedWeight
          bestSelected = selected
        return

      val pivot = candidates.maxBy(i => (adjacency(i).count(candidates.contains), weightByIndex(i)))
      val withoutPivot = candidates - pivot

      search(withoutPivot -- adjacency(pivot), selected + pivot, selectedWeight + weightByIndex(pivot))
      search(withoutPivot, selected, selectedWeight)

    search(component.toSet, Set.empty, 0.0)
    bestSelected.toList
  
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
      config: RefactorConfig = RefactorConfig.default,
      selectionMode: RefactoringSelectionMode = RefactoringSelectionMode.Greedy
  ): List[RefactoringSuggestion] =
    evaluateHierarchically(result, config, selectionMode).optimalSuggestions.take(topN)

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
      config: RefactorConfig = RefactorConfig.default,
      selectionMode: RefactoringSelectionMode = RefactoringSelectionMode.Greedy
  ): String =
    val hierarchy = evaluateHierarchically(result, config, selectionMode)

    formatHierarchicalReport(hierarchy, selectionMode)

  def formatHierarchicalReport(
      hierarchy: HierarchicalSuggestionResult,
      selectionMode: RefactoringSelectionMode = RefactoringSelectionMode.Greedy
  ): String =

    s"""
       |Hierarchical Refactoring Analysis Report
       |========================================
       |
       |Selection mode: ${selectionMode.toString}
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
