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
  */
case class RefactoringSuggestion(
    group: DuplicateGroup,
    unifyResult: Option[AntiUnifyResult],
    decision: RefactoringDecision,
    rank: Int = 0
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

end RefactoringSuggestion

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
    *   RefactoringDecision
    */
  def evaluate(group: DuplicateGroup, config: RefactorConfig = RefactorConfig.default): RefactoringDecision =
    val warnings = scala.collection.mutable.ListBuffer[String]()

    // Check minimum occurrences
    if group.occurrences < config.minOccurrences then
      return RefactoringDecision(
        shouldRefactor = false,
        reason = s"Occurrences (${group.occurrences}) < minimum (${config.minOccurrences})",
        score = 0.0,
        estimatedReduction = 0
      )

    // Check minimum pattern size
    if group.nodeCount < config.minPatternSize then
      return RefactoringDecision(
        shouldRefactor = false,
        reason = s"Pattern size (${group.nodeCount.toInt}) < minimum (${config.minPatternSize})",
        score = 0.0,
        estimatedReduction = 0
      )

    // Try anti-unification to get parameter count
    val unifyResult = AntiUnifier.unify(group)
    val paramCount  = unifyResult.map(_.variableParameters.size).getOrElse(0)

    // Check parameter count
    if paramCount > config.maxParameters then
      return RefactoringDecision(
        shouldRefactor = false,
        reason = s"Too many parameters ($paramCount > ${config.maxParameters})",
        score = 0.0,
        estimatedReduction = 0,
        warnings = List("Consider breaking down the pattern")
      )

    // Calculate reduction
    val originalNodes   = (group.nodeCount * group.occurrences).toInt
    val modelNodes      = group.nodeCount.toInt  // Model definition
    val callNodes       = group.occurrences * (1 + paramCount)  // Model calls with params
    val refactoredNodes = modelNodes + callNodes
    val reduction       = originalNodes - refactoredNodes
    val reductionRatio  = reduction.toDouble / originalNodes

    // Check minimum reduction
    if reduction < config.minReduction then
      return RefactoringDecision(
        shouldRefactor = false,
        reason = s"Reduction ($reduction) < minimum (${config.minReduction})",
        score = 0.0,
        estimatedReduction = reduction
      )

    // Check minimum reduction ratio
    if reductionRatio < config.minReductionRatio then
      return RefactoringDecision(
        shouldRefactor = false,
        reason = f"Reduction ratio (${reductionRatio * 100}%.1f%%) < minimum (${config.minReductionRatio * 100}%.0f%%)",
        score = 0.0,
        estimatedReduction = reduction
      )

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

    RefactoringDecision(
      shouldRefactor = true,
      reason = f"${group.occurrences} occurrences, $reduction node reduction (${reductionRatio * 100}%.0f%%)",
      score = score,
      estimatedReduction = reduction,
      warnings = warnings.toList
    )

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
      val decision    = evaluate(group, config)
      val unifyResult = if decision.shouldRefactor then AntiUnifier.unify(group) else None
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

end RefactoringDecider
