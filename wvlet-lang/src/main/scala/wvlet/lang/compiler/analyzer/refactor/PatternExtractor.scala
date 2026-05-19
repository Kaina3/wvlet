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

import wvlet.lang.compiler.CompilationUnit
import wvlet.lang.compiler.Context
import wvlet.lang.compiler.Phase
import wvlet.lang.model.plan.*
import wvlet.log.LogSupport

/**
  * Configuration for the PatternExtractor phase
  *
  * @param enabled
  *   Whether pattern extraction is enabled
  * @param refactorConfig
  *   Configuration for refactoring decisions
  * @param collectorConfig
  *   Configuration for subtree collection
  * @param detectorConfig
  *   Configuration for duplicate detection
  * @param maxSuggestions
  *   Maximum number of suggestions to generate
  * @param crossQueryAnalysis
  *   Whether to analyze patterns across multiple queries
  * @param selectionMode
  *   Strategy for selecting non-overlapping suggestions
  */
case class PatternExtractorConfig(
    enabled: Boolean = true,
    refactorConfig: RefactorConfig = RefactorConfig.default,
    collectorConfig: CollectorConfig = CollectorConfig.default,
    detectorConfig: DetectorConfig = DetectorConfig.default,
    maxSuggestions: Int = 10,
    crossQueryAnalysis: Boolean = false,
    selectionMode: RefactoringSelectionMode = RefactoringSelectionMode.Greedy
)

object PatternExtractorConfig:
  val default: PatternExtractorConfig = PatternExtractorConfig()
  
  // Disabled by default
  val disabled: PatternExtractorConfig = PatternExtractorConfig(enabled = false)

/**
  * Result of pattern extraction
  *
  * @param suggestions
  *   List of refactoring suggestions (may include nested patterns)
  * @param detectionResult
  *   Raw duplicate detection result
  * @param report
  *   Human-readable report
  * @param hierarchyResult
  *   Optional hierarchical analysis result with optimal suggestions
  */
case class PatternExtractionResult(
    suggestions: List[RefactoringSuggestion],
    detectionResult: DuplicateDetectionResult,
    report: String,
    hierarchyResult: Option[HierarchicalSuggestionResult] = None
):
  /**
    * Whether any actionable suggestions were found
    */
  def hasSuggestions: Boolean = suggestions.nonEmpty

  /**
    * Get the top suggestion
    */
  def topSuggestion: Option[RefactoringSuggestion] = suggestions.headOption

  /**
    * Total potential node reduction (from all suggestions, may overlap)
    */
  def totalPotentialReduction: Int = suggestions.map(_.decision.estimatedReduction).sum

  /**
    * Optimal suggestions for modeling (no overlapping/nested patterns)
    */
  def optimalSuggestions: List[RefactoringSuggestion] =
    hierarchyResult.map(_.optimalSuggestions).getOrElse(suggestions)

  /**
    * Total reduction from optimal suggestions (no double-counting)
    */
  def optimalReduction: Int =
    hierarchyResult.map(_.totalReductionOptimal).getOrElse(totalPotentialReduction)

  /**
    * Number of suggestions that are subsumed by others (nested patterns)
    */
  def subsumedCount: Int =
    hierarchyResult.map(_.allSuggestions.count(_.subsumedBy.isDefined)).getOrElse(0)

  /**
    * All suggestions including subsumed ones.
    * For multi-file apply, we use this to consider all candidates and decide per-file.
    */
  def allSuggestions: List[RefactoringSuggestion] =
    hierarchyResult.map(_.allSuggestions).getOrElse(suggestions)

end PatternExtractionResult

/**
  * Compiler phase for extracting duplicate patterns from LogicalPlan trees.
  *
  * This phase analyzes the logical plan to find repeated structural patterns that could be
  * refactored into reusable models. It uses structural hashing for efficient candidate detection and
  * anti-unification for parameter extraction.
  *
  * Usage:
  * {{{
  * // Single query analysis
  * val result = PatternExtractor.analyze(plan)
  *
  * // Cross-query analysis
  * val plans = List(("query1", plan1), ("query2", plan2))
  * val result = PatternExtractor.analyzeMultiple(plans)
  * }}}
  */
object PatternExtractor extends Phase("pattern-extractor") with LogSupport:

  // Store results for later retrieval
  private var lastResult: Option[PatternExtractionResult] = None

  /**
    * Get the last extraction result
    */
  def getLastResult: Option[PatternExtractionResult] = lastResult

  /**
    * Clear the last result
    */
  def clearLastResult(): Unit = lastResult = None

  /**
    * Phase entry point (called by compiler)
    */
  override def run(unit: CompilationUnit, context: Context): CompilationUnit =
    // Check if pattern extraction is enabled
    // For now, we run analysis but don't modify the plan
    val config = PatternExtractorConfig.default

    if !config.enabled then
      return unit

    // Extract the logical plan
    val plan = unit.resolvedPlan match
      case p: PackageDef =>
        // Find the main query in the package
        p.statements.collectFirst { case q: Query => q.child }.getOrElse(p)
      case other => other

    // Run analysis
    val result = analyze(plan, config)
    lastResult = Some(result)

    // Log results
    if result.hasSuggestions then
      info(s"Found ${result.suggestions.size} refactoring opportunities")
      debug(result.report)
    else
      debug("No refactoring opportunities found")

    // Return original unit (no modifications yet)
    // In future, could return modified unit with generated models
    unit

  /**
    * Analyze a single LogicalPlan for duplicate patterns
    *
    * @param plan
    *   The plan to analyze
    * @param config
    *   Configuration for extraction
    * @return
    *   Extraction result with suggestions
    */
  def analyze(
      plan: LogicalPlan,
      config: PatternExtractorConfig = PatternExtractorConfig.default
  ): PatternExtractionResult =
    debug(s"Analyzing plan for duplicate patterns...")

    // Detect duplicates
    val detectionResult = DuplicateDetector.detect(
      plan,
      config.detectorConfig,
      config.collectorConfig
    )

    debug(detectionResult.summary)

    // Run hierarchical analysis to find optimal suggestions
    val hierarchyResult = RefactoringDecider.evaluateHierarchically(
      detectionResult,
      config.refactorConfig,
      config.selectionMode
    )

    // Use optimal suggestions (filtered for nested patterns)
    val suggestions = hierarchyResult.optimalSuggestions.take(config.maxSuggestions)

    // Generate report
    val report = RefactoringDecider.formatHierarchicalReport(hierarchyResult, config.selectionMode)

    PatternExtractionResult(suggestions, detectionResult, report, Some(hierarchyResult))

  /**
    * Analyze multiple LogicalPlans for cross-query patterns
    *
    * @param plans
    *   List of (sourceId, plan) pairs
    * @param config
    *   Configuration for extraction
    * @return
    *   Extraction result with suggestions
    */
  def analyzeMultiple(
      plans: List[(String, LogicalPlan)],
      config: PatternExtractorConfig = PatternExtractorConfig.default
  ): PatternExtractionResult =
    debug(s"Analyzing ${plans.size} plans for cross-query patterns...")

    // Detect duplicates across all plans
    val t0 = System.currentTimeMillis()
    val detectionResult = DuplicateDetector.detectAcross(
      plans,
      config.detectorConfig,
      config.collectorConfig
    )
    val t1 = System.currentTimeMillis()
    println(s"  [PatternExtractor] Duplicate detection: ${t1 - t0}ms")

    debug(detectionResult.summary)

    // Run hierarchical analysis to find optimal suggestions
    val t2 = System.currentTimeMillis()
    val hierarchyResult = RefactoringDecider.evaluateHierarchically(
      detectionResult,
      config.refactorConfig,
      config.selectionMode
    )
    val t3 = System.currentTimeMillis()
    println(s"  [PatternExtractor] Hierarchical evaluation: ${t3 - t2}ms")

    // Use optimal suggestions (filtered for nested patterns)
    val suggestions = hierarchyResult.optimalSuggestions.take(config.maxSuggestions)

    // Generate report
    val report = RefactoringDecider.formatHierarchicalReport(hierarchyResult, config.selectionMode)

    PatternExtractionResult(suggestions, detectionResult, report, Some(hierarchyResult))

  /**
    * Quick analysis with default settings
    */
  def quickAnalyze(plan: LogicalPlan): PatternExtractionResult =
    analyze(plan, PatternExtractorConfig.default)

  /**
    * Analyze with aggressive settings (more patterns detected)
    */
  def aggressiveAnalyze(plan: LogicalPlan): PatternExtractionResult =
    val config = PatternExtractorConfig(
      refactorConfig = RefactorConfig.aggressive,
      collectorConfig = CollectorConfig(minDepth = 1, minNodeCount = 2),
      detectorConfig = DetectorConfig.aggressive
    )
    analyze(plan, config)

  /**
    * Analyze with conservative settings (only significant patterns)
    */
  def conservativeAnalyze(plan: LogicalPlan): PatternExtractionResult =
    val config = PatternExtractorConfig(
      refactorConfig = RefactorConfig.conservative,
      collectorConfig = CollectorConfig(minDepth = 3, minNodeCount = 5),
      detectorConfig = DetectorConfig(minOccurrences = 3, minNodeCount = 5, minDepth = 3)
    )
    analyze(plan, config)

  /**
    * Print a summary of the extraction result
    */
  def printSummary(result: PatternExtractionResult): Unit =
    info(result.report)

    if result.hasSuggestions then
      info("\nTop Refactoring Suggestions:")
      RefactoringDecider.printSuggestions(result.suggestions)

end PatternExtractor
