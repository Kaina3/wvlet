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

import wvlet.lang.api.Span.NoSpan
import wvlet.lang.catalog.Catalog.TableName
import wvlet.lang.compiler.TermName
import wvlet.lang.model.DataType
import wvlet.lang.model.plan.*
import wvlet.lang.model.expr.*
import wvlet.log.LogSupport

/**
  * Configuration for applying refactorings
  *
  * @param topK Number of top suggestions to apply (0 = all)
  * @param useOptimalSuggestions Whether to use optimal (non-overlapping) suggestions (single-file only)
  * @param modelNamePrefix Prefix for generated model names
  * @param skipIfOverlaps Whether to skip suggestions that would conflict with already-applied ones
  * @param requireVariableParams Only apply suggestions that have variable parameters
  * @param perFileMinOccurrences For multi-file apply: minimum occurrences per file to apply a pattern there
   * @param usePerFileSelection For multi-file apply: use per-file greedy selection instead of global optimal
   * @param selectionMode Selection strategy produced by pattern extraction
  */
case class ApplyConfig(
    topK: Int = 0,
    useOptimalSuggestions: Boolean = true,
    modelNamePrefix: String = "auto",
    skipIfOverlaps: Boolean = true,
    requireVariableParams: Boolean = true,
    perFileMinOccurrences: Int = 2,
    usePerFileSelection: Boolean = true,
    selectionMode: RefactoringSelectionMode = RefactoringSelectionMode.Greedy
)

object ApplyConfig:
  val default: ApplyConfig = ApplyConfig()
  
  /** Config for multi-file apply with per-file selection (recommended) */
  val multiFile: ApplyConfig = ApplyConfig(
    useOptimalSuggestions = false, // We use per-file selection instead
    usePerFileSelection = true,
    perFileMinOccurrences = 2
  )

/**
  * Information about a successfully applied refactoring
  *
  * @param suggestionId The suggestion identifier (model name)
  * @param modelDef The generated ModelDef
  * @param occurrencesReplaced Number of occurrences replaced with ModelScan
  * @param nodeReduction Estimated node reduction achieved
  */
case class AppliedRefactoring(
    suggestionId: String,
    modelDef: ModelDef,
    occurrencesReplaced: Int,
    nodeReduction: Int
)

/**
  * Information about a skipped refactoring
  *
  * @param suggestionId The suggestion identifier
  * @param reason Why it was skipped
  */
case class SkippedRefactoring(
    suggestionId: String,
    reason: String
)

/**
  * Result of applying refactorings
  *
  * @param updatedPlan The transformed LogicalPlan
  * @param applied List of successfully applied refactorings
  * @param skipped List of skipped refactorings with reasons
  */
case class ApplyResult(
    updatedPlan: LogicalPlan,
    applied: List[AppliedRefactoring],
    skipped: List[SkippedRefactoring]
):
  def hasChanges: Boolean = applied.nonEmpty
  
  def totalOccurrencesReplaced: Int = applied.map(_.occurrencesReplaced).sum
  
  def totalNodeReduction: Int = applied.map(_.nodeReduction).sum
  
  def summary: String =
    s"""Apply Result:
       |  Applied: ${applied.size} refactoring(s)
       |  Skipped: ${skipped.size}
       |  Occurrences replaced: $totalOccurrencesReplaced
       |  Node reduction: $totalNodeReduction
       |""".stripMargin

end ApplyResult

/**
  * Result of applying refactorings across multiple files
  *
  * @param models List of generated ModelDefs (shared across files)
  * @param fileResults List of (fileName, transformedPlan, summary) tuples
  * @param applied All successfully applied refactorings
  * @param skipped All skipped refactorings
  */
case class MultiApplyResult(
    models: List[ModelDef],
    fileResults: List[(String, LogicalPlan)],
    applied: List[AppliedRefactoring],
    skipped: List[SkippedRefactoring]
):
  def hasChanges: Boolean = applied.nonEmpty
  
  def totalOccurrencesReplaced: Int = applied.map(_.occurrencesReplaced).sum
  
  def totalNodeReduction: Int = applied.map(_.nodeReduction).sum
  
  def summary: String =
    s"""Multi-file Apply Result:
       |  Models generated: ${models.size}
       |  Files transformed: ${fileResults.size}
       |  Applied: ${applied.size} refactoring(s)
       |  Skipped: ${skipped.size}
       |  Total occurrences replaced: $totalOccurrencesReplaced
       |  Total node reduction: $totalNodeReduction
       |""".stripMargin

end MultiApplyResult

/**
  * Applies refactoring suggestions to transform a LogicalPlan.
  *
  * This is the main orchestrator that:
  * 1. Selects applicable suggestions from PatternExtractionResult
  * 2. Generates ModelDef for each selected suggestion
  * 3. Replaces occurrences with ModelScan references
  * 4. Returns the transformed plan with model definitions added
  */
object RefactoringApplier extends LogSupport:

  /**
    * Apply refactorings to a LogicalPlan based on extraction results.
    *
    * @param root The original LogicalPlan (typically a PackageDef or Query)
    * @param extraction The pattern extraction result containing suggestions
    * @param config Configuration for how to apply refactorings
    * @return ApplyResult containing the updated plan and application details
    */
  def applyRefactorings(
      root: LogicalPlan,
      extraction: PatternExtractionResult,
      config: ApplyConfig = ApplyConfig.default
  ): ApplyResult =
    if !extraction.hasSuggestions then
      return ApplyResult(root, Nil, Nil)

    // Get suggestions to apply
    val suggestions = if config.useOptimalSuggestions then
      extraction.optimalSuggestions
    else
      extraction.suggestions

    // Filter and take top K (0 = all)
    val filtered = suggestions
      .filter(s => s.decision.shouldRefactor)
      .filter(s => !config.requireVariableParams || s.unifyResult.exists(_.variableParameters.nonEmpty))
    val candidateSuggestions = if config.topK <= 0 then filtered else filtered.take(config.topK)

    if candidateSuggestions.isEmpty then
      return ApplyResult(root, Nil, List(SkippedRefactoring("all", "No applicable suggestions found")))

    // Track applied paths to detect overlaps
    val appliedPaths = scala.collection.mutable.Set[List[Int]]()
    val applied = scala.collection.mutable.ListBuffer[AppliedRefactoring]()
    val skipped = scala.collection.mutable.ListBuffer[SkippedRefactoring]()
    val generatedModels = scala.collection.mutable.ListBuffer[ModelDef]()
    
    // Collect all replacements first
    var currentPlan = root

    candidateSuggestions.foreach { suggestion =>
      val suggestionId = suggestion.suggestedModelName
      
      suggestion.unifyResult match
        case None =>
          skipped += SkippedRefactoring(suggestionId, "No anti-unification result")
          
        case Some(unifyResult) =>
          // Check for variable parameters
          val varParams = unifyResult.variableParameters
          if config.requireVariableParams && varParams.isEmpty then
            skipped += SkippedRefactoring(suggestionId, "No variable parameters")
          else
            // Check for overlaps with already-applied paths
            val occurrences = suggestion.group.subtrees
            val nonOverlapping = occurrences.filter { subtree =>
              !appliedPaths.exists(appliedPath =>
                isPathOverlap(subtree.path, appliedPath)
              )
            }
            
            if nonOverlapping.isEmpty then
              skipped += SkippedRefactoring(suggestionId, "All occurrences overlap with previously applied refactorings")
            else
              // Generate model name with prefix
              val modelName = s"${config.modelNamePrefix}_${suggestionId}"
              
              // Generate ModelDef
              val modelDef = generateModelDef(modelName, unifyResult)
              generatedModels += modelDef
              
              // Generate replacements for non-overlapping occurrences
              val replacements = nonOverlapping.zipWithIndex.map { case (subtree, idx) =>
                val substitution = unifyResult.substitutions.lift(idx).getOrElse(
                  unifyResult.substitutions.headOption.getOrElse(
                    Substitution(0, Map.empty)
                  )
                )
                val modelScan = generateModelScan(modelName, varParams, substitution, subtree)
                (subtree.path, modelScan)
              }
              
              // Apply replacements
              currentPlan = PathRewriter.replaceMany(currentPlan, replacements.toList)
              
              // Mark paths as applied
              nonOverlapping.foreach(s => appliedPaths += s.path)
              
              applied += AppliedRefactoring(
                suggestionId = modelName,
                modelDef = modelDef,
                occurrencesReplaced = nonOverlapping.size,
                nodeReduction = suggestion.decision.estimatedReduction
              )
    }

    // Insert generated models into the plan
    val finalPlan = insertModels(currentPlan, generatedModels.toList)

    ApplyResult(finalPlan, applied.toList, skipped.toList)

  /**
    * Check if two paths overlap (one is a prefix/ancestor of the other)
    */
  private def isPathOverlap(path1: List[Int], path2: List[Int]): Boolean =
    path1.startsWith(path2) || path2.startsWith(path1)

  /**
    * Generate a ModelDef from an AntiUnifyResult
    *
    * @param modelName The name for the model
    * @param unifyResult The anti-unification result containing pattern and parameters
    * @return A ModelDef node
    */
  private def generateModelDef(modelName: String, unifyResult: AntiUnifyResult): ModelDef =
    // Recursively strip wrappers that don't add semantics to a reusable model body.
    // Keep stripping until we hit a non-wrapper node or an actual relation operation.
    @scala.annotation.tailrec
    def stripNonSemanticWrappersForModel(r: Relation): Relation =
      r match
        // The alias is an occurrence-local detail (e.g., join ... as t9) and should not be baked into a model body.
        // Keeping it causes extra nesting like `from { ... } as t9` inside model definitions.
        case a: AliasedRelation =>
          stripNonSemanticWrappersForModel(a.child)
        // Query wrapper doesn't add semantics for a model body, but can affect pretty-printing/brace insertion.
        case q: Query =>
          stripNonSemanticWrappersForModel(q.child)
        case other =>
          other

    // Extract variable parameters (those that differ across instances)
    val varParams = unifyResult.variableParameters
    
    // Generate DefArg for each variable parameter
    val defArgs = varParams.map { param =>
      DefArg(
        name = TermName(param.id.stripPrefix("$")),
        dataType = param.inferredType,
        defaultValue = None,
        span = NoSpan
      )
    }
    
    // Wrap the pattern in a Query. Also strip wrappers that should not be part of a reusable model body.
    val queryBody = unifyResult.pattern match
      case q: Query =>
        Query(stripNonSemanticWrappersForModel(q.child), NoSpan)
      case r: Relation =>
        Query(stripNonSemanticWrappersForModel(r), NoSpan)
      case other =>
        // Fallback: wrap in Query with EmptyRelation
        // This shouldn't happen if SubtreeCollector only collects Relations
        warn(s"Pattern is not a Relation: ${other.getClass.getSimpleName}, using EmptyRelation")
        Query(EmptyRelation(NoSpan), NoSpan)
    
    ModelDef(
      name = TableName(modelName),
      params = defArgs,
      givenRelationType = None,
      child = queryBody,
      span = NoSpan
    )

  /**
    * Generate a ModelScan (model invocation) for a specific occurrence
    *
    * @param modelName The model name to reference
    * @param params The model's parameters (ExtractedParameter list)
    * @param substitution The substitution for this occurrence
    * @param subtree The original subtree being replaced
    * @return A ModelScan node
    */
  private def generateModelScan(
      modelName: String,
      params: List[ExtractedParameter],
      substitution: Substitution,
      subtree: CollectedSubtree
  ): ModelScan =
    // Get the parameter order
    val paramOrder = params.map(_.id)
    
    // Convert bindings to FunctionArgs
    val args = ExpressionValueEncoder.toFunctionArgs(substitution.bindings, paramOrder)
    
    // Use the original subtree's relationType for the schema
    val schema = subtree.root match
      case r: Relation => r.relationType
      case _ => wvlet.lang.model.DataType.EmptyRelationType
    
    ModelScan(
      name = TableName(modelName),
      modelArgs = args,
      schema = schema,
      span = NoSpan
    )

  /**
    * Insert generated ModelDefs into the plan.
    *
    * For PackageDef: prepend models to statements
    * For Query: wrap in PackageDef with models
    * For other: wrap in PackageDef
    */
  private def insertModels(plan: LogicalPlan, models: List[ModelDef]): LogicalPlan =
    if models.isEmpty then
      return plan
    
    plan match
      case p: PackageDef =>
        // Insert models before other statements
        p.copy(statements = models ++ p.statements)
        
      case q: Query =>
        // Wrap in PackageDef with models
        PackageDef(
          name = NameExpr.EmptyName,
          statements = models :+ q,
          span = NoSpan
        )
        
      case r: Relation =>
        // Wrap relation in Query, then in PackageDef
        PackageDef(
          name = NameExpr.EmptyName,
          statements = models :+ Query(r, NoSpan),
          span = NoSpan
        )
        
      case other =>
        // Wrap as-is in PackageDef
        PackageDef(
          name = NameExpr.EmptyName,
          statements = models :+ other,
          span = NoSpan
        )

  /**
    * Apply refactorings to multiple files based on cross-query pattern extraction.
    *
    * This method uses per-file greedy selection:
    * 1. Consider ALL suggestions (including subsumed) sorted by score descending
    * 2. For each file, greedily select non-conflicting occurrences (>= perFileMinOccurrences)
    * 3. Generate shared ModelDefs for patterns that are applied somewhere
    * 4. Transform each file's plan by replacing selected occurrences with ModelScan
    *
    * @param namedPlans List of (fileName, LogicalPlan) tuples
    * @param extraction The cross-query pattern extraction result
    * @param config Configuration for how to apply refactorings
    * @return MultiApplyResult containing shared models and per-file transformed plans
    */
  def applyRefactoringsMultiple(
      namedPlans: List[(String, LogicalPlan)],
      extraction: PatternExtractionResult,
      config: ApplyConfig = ApplyConfig.default
  ): MultiApplyResult =
    if !extraction.hasSuggestions then
      return MultiApplyResult(Nil, namedPlans, Nil, Nil)

    // Use all suggestions (including subsumed) for per-file selection mode
    // Otherwise fall back to optimal-only for backward compatibility
    val baseSuggestions = if config.selectionMode == RefactoringSelectionMode.OptimizeScore then
      extraction.optimalSuggestions
    else if config.usePerFileSelection then
      extraction.allSuggestions
    else if config.useOptimalSuggestions then
      extraction.optimalSuggestions
    else
      extraction.suggestions

    // Filter by shouldRefactor and variable params, then take top K
    val filtered = baseSuggestions
      .filter(s => s.decision.shouldRefactor)
      .filter(s => !config.requireVariableParams || s.unifyResult.exists(_.variableParameters.nonEmpty))
    val candidateSuggestions = if config.topK <= 0 then filtered else filtered.take(config.topK)

    if candidateSuggestions.isEmpty then
      return MultiApplyResult(
        Nil,
        namedPlans,
        Nil,
        List(SkippedRefactoring("all", "No applicable suggestions found"))
      )

    // Sort candidates by score descending (higher score = higher priority)
    // Use model name as tiebreaker for deterministic ordering
    val sortedCandidates = candidateSuggestions.sortBy(s => (-s.decision.score, s.suggestedModelName))

    // Prepare model info for each candidate
    case class CandidateInfo(
        suggestion: RefactoringSuggestion,
        modelName: String,
        varParams: List[ExtractedParameter],
        unifyResult: AntiUnifyResult,
        // Map: sourceId -> List[(subtree, substitution)]
        occurrencesBySource: Map[Option[String], List[(CollectedSubtree, Substitution)]]
    )

    val candidateInfos = sortedCandidates.flatMap { suggestion =>
      suggestion.unifyResult.map { unifyResult =>
        val varParams = unifyResult.variableParameters
        val modelName = s"${config.modelNamePrefix}_${suggestion.suggestedModelName}"
        
        // Group occurrences by sourceId (file name)
        val occurrences = suggestion.group.subtrees
        val occurrencesBySource = occurrences.zipWithIndex.groupBy { case (subtree, _) =>
          subtree.sourceId
        }.map { case (sourceId, items) =>
          val subtreesWithSubs = items.map { case (subtree, occIdx) =>
            val substitution = unifyResult.substitutions.lift(occIdx).getOrElse(
              unifyResult.substitutions.headOption.getOrElse(Substitution(0, Map.empty))
            )
            (subtree, substitution)
          }
          sourceId -> subtreesWithSubs
        }
        
        CandidateInfo(suggestion, modelName, varParams, unifyResult, occurrencesBySource)
      }
    }

    // ===== Per-file greedy selection =====
    // For each file: covered paths, and selected replacements
    val fileNames = namedPlans.map(_._1).toSet
    
    // Track covered paths per file: fileName -> Set[path]
    val coveredPathsPerFile = scala.collection.mutable.Map[String, scala.collection.mutable.Set[List[Int]]]()
    fileNames.foreach(f => coveredPathsPerFile(f) = scala.collection.mutable.Set.empty)
    
    // Track selected occurrences: fileName -> List[(modelName, varParams, subtree, substitution)]
    case class SelectedReplacement(
        modelName: String,
        varParams: List[ExtractedParameter],
        subtree: CollectedSubtree,
        substitution: Substitution
    )
    val selectedPerFile = scala.collection.mutable.Map[String, scala.collection.mutable.ListBuffer[SelectedReplacement]]()
    fileNames.foreach(f => selectedPerFile(f) = scala.collection.mutable.ListBuffer.empty)
    
    // Track which models are actually used (so we only generate models that have at least one replacement)
    val usedModelNames = scala.collection.mutable.Set[String]()
    
    // Process candidates in score order
    candidateInfos.foreach { info =>
      // Check variable params requirement
      if config.requireVariableParams && info.varParams.isEmpty then
        // Skip silently (already filtered, but just in case)
        ()
      else
        // Count total remaining occurrences across all files (for cross-file patterns)
        var totalRemainingAcrossFiles = 0
        val remainingByFile = scala.collection.mutable.Map[String, List[(CollectedSubtree, Substitution)]]()
        
        fileNames.foreach { fileName =>
          val fileOccurrences = info.occurrencesBySource.getOrElse(Some(fileName), Nil)
          val coveredPaths = coveredPathsPerFile(fileName)
          
          // Filter to non-conflicting occurrences
          val remaining = fileOccurrences.filter { case (subtree, _) =>
            !coveredPaths.exists(coveredPath => isPathOverlap(subtree.path, coveredPath))
          }
          
          if remaining.nonEmpty then
            remainingByFile(fileName) = remaining
            totalRemainingAcrossFiles += remaining.size
        }
        
        // Apply if at least one non-conflicting occurrence remains.
        // Note: Refactoring candidates are already filtered by minOccurrences during analysis;
        // here we only drop occurrences due to conflicts.
        if totalRemainingAcrossFiles >= 1 then
          remainingByFile.foreach { case (fileName, remaining) =>
            remaining.foreach { case (subtree, substitution) =>
              selectedPerFile(fileName) += SelectedReplacement(info.modelName, info.varParams, subtree, substitution)
              coveredPathsPerFile(fileName) += subtree.path
            }
          }
          usedModelNames += info.modelName
    }
    
    // ===== Generate models for used candidates =====
    val generatedModels = candidateInfos
      .filter(info => usedModelNames.contains(info.modelName))
      .map(info => generateModelDef(info.modelName, info.unifyResult))
      .toList
    
    // ===== Build applied/skipped lists =====
    val applied = scala.collection.mutable.ListBuffer[AppliedRefactoring]()
    val skipped = scala.collection.mutable.ListBuffer[SkippedRefactoring]()
    
    candidateInfos.foreach { info =>
      if usedModelNames.contains(info.modelName) then
        // Count total occurrences replaced across all files
        val totalReplaced = selectedPerFile.values.flatten.count(_.modelName == info.modelName)
        applied += AppliedRefactoring(
          suggestionId = info.modelName,
          modelDef = generateModelDef(info.modelName, info.unifyResult),
          occurrencesReplaced = totalReplaced,
          nodeReduction = info.suggestion.decision.estimatedReduction
        )
      else
        skipped += SkippedRefactoring(info.modelName, "No non-conflicting occurrences remained")
    }

    // ===== Apply replacements to each file's plan =====
    val transformedPlans = namedPlans.map { case (fileName, plan) =>
      val selections = selectedPerFile(fileName).toList
      
      if selections.isEmpty then
        (fileName, plan)
      else
        val replacements = selections.map { sel =>
          val modelScan = generateModelScan(sel.modelName, sel.varParams, sel.substitution, sel.subtree)
          (sel.subtree.path, modelScan)
        }
        val transformedPlan = PathRewriter.replaceMany(plan, replacements)
        (fileName, transformedPlan)
    }

    MultiApplyResult(
      models = generatedModels,
      fileResults = transformedPlans,
      applied = applied.toList,
      skipped = skipped.toList
    )

end RefactoringApplier
