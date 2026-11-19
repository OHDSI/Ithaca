#' Run a documented example Ithaca pipeline
#'
#' Executes the standard callable flow:
#' rubric creation, optional pre-run edit, concept set creation,
#' scoring, and optional post-run edit.
#'
#' @param conditionSpecs List of condition specifications.
#' @param databases List of database specifications.
#' @param outputDirectory Root output directory.
#' @param promptDir Optional prompt directory override.
#' @param rubricTemplates Optional rubric template list.
#' @param includeLabs Logical; include labs in rubric prompts.
#' @param useLibraryHints Logical; when `TRUE`, run concept set library hint suggestions during rubric creation.
#' @param libraryHintSimilarityThreshold Similarity threshold used for concept set hint candidate retrieval.
#' @param libraryHintTopCandidates Number of top concept set candidates to include per needed concept set.
#' @param useExistingRubric Logical; when `TRUE`, skip rubric creation for conditions with existing rubric artifacts.
#' @param seedLibraryPath Seed concept set library path.
#' @param useLlmAdjudication Logical; when `TRUE`, ambiguous concept-set matches are adjudicated by LLM.
#' @param llmTopCandidates Number of top embedding candidates passed to LLM adjudication.
#' @param llmAmbiguityMinScore Optional lower bound of embedding score gray-zone for adjudication trigger.
#' @param llmAmbiguityMaxScore Optional upper bound of embedding score gray-zone for adjudication trigger.
#' @param llmTop2GapThreshold Maximum top1-top2 embedding gap considered ambiguous.
#' @param llmConfidenceThreshold Minimum LLM confidence to accept adjudicated decisions.
#' @param conceptLibraryVersion Concept set library version used for scoring. `NULL` (default), blank, or `"latest"` uses the newest library version; a positive integer pins that snapshot.
#' @param generateCohorts Logical; generate cohorts before scoring.
#' @param cohortDefinitionSet Cohort definition set.
#' @param cohortTableName Cohort table name input used during cohort generation and scoring.
#' Can be `NULL`, a single table name for all databases, a named character/list keyed by
#' `databaseId`, or an unnamed character/list with one value per database (database order).
#' @param webapiBaseUrl Optional OHDSI WebAPI base URL used when `cohortDefinitionSet` is `NULL`.
#' @param preRunEdit Optional list with `conditionName`, `ruleId`, `updates`, `regenerateSql`.
#' @param postRunEdit Optional list with `conditionName`, `ruleId`, `updates`, `regenerateSql`.
#' @param useExistingScores Logical; reuse existing score files.
#' @param llmClientReasoning LLM client for reasoning tasks.
#' @param llmClientNonReasoning LLM client for non-reasoning tasks.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return List containing the workflow object and score results.
#' @examples
#' \dontrun{
#' out <- runIthacaExample(
#'   conditionSpecs = list(
#'     list(
#'       testCondition = "Gastroesophageal reflux disease",
#'       dxCriteria = "1 inpatient/ED dx OR 2 outpatient dx",
#'       additionalInformation = "",
#'       rubricName = "Gastroesophageal reflux disease",
#'       randomCohortDefinitionId = 14412
#'     )
#'   ),
#'   databases = list(CCAE_OHDA_DBSpec),
#'   outputDirectory = "P:/llm/rubrics",
#'   generateCohorts = FALSE
#' )
#' }
#' @export
runIthacaExample <- function(
    conditionSpecs,
    databases,
    outputDirectory = "P:/llm/rubrics",
    promptDir = NULL,
    rubricTemplates = NULL,
    includeLabs = FALSE,
    useLibraryHints = FALSE,
    libraryHintSimilarityThreshold = 0.94,
    libraryHintTopCandidates = 5,
    useExistingRubric = FALSE,
    seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
    useLlmAdjudication = TRUE,
    llmTopCandidates = 25,
    llmAmbiguityMinScore = NULL,
    llmAmbiguityMaxScore = NULL,
    llmTop2GapThreshold = 0.015,
    llmConfidenceThreshold = 0.60,
    conceptLibraryVersion = NULL,
    generateCohorts = FALSE,
    cohortDefinitionSet = NULL,
    cohortTableName = NULL,
    webapiBaseUrl = NULL,
    preRunEdit = NULL,
    postRunEdit = NULL,
    useExistingScores = FALSE,
    llmClientReasoning = ithGetLlmClientFromConfig(list(), "reasoning"),
    llmClientNonReasoning = ithGetLlmClientFromConfig(list(), "nonReasoning"),
    verbose = TRUE) {

  workflow <- IthacaWorkflow$new(
    conditionSpecs = conditionSpecs,
    databases = databases,
    outputDirectory = outputDirectory,
    promptDir = promptDir,
    llmClient = llmClientReasoning,
    llmClientNonReasoning = llmClientNonReasoning,
    verbose = verbose
  )

  workflow$createRubrics(
    rubricTemplates = rubricTemplates,
    includeLabs = includeLabs,
    useLibraryHints = useLibraryHints,
    libraryHintSimilarityThreshold = libraryHintSimilarityThreshold,
    libraryHintTopCandidates = libraryHintTopCandidates,
    useExistingRubric = useExistingRubric
  )

  if (!is.null(preRunEdit)) {
    workflow$editRubric(
      conditionName = preRunEdit$conditionName,
      ruleId = preRunEdit$ruleId,
      updates = preRunEdit$updates,
      regenerateSql = isTRUE(preRunEdit$regenerateSql)
    )
  }

  workflow$createConceptSets(
    seedLibraryPath = seedLibraryPath,
    useLlmAdjudication = useLlmAdjudication,
    llmTopCandidates = llmTopCandidates,
    llmAmbiguityMinScore = llmAmbiguityMinScore,
    llmAmbiguityMaxScore = llmAmbiguityMaxScore,
    llmTop2GapThreshold = llmTop2GapThreshold,
    llmConfidenceThreshold = llmConfidenceThreshold
  )

  scoreResults <- workflow$scoreSubjects(
    useExisting = useExistingScores,
    seedLibraryPath = seedLibraryPath,
    conceptLibraryVersion = conceptLibraryVersion,
    generateCohorts = generateCohorts,
    cohortDefinitionSet = cohortDefinitionSet,
    cohortTableName = cohortTableName,
    webapiBaseUrl = webapiBaseUrl
  )

  if (!is.null(postRunEdit)) {
    workflow$postRunEdit(
      conditionName = postRunEdit$conditionName,
      ruleId = postRunEdit$ruleId,
      updates = postRunEdit$updates,
      regenerateSql = isTRUE(postRunEdit$regenerateSql)
    )
  }

  list(
    workflow = workflow,
    scoreResults = scoreResults
  )
}

# Example (not executed automatically):
# results <- runIthacaExample(
#   conditionSpecs = list(
#     list(
#       testCondition = "Gastroesophageal reflux disease",
#       dxCriteria = "1 inpatient/ED dx OR 2 outpatient dx 1-60 days apart",
#       additionalInformation = "",
#       rubricName = "Gastroesophageal reflux disease",
#'       randomCohortDefinitionId = 14412
#     )
#   ),
#   databases = list(CCAE_OHDA_DBSpec, MDCR_OHDA_DBSpec),
#   outputDirectory = "P:/llm/rubrics",
#   generateCohorts = FALSE
# )
