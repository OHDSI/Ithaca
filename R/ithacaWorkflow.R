#' Ithaca end-to-end workflow class
#'
#' Orchestrates rubric creation, optional edits, concept set generation,
#' cohort generation, and subject scoring.
#'
#' @field conditionSpecs List of condition specifications used across workflow stages.
#' @field databases List of database specifications.
#' @field outputDirectory Root output directory for workflow artifacts.
#' @field promptDir Optional prompt directory override.
#' @field llmClient LLM client used for reasoning/structured tasks.
#' @field llmClientNonReasoning LLM client used for non-reasoning tasks.
#' @field verbose Logical; when `TRUE`, print progress messages.
#'
#' @param conditionSpecs List of condition specifications.
#' @param databases List of database specifications.
#' @param outputDirectory Root output directory.
#' @param promptDir Optional prompt directory override.
#' @param llmClient LLM client for reasoning tasks.
#' @param llmClientNonReasoning LLM client for non-reasoning tasks.
#' @param verbose Logical; when `TRUE`, print progress messages.
#' @param rubricTemplates Optional rubric template list.
#' @param includeLabs Logical; include labs in rubric generation prompts.
#' @param useLibraryHints Logical; when `TRUE`, run concept set library hint suggestions during rubric creation.
#' @param libraryHintSimilarityThreshold Similarity threshold used for concept set hint candidate retrieval.
#' @param libraryHintTopCandidates Number of top concept set candidates to include per needed concept set.
#' @param useExistingRubric Logical; when `TRUE`, skip rubric creation for conditions with existing rubric artifacts.
#' @param conditionName Condition/rubric name.
#' @param ruleId Rule identifier.
#' @param updates Named list of field replacements.
#' @param regenerateSql Logical; regenerate SQL when criteria/concepts are changed.
#' @param seedLibraryPath Seed concept set library path.
#' @param useLlmAdjudication Logical; when `TRUE`, ambiguous concept-set matches are adjudicated by LLM.
#' @param llmTopCandidates Number of top embedding candidates passed to LLM adjudication.
#' @param llmAmbiguityMinScore Optional lower bound of embedding-score ambiguity range for LLM adjudication.
#' @param llmAmbiguityMaxScore Optional upper bound of embedding-score ambiguity range for LLM adjudication.
#' @param llmTop2GapThreshold Maximum top-1 minus top-2 embedding gap still considered ambiguous.
#' @param llmConfidenceThreshold Minimum LLM confidence required to accept adjudicated decisions.
#' @param useExisting Logical; reuse existing scoring outputs.
#' @param conceptLibraryVersion Concept set library version used for scoring. `NULL` (default), blank, or `"latest"` uses the newest library version; a positive integer pins that snapshot.
#' @param generateCohorts Logical; generate cohorts before scoring.
#' @param cohortDefinitionSet Cohort definitions with `cohortId`, `cohortName`, and `sql`.
#' @param cohortTableName Cohort table name input used during cohort generation and scoring.
#' Can be `NULL`, a single table name for all databases, a named character/list keyed by
#' `databaseId`, or an unnamed character/list with one value per database (workflow database order).
#' @param webapiBaseUrl Optional OHDSI WebAPI base URL used when `cohortDefinitionSet` is `NULL`.
#'
#' @section Methods:
#' \describe{
#'   \item{`new(conditionSpecs, databases, outputDirectory, promptDir = NULL, llmClient = ithGetLlmClientFromConfig(list(), "reasoning"), llmClientNonReasoning = ithGetLlmClientFromConfig(list(), "nonReasoning"), verbose = TRUE)`}{Initialize the workflow object.}
#'   \item{`createRubrics(rubricTemplates = NULL, includeLabs = FALSE, useLibraryHints = FALSE, libraryHintSimilarityThreshold = 0.94, libraryHintTopCandidates = 5, useExistingRubric = FALSE)`}{Generate rubrics for all configured conditions.}
#'   \item{`editRubric(conditionName, ruleId, updates, regenerateSql = TRUE)`}{Edit one rule and optionally regenerate SQL.}
#'   \item{`createConceptSets(seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb", useLlmAdjudication = TRUE, llmTopCandidates = 25, llmTop2GapThreshold = 0.015, llmConfidenceThreshold = 0.60)`}{Create or update concept sets needed by the generated rubrics.}
#'   \item{`scoreSubjects(useExisting = FALSE, seedLibraryPath = NULL, conceptLibraryVersion = NULL, generateCohorts = FALSE, cohortDefinitionSet = NULL, cohortTableName = NULL, webapiBaseUrl = NULL)`}{Score subjects across configured databases.}
#'   \item{`postRunEdit(conditionName, ruleId, updates, regenerateSql = TRUE)`}{Convenience alias for `editRubric()`.}
#'   \item{`runFull(rubricTemplates = NULL, includeLabs = FALSE, useLibraryHints = FALSE, libraryHintSimilarityThreshold = 0.94, libraryHintTopCandidates = 5, useExistingRubric = FALSE, seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb", useLlmAdjudication = TRUE, llmTopCandidates = 25, llmTop2GapThreshold = 0.015, llmConfidenceThreshold = 0.60, conceptLibraryVersion = NULL, useExisting = FALSE, generateCohorts = FALSE, cohortDefinitionSet = NULL, cohortTableName = NULL, webapiBaseUrl = NULL)`}{Run rubric generation, concept set creation, and scoring end-to-end, using `seedLibraryPath` for both concept-set initialization and scoring library resolution.}
#' }
#'
#' @examples
#' \dontrun{
#' wf <- IthacaWorkflow$new(
#'   conditionSpecs = list(list(testCondition = "GERD", dxCriteria = "...", rubricName = "GERD", randomCohortDefinitionId = 14412)),
#'   databases = list(CCAE_OHDA_DBSpec),
#'   outputDirectory = "P:/llm/rubrics"
#' )
#' }
#' @export
IthacaWorkflow <- R6::R6Class(
  "IthacaWorkflow",
  public = list(
    conditionSpecs = NULL,
    databases = NULL,
    outputDirectory = NULL,
    promptDir = NULL,
    llmClient = NULL,
    llmClientNonReasoning = NULL,
    verbose = TRUE,

    #' @description Initialize a new `IthacaWorkflow` instance.
    initialize = function(conditionSpecs,
                          databases,
                          outputDirectory,
                          promptDir = NULL,
                          llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                          llmClientNonReasoning = ithGetLlmClientFromConfig(list(), "nonReasoning"),
                          verbose = TRUE) {
      ithMaybeMessage(verbose, "Initializing IthacaWorkflow: ", length(conditionSpecs), " condition(s), ", length(databases), " database(s).")
      self$conditionSpecs <- conditionSpecs
      self$databases <- databases
      self$outputDirectory <- outputDirectory
      self$promptDir <- promptDir
      self$llmClient <- llmClient
      self$llmClientNonReasoning <- llmClientNonReasoning
      self$verbose <- verbose
    },

    #' @description Create rubrics for all configured conditions.
    createRubrics = function(rubricTemplates = NULL,
                             includeLabs = FALSE,
                             useLibraryHints = FALSE,
                             libraryHintSimilarityThreshold = 0.94,
                             libraryHintTopCandidates = 5,
                             useExistingRubric = FALSE) {
      ithMaybeMessage(self$verbose, "Starting rubric generation...")
      ithCreateRubrics(
        conditionSpecs = self$conditionSpecs,
        outputDirectory = self$outputDirectory,
        rubricTemplates = rubricTemplates,
        llmClient = self$llmClient,
        promptDir = self$promptDir,
        includeLabs = includeLabs,
        useLibraryHints = useLibraryHints,
        libraryHintSimilarityThreshold = libraryHintSimilarityThreshold,
        libraryHintTopCandidates = libraryHintTopCandidates,
        useExistingRubric = useExistingRubric,
        verbose = self$verbose
      )
      ithMaybeMessage(self$verbose, "Rubric generation completed.")
      invisible(self)
    },

    #' @description Edit a rubric rule and optionally regenerate SQL.
    editRubric = function(conditionName, ruleId, updates, regenerateSql = TRUE) {
      ithMaybeMessage(self$verbose, "Editing rubric rule ", ruleId, " for condition: ", conditionName, ".")
      shortRubric <- ithLoadLatestShortRubric(conditionName, self$outputDirectory)
      updated <- ithUpdateRubricRule(
        shortRubric = shortRubric,
        ruleId = ruleId,
        updates = updates,
        regenerateSql = regenerateSql,
        llmClient = self$llmClient,
        promptDir = self$promptDir,
        verbose = self$verbose
      )
      ithSaveShortRubric(updated, conditionName, self$outputDirectory)
      ithMaybeMessage(self$verbose, "Rubric edit saved for condition: ", conditionName, ".")
      invisible(self)
    },

    #' @description Create or update concept sets needed by generated rubrics.
    createConceptSets = function(seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
                   useLlmAdjudication = TRUE,
                   llmTopCandidates = 25,
                   llmAmbiguityMinScore = NULL,
                   llmAmbiguityMaxScore = NULL,
                   llmTop2GapThreshold = 0.015,
                   llmConfidenceThreshold = 0.60) {
      ithMaybeMessage(self$verbose, "Starting concept set workflow...")
      conceptLibraryLocation <- ithResolveConceptLibraryLocation(
        libraryPath = seedLibraryPath,
        defaultFileName = "rubricConceptSetLibrary.duckdb"
      )
      ithMaybeMessage(self$verbose, "Using concept set library from: ", conceptLibraryLocation$resolvedPath)

      ithCreateRubricConceptSets(
        conditionSpecs = self$conditionSpecs,
        outputDirectory = self$outputDirectory,
        conceptLibraryDirectory = conceptLibraryLocation$libraryDirectory,
        database = self$databases[[1]],
        llmClientReasoning = self$llmClient,
        llmClientNonReasoning = self$llmClientNonReasoning,
        conceptLibraryFile = conceptLibraryLocation$libraryFileName,
        useLlmAdjudication = useLlmAdjudication,
        llmTopCandidates = llmTopCandidates,
        llmAmbiguityMinScore = llmAmbiguityMinScore,
        llmAmbiguityMaxScore = llmAmbiguityMaxScore,
        llmTop2GapThreshold = llmTop2GapThreshold,
        llmConfidenceThreshold = llmConfidenceThreshold,
        verbose = self$verbose
      )
      ithMaybeMessage(self$verbose, "Concept set workflow completed.")
      invisible(self)
    },

    #' @description Score subjects across configured databases.
    scoreSubjects = function(useExisting = FALSE,
                              seedLibraryPath = NULL,
                              conceptLibraryVersion = NULL,
                              generateCohorts = FALSE,
                              cohortDefinitionSet = NULL,
                              cohortTableName = NULL,
                              webapiBaseUrl = NULL) {
      ithMaybeMessage(self$verbose, "Starting subject scoring workflow...")
      ithScoreSubjects(
        conditionSpecs = self$conditionSpecs,
        databases = self$databases,
        outputDirectory = self$outputDirectory,
        seedLibraryPath = seedLibraryPath,
        conceptLibraryVersion = conceptLibraryVersion,
        cohortTableName = cohortTableName,
        useExisting = useExisting,
        generateCohorts = generateCohorts,
        cohortDefinitionSet = cohortDefinitionSet,
        webapiBaseUrl = webapiBaseUrl,
        llmClient = self$llmClient,
        promptDir = self$promptDir,
        verbose = self$verbose
      )
    },

    #' @description Convenience wrapper for `editRubric()` after a run.
    postRunEdit = function(conditionName, ruleId, updates, regenerateSql = TRUE) {
      self$editRubric(conditionName, ruleId, updates, regenerateSql = regenerateSql)
    },

    #' @description Run the full workflow end-to-end.
    runFull = function(rubricTemplates = NULL,
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
                        useExisting = FALSE,
                        generateCohorts = FALSE,
                        cohortDefinitionSet = NULL,
                        cohortTableName = NULL,
                        webapiBaseUrl = NULL) {
                  ithMaybeMessage(self$verbose, "Running full Ithaca workflow...")
      self$createRubrics(
        rubricTemplates = rubricTemplates,
        includeLabs = includeLabs,
        useLibraryHints = useLibraryHints,
        libraryHintSimilarityThreshold = libraryHintSimilarityThreshold,
        libraryHintTopCandidates = libraryHintTopCandidates,
        useExistingRubric = useExistingRubric
      )
      self$createConceptSets(
        seedLibraryPath = seedLibraryPath,
        useLlmAdjudication = useLlmAdjudication,
        llmTopCandidates = llmTopCandidates,
        llmAmbiguityMinScore = llmAmbiguityMinScore,
        llmAmbiguityMaxScore = llmAmbiguityMaxScore,
        llmTop2GapThreshold = llmTop2GapThreshold,
        llmConfidenceThreshold = llmConfidenceThreshold
      )
      self$scoreSubjects(
        useExisting = useExisting,
        seedLibraryPath = seedLibraryPath,
        conceptLibraryVersion = conceptLibraryVersion,
        generateCohorts = generateCohorts,
        cohortDefinitionSet = cohortDefinitionSet,
        cohortTableName = cohortTableName,
        webapiBaseUrl = webapiBaseUrl
      )
      ithMaybeMessage(self$verbose, "Full Ithaca workflow completed.")
    }
  )
)

#' Run the full Ithaca workflow
#'
#' @param conditionSpecs List of condition specifications.
#' @param databases List of database specifications.
#' @param outputDirectory Root output directory.
#' @param promptDir Optional prompt directory override.
#' @param rubricTemplates Optional rubric template list.
#' @param includeLabs Logical; include labs in rubric generation prompts.
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
#' @param useExistingScores Logical; reuse existing scoring outputs.
#' @param generateCohorts Logical; generate cohorts before scoring.
#' @param cohortDefinitionSet Cohort definitions with `cohortId`, `cohortName`, `sql`.
#' @param cohortTableName Cohort table name input used during cohort generation and scoring.
#' Can be `NULL`, a single table name for all databases, a named character/list keyed by
#' `databaseId`, or an unnamed character/list with one value per database (database order).
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return `IthacaWorkflow` object.
#' @examples
#' \dontrun{
#' workflow <- runIthacaWorkflow(
#'   conditionSpecs = list(list(testCondition = "GERD", dxCriteria = "...", rubricName = "GERD", randomCohortDefinitionId = 14412)),
#'   databases = list(CCAE_OHDA_DBSpec),
#'   outputDirectory = "P:/llm/rubrics",
#'   seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   conceptLibraryVersion = 142,
#'   generateCohorts = FALSE
#' )
#' }
#' @export
runIthacaWorkflow <- function(conditionSpecs,
                                databases,
                                outputDirectory,
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
                                useExistingScores = FALSE,
                                generateCohorts = FALSE,
                                cohortDefinitionSet = NULL,
                                cohortTableName = NULL,
                                webapiBaseUrl = NULL,
                                verbose = TRUE) {
  ithMaybeMessage(verbose, "Launching runIthacaWorkflow...")
  workflow <- IthacaWorkflow$new(
    conditionSpecs = conditionSpecs,
    databases = databases,
    outputDirectory = outputDirectory,
    promptDir = promptDir,
    verbose = verbose
  )

  workflow$runFull(
    rubricTemplates = rubricTemplates,
    includeLabs = includeLabs,
    useLibraryHints = useLibraryHints,
    libraryHintSimilarityThreshold = libraryHintSimilarityThreshold,
    libraryHintTopCandidates = libraryHintTopCandidates,
    useExistingRubric = useExistingRubric,
    seedLibraryPath = seedLibraryPath,
    useLlmAdjudication = useLlmAdjudication,
    llmTopCandidates = llmTopCandidates,
    llmAmbiguityMinScore = llmAmbiguityMinScore,
    llmAmbiguityMaxScore = llmAmbiguityMaxScore,
    llmTop2GapThreshold = llmTop2GapThreshold,
    llmConfidenceThreshold = llmConfidenceThreshold,
    conceptLibraryVersion = conceptLibraryVersion,
    useExisting = useExistingScores,
    generateCohorts = generateCohorts,
    cohortDefinitionSet = cohortDefinitionSet,
    cohortTableName = cohortTableName,
    webapiBaseUrl = webapiBaseUrl
  )

  ithMaybeMessage(verbose, "runIthacaWorkflow completed.")
  invisible(workflow)
}

getEmbeddingVectors <- function(text, reportTime = TRUE, silent = FALSE) {
  startTime <- Sys.time()
  inputs <- as.character(text)
  vectors <- list()

  for (up in seq_along(inputs)) {
    payload <- jsonlite::toJSON(list(input = inputs[[up]]), auto_unbox = TRUE)
    response <- httr::RETRY(
      "POST",
      url = ithGetCredential("GENAI_EMBED_ENDPOINT"),
      body = payload,
      httr::add_headers(
        "Content-Type" = "application/json",
        "api-key" = ithGetCredential("GENAI_API_EMBED_KEY")
      ),
      times = 3,
      pauseBase = 1
    )

    parsed <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    vectors[[up]] <- parsed$data$embedding[[1]]
  }

  if (reportTime && !silent) {
    message("Embedding generation took: ", round(as.numeric(Sys.time() - startTime, units = "secs"), 2), " sec")
  }

  vectors
}

#' Save concept set to in-memory library object
#'
#' @param cleanName Human-readable concept set name.
#' @param conceptSetName Canonical concept set key.
#' @param finalConceptSet Concept set expression/object.
#' @param csLibraryName Existing two-table concept set library object.
#'
#' @return Updated concept set library object.
#' @examples
#' csLibrary <- list(
#'   data.frame(conceptSetName = character(), cleanName = character(), conceptSet = I(list())),
#'   data.frame(conceptSetName = character(), cleanName = character(), embed = I(list()))
#' )
#' \dontrun{
#' updated <- saveConceptSet(
#'   cleanName = "heart failure",
#'   conceptSetName = "heart_failure",
#'   finalConceptSet = list(items = list()),
#'   csLibraryName = csLibrary
#' )
#' }
#' @export
saveConceptSet <- function(cleanName, conceptSetName, finalConceptSet, csLibraryName) {
  embedded <- data.frame(
    conceptSetName = conceptSetName,
    cleanName = cleanName,
    embed = I(getEmbeddingVectors(tolower(cleanName), reportTime = FALSE, silent = TRUE)),
    stringsAsFactors = FALSE
  )

  finalDf <- data.frame(
    conceptSetName = conceptSetName,
    cleanName = cleanName,
    conceptSet = I(list(finalConceptSet)),
    stringsAsFactors = FALSE
  )

  csLibraryName[[2]] <- rbind(csLibraryName[[2]], embedded)
  csLibraryName[[1]] <- rbind(csLibraryName[[1]], finalDf)
  csLibraryName
}
