#' Create rubric sets for one or more conditions
#'
#' @param conditionList List of condition specifications.
#' @param outputDirectory Root output directory for rubric artifacts.
#' @param rubricTemplates Optional rubric template list.
#' @param includeLabs Logical; include lab values and results in prompts.
#' @param promptDir Optional prompt folder override.
#' @param llmClient Structured chat client used for rubric generation.
#' @param useLibraryHints Logical; when `TRUE`, run a second-pass concept set naming suggestion step using library candidates as preferences.
#' @param libraryHintSimilarityThreshold Similarity threshold used to build preferred library candidates for hinting.
#' @param libraryHintTopCandidates Number of top candidates to provide per needed concept set during hinting.
#' @param useExistingRubric Logical; when `TRUE`, skip rubric creation for conditions with existing rubric artifacts.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisibly returns generated files in `outputDirectory`.
#' @examples
#' \dontrun{
#' conditionList <- list(
#'   list(
#'     testCondition = "Gastroesophageal reflux disease",
#'     dxCriteria = "1 inpatient/ED dx OR 2 outpatient dx",
#'     additionalInformation = ""
#'   )
#' )
#' getRubrics(conditionList, outputDirectory = "P:/llm/rubrics")
#' }
#' @export
getRubrics <- function(conditionList,
                       outputDirectory,
                       rubricTemplates = NULL,
                       includeLabs = FALSE,
                       promptDir = NULL,
                       llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                       useLibraryHints = FALSE,
                       libraryHintSimilarityThreshold = 0.94,
                       libraryHintTopCandidates = 5,
                       useExistingRubric = FALSE,
                       verbose = TRUE) {
  ithCreateRubrics(
    conditionSpecs = conditionList,
    outputDirectory = outputDirectory,
    rubricTemplates = rubricTemplates,
    llmClient = llmClient,
    promptDir = promptDir,
    includeLabs = includeLabs,
    useLibraryHints = useLibraryHints,
    libraryHintSimilarityThreshold = libraryHintSimilarityThreshold,
    libraryHintTopCandidates = libraryHintTopCandidates,
    useExistingRubric = useExistingRubric,
    verbose = verbose
  )
}

#' Render one rubric rule as executable SQL using the first configured database
#'
#' @param conditionName Rubric name or test condition name from the configured condition set.
#' @param rubricId Rubric identifier, for example `R1`.
#' @param ruleId Rule identifier, for example `ER1`; defaults to `1`.
#' @param configPath Path to YAML configuration file.
#'
#' @return A list containing rendered translated `sql`, raw `sqlStatement`, database metadata,
#'   and resolved concept-set plugin details. The function does not execute the returned SQL.
#' @examples
#' \dontrun{
#' ruleSql <- getRubricRuleSqlFromConfig(
#'   conditionName = "Rheumatoid arthritis",
#'   rubricId = "R1",
#'   ruleId = "ER1"
#' )
#' cat(ruleSql$sql)
#' }
#' @export
getRubricRuleSqlFromConfig <- function(conditionName,
                                       rubricId,
                                       ruleId = "1",
                                       configPath = "config.yaml") {
  config <- Ithaca:::ithReadIthacaWorkflowConfig(configPath)
  databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = .GlobalEnv)
  if (length(databases) == 0L) {
    stop("No databases are configured")
  }

  conditionNames <- vapply(
    config$conditionSpecs,
    function(spec) as.character(Ithaca:::ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  conditionIndex <- which(conditionNames == conditionName)
  if (length(conditionIndex) != 1L) {
    stop("conditionName must match exactly one configured condition: ", conditionName)
  }
  condition <- config$conditionSpecs[[conditionIndex]]
  database <- databases[[1]]
  databaseId <- as.character(database$databaseId[[1]])
  conditionNameResolved <- as.character(condition$rubricName[[1]])
  outputDirectory <- as.character(config$outputDirectory[[1]])
  conditionDirectory <- file.path(outputDirectory, conditionNameResolved)

  shortRubric <- Ithaca:::ithLoadLatestShortRubric(conditionNameResolved, outputDirectory)
  matchIndex <- which(
    as.character(shortRubric$rubric) == as.character(rubricId) &
      as.character(shortRubric$ruleId) == as.character(ruleId)
  )
  if (length(matchIndex) != 1L) {
    stop(
      "Expected exactly one matching rubric/rule row for rubricId=", rubricId,
      ", ruleId=", ruleId, "; found ", length(matchIndex)
    )
  }

  scoringDirectory <- file.path(outputDirectory, "scoring", conditionNameResolved, databaseId)
  matchedConceptSets <- Ithaca:::ithLoadLatest(conditionDirectory, "matchedConceptSets.rds")
  if (is.null(matchedConceptSets)) {
    stop("No matchedConceptSets artifact found in: ", conditionDirectory)
  }

  conceptSetLibrary <- Ithaca:::ithLoadConceptSetLibraryForScoring(
    outputDirectory = outputDirectory,
    seedLibraryPath = config$seedLibraryPath,
    libraryVersion = config$conceptLibraryVersion
  )
  conceptSetJsonLibrary <- conceptSetLibrary[[1]]
  sqlStatement <- as.character(shortRubric$sqlStatement[[matchIndex]])
  plugins <- unique(stringr::str_extract_all(sqlStatement, "\\B@[A-Za-z0-9_]+")[[1]])
  plugins <- plugins[!(plugins %in% c("@databaseSchema", "@cohortSchema", "@cohortTable", "@cohortId"))]
  pluginDetails <- list()

  if (length(plugins) > 0L) {
    pluginNames <- substr(plugins, 2, nchar(plugins))
    pluginDetails <- lapply(
      pluginNames,
      Ithaca:::ithResolveConceptSetPluginDetails,
      matchedConceptSets = matchedConceptSets,
      conceptSetJsonLibrary = conceptSetJsonLibrary,
      database = database
    )
    statuses <- vapply(pluginDetails, function(detail) as.character(detail$status), character(1))
    if (any(!grepl("^resolved", statuses))) {
      unresolved <- paste0(pluginNames[!grepl("^resolved", statuses)], " (", statuses[!grepl("^resolved", statuses)], ")")
      stop("Could not resolve concept-set plugin(s): ", paste(unresolved, collapse = ", "))
    }
    for (pluginIndex in seq_along(plugins)) {
      sqlStatement <- gsub(plugins[[pluginIndex]], pluginDetails[[pluginIndex]]$value, sqlStatement, fixed = TRUE)
    }
  }

  cohortTable <- Ithaca:::ithDefaultCohortTable(database)
  renderedSql <- SqlRender::render(
    sqlStatement,
    databaseSchema = database$cdmDatabaseSchema,
    cohortSchema = database$workDatabaseSchema,
    cohortTable = cohortTable,
    cohortId = as.integer(condition$randomCohortDefinitionId[[1]])
  )
  translatedSql <- SqlRender::translate(renderedSql, database$connectionDetails$dbms)

  list(
    sql = translatedSql,
    rawSqlStatement = as.character(shortRubric$sqlStatement[[matchIndex]]),
    conditionName = conditionNameResolved,
    databaseId = databaseId,
    rubricId = as.character(rubricId),
    ruleId = as.character(ruleId),
    cohortTable = cohortTable,
    pluginDetails = pluginDetails
  )
}

#' Reset rubric checkpoint state for a condition
#'
#' Removes the condition-level rubric checkpoint file used for resume behavior.
#' Optionally removes existing generated rubric artifacts for a full clean rebuild.
#'
#' @param conditionName Condition/rubric name.
#' @param outputDirectory Root output directory for rubric artifacts.
#' @param deleteArtifacts Logical; when `TRUE`, also remove versioned rubric artifact files.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisible list with removed checkpoint/artifact file paths.
#' @examples
#' \dontrun{
#' resetRubricCheckpoint(
#'   conditionName = "Gastroesophageal reflux disease",
#'   outputDirectory = "P:/llm/rubrics",
#'   deleteArtifacts = FALSE
#' )
#' }
#' @export
resetRubricCheckpoint <- function(conditionName,
                                  outputDirectory,
                                  deleteArtifacts = FALSE,
                                  verbose = TRUE) {
  ithResetRubricCheckpoint(
    conditionName = conditionName,
    outputDirectory = outputDirectory,
    deleteArtifacts = deleteArtifacts,
    verbose = verbose
  )
}

#' Build or update rubric concept sets
#'
#' @param conditionList List of condition specifications.
#' @param outputDirectory Root output directory for rubric artifacts.
#' @param database Database specification list/object.
#' @param seedLibraryPath Seed concept set library file path, or a directory
#' containing DuckDB file `rubricConceptSetLibrary.duckdb`.
#' @param llmClientReasoning Reasoning model client.
#' @param llmClientNonReasoning Non-reasoning model client.
#' @param similarityThreshold Cosine similarity threshold for library matching.
#' @param fuzzyThreshold Fuzzy-name similarity threshold for name-based matching.
#' @param useLlmAdjudication Logical; when `TRUE`, ambiguous matches are adjudicated by LLM using top candidate sets.
#' @param llmTopCandidates Number of top embedding candidates to provide to LLM adjudication.
#' @param llmAmbiguityMinScore Optional lower bound of embedding score gray-zone for LLM adjudication trigger.
#' @param llmAmbiguityMaxScore Optional upper bound of embedding score gray-zone for LLM adjudication trigger.
#' @param llmTop2GapThreshold Maximum top1-top2 embedding gap considered ambiguous enough for LLM adjudication.
#' @param llmConfidenceThreshold Minimum LLM confidence to accept adjudicated candidate/no-match.
#' @param conceptSetWorkers Number of parallel worker processes used to build missing concept
#'   sets. `1` (default) builds them one at a time, condition by condition. With more than one,
#'   the distinct missing sets across all conditions are built in parallel and written to the
#'   library once all builds finish.
#' @param conceptSetFolder Folder for concept-set builder files, shared across runs and conditions.
#'   Each set gets a subfolder; with parallel workers a finished set is saved as `result.rds` and
#'   reused on a rerun. `NULL` uses `<outputDirectory>/conceptSetBuild` for parallel runs and the
#'   condition folders for serial runs.
#' @param llmConfig Config list (the `llmReasoning` / `llmNonReasoning` blocks) used by parallel
#'   workers to build their own LLM clients; `list()` uses the defaults.
#' @param emptyConceptSet What to do with a concept set whose build returns no concepts:
#'   `"placeholder"` (default) records it as known empty (placeholder concept 36433327 plus a
#'   `knownEmpty.json` file in the set's folder), skips it on reruns, and flags the rubric rules that
#'   use it with `okToUse = FALSE`; `"stop"` stops after the run. Build errors are never recorded as empty.
#' @param emptyConceptSetFallback Logical; when `TRUE`, an empty set is first replaced by an existing
#'   (or newly built) set under a simplified name that drops a qualifier word such as `brand`.
#' @param rebuildEmptyConceptSets Logical; when `TRUE`, sets recorded as known empty are rebuilt.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisibly updates concept set library and matched concept set files.
#' @examples
#' \dontrun{
#' createRubricConceptSets(
#'   conditionList = list(list(testCondition = "GERD")),
#'   outputDirectory = "P:/llm/rubrics",
#'   database = CCAE_OHDA_DBSpec
#' )
#' }
#' @export
createRubricConceptSets <- function(conditionList,
                                    outputDirectory,
                                    database,
                                    seedLibraryPath = "P:/llm/rubricsNew",
                                    llmClientReasoning = ithGetLlmClientFromConfig(list(), "reasoning"),
                                    llmClientNonReasoning = ithGetLlmClientFromConfig(list(), "nonReasoning"),
                                    similarityThreshold = 0.94,
                                    fuzzyThreshold = 0.94,
                                    useLlmAdjudication = TRUE,
                                    llmTopCandidates = 25,
                                    llmAmbiguityMinScore = NULL,
                                    llmAmbiguityMaxScore = NULL,
                                    llmTop2GapThreshold = 0.015,
                                    llmConfidenceThreshold = 0.60,
                                    conceptSetWorkers = 1L,
                                    conceptSetFolder = NULL,
                                    llmConfig = list(),
                                    emptyConceptSet = "placeholder",
                                    emptyConceptSetFallback = TRUE,
                                    rebuildEmptyConceptSets = FALSE,
                                    verbose = TRUE) {
  conceptLibraryLocation <- ithResolveConceptLibraryLocation(
    libraryPath = seedLibraryPath,
    defaultFileName = "rubricConceptSetLibrary.duckdb"
  )
  ithMaybeMessage(verbose, "Using concept set library from: ", conceptLibraryLocation$resolvedPath)

  ithCreateRubricConceptSets(
    conditionSpecs = conditionList,
    outputDirectory = outputDirectory,
    conceptLibraryDirectory = conceptLibraryLocation$libraryDirectory,
    database = database,
    llmClientReasoning = llmClientReasoning,
    llmClientNonReasoning = llmClientNonReasoning,
    conceptLibraryFile = conceptLibraryLocation$libraryFileName,
    similarityThreshold = similarityThreshold,
    fuzzyThreshold = fuzzyThreshold,
    useLlmAdjudication = useLlmAdjudication,
    llmTopCandidates = llmTopCandidates,
    llmAmbiguityMinScore = llmAmbiguityMinScore,
    llmAmbiguityMaxScore = llmAmbiguityMaxScore,
    llmTop2GapThreshold = llmTop2GapThreshold,
    llmConfidenceThreshold = llmConfidenceThreshold,
    conceptSetWorkers = conceptSetWorkers,
    conceptSetFolder = conceptSetFolder,
    llmConfig = llmConfig,
    emptyConceptSet = emptyConceptSet,
    emptyConceptSetFallback = emptyConceptSetFallback,
    rebuildEmptyConceptSets = rebuildEmptyConceptSets,
    verbose = verbose
  )
}

#' Query a concept set by name from the DuckDB concept set library
#'
#' @param libraryPath Path to the concept set library directory or file. When
#'   a directory is provided, `rubricConceptSetLibrary.duckdb` is used.
#' @param conceptSetName Concept set name or SQL-LIKE pattern to find. Matching
#'   is case-insensitive; `%` matches any sequence and `_` matches one character.
#' @param libraryVersion Optional library version to query. Defaults to the
#'   latest version.
#' @param smallList Logical; when `TRUE`, return only a data frame containing
#'   `conceptSetName` and `cleanName`.
#'
#' @return For an exact name, a list containing `conceptSetName`, `cleanName`,
#'   `conceptSet`, `embedding`, `libraryPath`, and `libraryVersion`, or `NULL`
#'   when no match exists. For a pattern containing `%` or `_`, returns a list
#'   of matching entries.
#' @examples
#' \dontrun{
#' result <- queryConceptSetLibrary(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   conceptSetName = "heart_failure"
#' )
#' result$conceptSet
#' }
#' @export
queryConceptSetLibrary <- function(libraryPath,
                                   conceptSetName,
                                   libraryVersion = NULL,
                                   smallList = FALSE) {
  if (is.null(conceptSetName) || !nzchar(trimws(as.character(conceptSetName[[1]])))) {
    stop("conceptSetName must be a non-empty string")
  }

  resolvedPath <- ithResolveDuckdbLibraryPath(
    libraryPath = libraryPath,
    defaultFileName = ithDefaultConceptLibraryFileName(),
    mustExist = TRUE
  )
  libraryData <- ithDuckdbGetConceptSetLibrary(
    libraryPath = resolvedPath,
    libraryVersion = libraryVersion
  )
  conceptTable <- libraryData[[1]]
  requestedName <- trimws(as.character(conceptSetName[[1]]))
  hasWildcard <- grepl("[%_]", requestedName)
  if (hasWildcard) {
    patternParts <- strsplit(requestedName, "", fixed = TRUE)[[1]]
    regexParts <- vapply(patternParts, function(part) {
      if (identical(part, "%")) {
        return(".*")
      }
      if (identical(part, "_")) {
        return(".")
      }
      if (grepl("[.\\\\^$|()\\[\\]{}+*?]", part, perl = TRUE)) {
        return(paste0("\\\\", part))
      }
      part
    }, character(1))
    matchIndex <- which(grepl(
      paste0("^", paste(regexParts, collapse = ""), "$"),
      as.character(conceptTable$conceptSetName),
      ignore.case = TRUE,
      perl = TRUE
    ))
  } else {
    matchIndex <- which(tolower(trimws(as.character(conceptTable$conceptSetName))) == tolower(requestedName))
  }

  if (length(matchIndex) == 0) {
    if (isTRUE(smallList)) {
      return(data.frame(
        conceptSetName = character(),
        cleanName = character(),
        stringsAsFactors = FALSE
      ))
    }
    return(NULL)
  }

  if (isTRUE(smallList)) {
    return(data.frame(
      conceptSetName = as.character(conceptTable$conceptSetName[matchIndex]),
      cleanName = as.character(conceptTable$cleanName[matchIndex]),
      stringsAsFactors = FALSE
    ))
  }

  resolvedVersion <- if (is.null(libraryVersion)) {
    ithDuckdbGetLatestLibraryVersion(resolvedPath)
  } else {
    libraryVersion
  }

  buildResult <- function(rowIndex) {
    list(
      conceptSetName = conceptTable$conceptSetName[[rowIndex]],
      cleanName = conceptTable$cleanName[[rowIndex]],
      conceptSet = conceptTable$conceptSet[[rowIndex]],
      embedding = libraryData[[2]]$embed[[rowIndex]],
      libraryPath = resolvedPath,
      libraryVersion = resolvedVersion
    )
  }

  if (!hasWildcard) {
    return(buildResult(matchIndex[[1]]))
  }

  lapply(matchIndex, buildResult)
}

#' Export a concept set from the DuckDB concept set library as JSON text
#'
#' @param libraryPath Path to the concept set library directory or DuckDB file.
#' @param conceptSetName Exact concept set name or clean name (case-insensitive;
#'   no wildcards).
#' @param libraryVersion Optional library version. Defaults to the latest version.
#' @param pretty Logical; when `TRUE`, indent the JSON.
#' @param outputDirectory Directory to write `<conceptSetName>.json` to. Defaults to
#'   `outputDirectory` in the config. Created if it does not exist.
#' @param configPath Path to the Ithaca config, used when `outputDirectory` is `NULL`.
#' @param writeFile Logical; when `FALSE`, do not write a file.
#' @param copyToClipboard Logical; when `TRUE`, also copy the JSON to the clipboard.
#'
#' @return A `json` character string with the ATLAS-style concept set expression
#'   (`{"items": [...]}`), returned invisibly when written to a file or the clipboard.
#'   If the concept set is not found, returns `NULL` after displaying a message.
#' @examples
#' \dontrun{
#' exportConceptSetJson(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   conceptSetName = "heart_failure",
#'   copyToClipboard = TRUE
#' )
#' }
#' @export
exportConceptSetJson <- function(libraryPath,
                                 conceptSetName,
                                 libraryVersion = NULL,
                                 pretty = TRUE,
                                 outputDirectory = NULL,
                                 configPath = "config.yaml",
                                 writeFile = TRUE,
                                 copyToClipboard = FALSE) {
  if (is.null(conceptSetName) || !nzchar(trimws(as.character(conceptSetName[[1]])))) {
    stop("conceptSetName must be a non-empty string")
  }
  if (isTRUE(writeFile) && is.null(outputDirectory)) {
    config <- ithReadIthacaConditionConfig(configPath)
    if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
      stop("outputDirectory was not supplied and is not defined in config: ", configPath)
    }
    outputDirectory <- as.character(config$outputDirectory[[1]])
  }

  resolvedPath <- ithResolveDuckdbLibraryPath(
    libraryPath = libraryPath,
    defaultFileName = ithDefaultConceptLibraryFileName(),
    mustExist = TRUE
  )
  conceptTable <- ithDuckdbGetConceptSetLibrary(
    libraryPath = resolvedPath,
    libraryVersion = libraryVersion
  )[[1]]

  requestedName <- tolower(trimws(as.character(conceptSetName[[1]])))
  matchIndex <- which(tolower(trimws(conceptTable$conceptSetName)) == requestedName)
  if (length(matchIndex) == 0L) {
    matchIndex <- which(tolower(trimws(conceptTable$cleanName)) == requestedName)
  }
  if (length(matchIndex) == 0L) {
    message("Concept set not found in library: ", conceptSetName[[1]])
    return(invisible(NULL))
  }
  if (length(matchIndex) > 1L) {
    stop("Concept set name is ambiguous in library: ", conceptSetName[[1]])
  }

  # Normalizes legacy storage (e.g. a bare items list) to list(items = ...)
  conceptSet <- ithPrepareConceptSetForResolution(conceptTable$conceptSet[[matchIndex]])
  if (is.null(conceptSet)) {
    stop("Concept set could not be parsed: ", conceptSetName[[1]])
  }

  json <- jsonlite::toJSON(conceptSet, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = pretty)

  if (isTRUE(writeFile)) {
    dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
    outputFile <- normalizePath(
      file.path(outputDirectory, paste0(conceptTable$conceptSetName[[matchIndex]], ".json")),
      winslash = "/",
      mustWork = FALSE
    )
    writeLines(json, outputFile, useBytes = TRUE)
    message("Concept set JSON written to: ", outputFile)
  }
  if (isTRUE(copyToClipboard)) {
    ithCopyToClipboard(as.character(json))
    message("Concept set JSON copied to the clipboard")
  }

  if (isTRUE(writeFile) || isTRUE(copyToClipboard)) {
    return(invisible(json))
  }
  json
}

ithCopyToClipboard <- function(text) {
  if (.Platform$OS.type == "windows") {
    utils::writeClipboard(enc2utf8(text), format = 13L)
  } else if (requireNamespace("clipr", quietly = TRUE)) {
    clipr::write_clip(text)
  } else {
    stop("Copying to the clipboard requires the 'clipr' package on this platform")
  }
}

#' List concept sets containing a target concept ID
#'
#' @param libraryPath Path to the concept set library directory or DuckDB file.
#' @param conceptId Concept ID to find. Defaults to Ithaca's failed-resolution fallback ID `36433327`.
#' @param singleConceptOnly Logical; when `TRUE`, return only concept sets whose
#'   unique concept list consists solely of `conceptId`.
#' @param libraryVersion Optional library version. Defaults to the latest version.
#'
#' @return A data frame listing matching concept sets, clean names, concept counts,
#'   and library metadata. Returns zero rows when no matches are found.
#' @examples
#' \dontrun{
#' failedConceptSets <- listConceptSetsWithConceptId(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
#' )
#' failedConceptSets
#' }
#' @export
listConceptSetsWithConceptId <- function(libraryPath,
                                         conceptId = 36433327,
                                         singleConceptOnly = TRUE,
                                         libraryVersion = NULL) {
  targetConceptId <- as.character(as.numeric(conceptId[[1]]))
  if (is.na(targetConceptId) || !nzchar(targetConceptId)) {
    stop("conceptId must be a valid numeric concept ID")
  }

  resolvedPath <- Ithaca:::ithResolveDuckdbLibraryPath(
    libraryPath = libraryPath,
    defaultFileName = Ithaca:::ithDefaultConceptLibraryFileName(),
    mustExist = TRUE
  )
  libraryData <- Ithaca:::ithDuckdbGetConceptSetLibrary(
    libraryPath = resolvedPath,
    libraryVersion = libraryVersion
  )
  conceptTable <- libraryData[[1]]
  resolvedVersion <- libraryVersion
  if (is.null(resolvedVersion)) {
    resolvedVersion <- Ithaca:::ithDuckdbGetLatestLibraryVersion(resolvedPath)
  }

  extractConceptIds <- function(value) {
    if (is.null(value)) {
      return(character())
    }
    if (is.data.frame(value)) {
      matchingColumns <- intersect(c("conceptId", "concept_id"), names(value))
      if (length(matchingColumns) > 0L) {
        return(as.character(value[[matchingColumns[[1]]]]))
      }
      return(character())
    }
    if (is.list(value)) {
      valueNames <- names(value)
      if (!is.null(valueNames)) {
        matchingNames <- intersect(c("conceptId", "concept_id"), valueNames)
        if (length(matchingNames) > 0L) {
          return(as.character(value[[matchingNames[[1]]]]))
        }
      }
      return(unique(unlist(lapply(value, extractConceptIds), use.names = FALSE)))
    }
    character()
  }

  rows <- lapply(seq_len(nrow(conceptTable)), function(rowIndex) {
    normalized <- Ithaca:::ithPrepareConceptSetForResolution(conceptTable$conceptSet[[rowIndex]])
    conceptIds <- unique(as.character(extractConceptIds(normalized)))
    conceptIds <- conceptIds[!is.na(conceptIds) & nzchar(conceptIds)]
    matched <- targetConceptId %in% conceptIds
    onlyTarget <- length(conceptIds) == 1L && identical(conceptIds[[1]], targetConceptId)

    if (isTRUE(singleConceptOnly)) {
      keep <- onlyTarget
    } else {
      keep <- matched
    }

    if (!keep) {
      return(NULL)
    }

    data.frame(
      conceptSetName = as.character(conceptTable$conceptSetName[[rowIndex]]),
      cleanName = as.character(conceptTable$cleanName[[rowIndex]]),
      matchedConceptId = targetConceptId,
      conceptCount = length(conceptIds),
      libraryPath = resolvedPath,
      libraryVersion = as.integer(resolvedVersion),
      stringsAsFactors = FALSE
    )
  })

  matches <- Filter(Negate(is.null), rows)
  if (length(matches) == 0L) {
    return(data.frame(
      conceptSetName = character(),
      cleanName = character(),
      matchedConceptId = character(),
      conceptCount = integer(),
      libraryPath = character(),
      libraryVersion = integer(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, matches)
}

#' Update a concept set library from a JSON concept set file
#'
#' Loads the latest version of an existing concept set library, upserts one
#' concept set using a JSON file, refreshes embedding metadata, and saves a new
#' versioned library artifact.
#' When `csName` already exists, only the concept set JSON payload is updated;
#' existing `cleanName` and embedding values are preserved.
#'
#' @param libraryPath Path to the concept set library directory or file.
#' When a directory is provided, `rubricConceptSetLibrary.duckdb` is used.
#' The resolved library file is updated in place using versioned row history.
#' @param csName Concept set name key to upsert.
#' @param conceptSetJsonPath Path to a concept set JSON file.
#' @param cleanName Optional display/clean name. Defaults to `csName` converted
#' from underscores to spaces.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisible list with resolved input path, saved path, and updated concept set metadata.
#' @examples
#' \dontrun{
#' updateConceptSetLibraryFromJson(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   csName = "immunomodulator_medication_codes",
#'   conceptSetJsonPath = "P:/llm/largeScalePhenotyping/Crohns disease/immunomodulator_medication_codes/immunomodulator medication codes.json"
#' )
#' }
#' @export
updateConceptSetLibraryFromJson <- function(libraryPath,
                                            csName,
                                            conceptSetJsonPath,
                                            cleanName = NULL,
                                            runId = "manualUpdate",
                                            conditionName = NA_character_,
                                            cohortId = NA_integer_,
                                            verbose = TRUE) {
  ithUpdateConceptSetLibraryFromJson(
    libraryPath = libraryPath,
    csName = csName,
    conceptSetJsonPath = conceptSetJsonPath,
    cleanName = cleanName,
    runId = runId,
    conditionName = conditionName,
    cohortId = cohortId,
    verbose = verbose
  )
}

#' Insert a concept set from a JSON file
#'
#' @param libraryPath Path to the concept set library directory or file.
#' @param conceptSetJsonPath Path to a concept set JSON file.
#' @param cleanName Optional human-readable clean/display name. When omitted,
#'   it is derived from the JSON filename stem by replacing underscores with spaces.
#' @param conceptSetName Optional concept set name key. When omitted, it is derived
#'   from the clean name using lowercase underscore normalization.
#' @param runId Run identifier stored with the library revision.
#' @param conditionName Optional condition name stored with the revision.
#' @param cohortId Optional cohort identifier stored with the revision.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisible list with resolved input path, saved path, and updated concept set metadata.
#' @examples
#' \dontrun{
#' insertConceptSetFromJson(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   cleanName = "Major depressive disorder diagnosis codes",
#'   conceptSetJsonPath = "P:/llm/rubrics/major_depressive_disorder_diagnosis_codes.json"
#' )
#' }
#' @export
insertConceptSetFromJson <- function(libraryPath,
                                     cleanName = NULL,
                                     conceptSetJsonPath,
                                     conceptSetName = NULL,
                                     runId = "manualInsert",
                                     conditionName = NA_character_,
                                     cohortId = NA_integer_,
                                     verbose = TRUE) {
  if (is.null(conceptSetJsonPath) || !nzchar(trimws(as.character(conceptSetJsonPath[[1]])))) {
    stop("conceptSetJsonPath must be a non-empty path")
  }

  fileStem <- tools::file_path_sans_ext(basename(as.character(conceptSetJsonPath[[1]])))
  if (is.null(cleanName) || !nzchar(trimws(as.character(cleanName[[1]])))) {
    resolvedCleanName <- gsub("_", " ", fileStem, fixed = TRUE)
  } else {
    resolvedCleanName <- trimws(as.character(cleanName[[1]]))
  }

  if (is.null(conceptSetName) || !nzchar(trimws(as.character(conceptSetName[[1]])))) {
    resolvedName <- tolower(gsub("[^A-Za-z0-9]+", "_", resolvedCleanName))
    resolvedName <- gsub("^_+|_+$", "", resolvedName)
  } else {
    resolvedName <- trimws(as.character(conceptSetName[[1]]))
  }
  if (!nzchar(resolvedCleanName) || !nzchar(resolvedName)) {
    stop("Could not derive non-empty cleanName and conceptSetName from conceptSetJsonPath")
  }

  updateConceptSetLibraryFromJson(
    libraryPath = libraryPath,
    csName = resolvedName,
    conceptSetJsonPath = conceptSetJsonPath,
    cleanName = resolvedCleanName,
    runId = runId,
    conditionName = conditionName,
    cohortId = cohortId,
    verbose = verbose
  )
}

#' Normalize concept set library storage format
#'
#' Rewrites every concept set JSON payload in a concept set library to a
#' canonical `list(items = ...)` storage shape and saves a new versioned
#' library artifact.
#'
#' @param libraryPath Path to the concept set library directory or file.
#' DuckDB library path or a directory containing `rubricConceptSetLibrary.duckdb`.
#' @param outputFileName Optional output base filename for versioned save.
#' Defaults to the resolved library base name with trailing version suffixes removed.
#' @param replaceUnreadableWithEmpty Logical; when `TRUE`, unreadable concept set
#' payloads are replaced with canonical empty payloads `list(items = list())`.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisible list with saved path, normalized row counts, and unreadable concept set names.
#' @examples
#' \dontrun{
#' normalizeConceptSetLibraryStorage(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
#' )
#' }
#' @export
normalizeConceptSetLibraryStorage <- function(libraryPath,
                                              outputFileName = NULL,
                                              replaceUnreadableWithEmpty = TRUE,
                                              runId = "normalizeConceptSetLibraryStorage",
                                              verbose = TRUE) {
  ithNormalizeConceptSetLibraryStorage(
    libraryPath = libraryPath,
    outputFileName = outputFileName,
    replaceUnreadableWithEmpty = replaceUnreadableWithEmpty,
    runId = runId,
    verbose = verbose
  )
}

#' Delete concept set(s) from the concept set library by name
#'
#' Removes concept set revisions and current rows for concept sets matching
#' `conceptSetName` (case-insensitive match against `conceptSetName` or
#' `cleanName` in `conceptSetCurrent`), then rebuilds current pointers from
#' remaining revisions.
#'
#' @param libraryPath Path to the concept set library directory or file.
#' DuckDB library path or a directory containing `rubricConceptSetLibrary.duckdb`.
#' @param conceptSetName Concept set name to delete (case-insensitive).
#' @param dryRun Logical; when `TRUE`, returns matching rows without deleting.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisible list with matched concept sets, revision rows, and deletion status.
#' @examples
#' \dontrun{
#' deleteConceptSetFromLibrary(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   conceptSetName = "osteoarthritis diagnosis codes",
#'   dryRun = TRUE
#' )
#' }
#' @export
deleteConceptSetFromLibrary <- function(libraryPath,
                                        conceptSetName,
                                        dryRun = TRUE,
                                        verbose = TRUE) {
  ithDeleteConceptSetFromLibrary(
    libraryPath = libraryPath,
    conceptSetName = conceptSetName,
    dryRun = dryRun,
    verbose = verbose
  )
}

#' Delete a concept set from the DuckDB library by clean name
#'
#' @param libraryPath Path to the concept set library directory or file.
#' @param cleanName Exact case-insensitive clean/display name to remove.
#' @param dryRun Logical; when `TRUE`, preview matches without deleting.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisibly returns the deletion preview or result.
#' @examples
#' \dontrun{
#' deleteConceptSetFromLibraryByCleanName(
#'   libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   cleanName = "major depressive disorder",
#'   dryRun = TRUE
#' )
#' }
#' @export
deleteConceptSetFromLibraryByCleanName <- function(libraryPath,
                                                   cleanName,
                                                   dryRun = TRUE,
                                                   verbose = TRUE) {
  ithDeleteConceptSetFromLibrary(
    libraryPath = libraryPath,
    conceptSetName = cleanName,
    dryRun = dryRun,
    verbose = verbose,
    cleanNameOnly = TRUE
  )
}

#' Edit a rubric rule and optionally regenerate SQL
#'
#' @param conditionName Condition/rubric name.
#' @param ruleId Rule identifier.
#' @param updates Named list of field replacements.
#' @param outputDirectory Root output directory for rubric artifacts.
#' @param regenerateSql Logical; regenerate SQL when criteria/concepts are changed.
#' @param llmClient LLM client used for SQL regeneration.
#' @param promptDir Optional prompt folder override.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Updated short rubric (invisibly).
#' @examples
#' \dontrun{
#' adjustRubric(
#'   conditionName = "Gastroesophageal reflux disease",
#'   ruleId = "ER1",
#'   updates = list(criteria = "updated criterion text"),
#'   outputDirectory = "P:/llm/rubrics"
#' )
#' }
#' @export
adjustRubric <- function(conditionName,
                        ruleId,
                        updates,
                        outputDirectory,
                        regenerateSql = TRUE,
                        llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                        promptDir = NULL,
                        verbose = TRUE) {
  shortRubric <- ithLoadLatestShortRubric(conditionName, outputDirectory)
  updated <- ithUpdateRubricRule(
    shortRubric = shortRubric,
    ruleId = ruleId,
    updates = updates,
    regenerateSql = regenerateSql,
    llmClient = llmClient,
    promptDir = promptDir,
    verbose = verbose
  )
  ithSaveShortRubric(updated, conditionName, outputDirectory)
  invisible(updated)
}

#' Apply edits from a concise rubric CSV file
#'
#' Reads an edited concise rubric table, validates completeness, applies row additions/
#' deletions/updates, regenerates SQL when criteria or needed concept sets changed,
#' and updates downstream rubric artifacts.
#'
#' SQL edits in the CSV are ignored; SQL is reused from existing rows or regenerated
#' by the application as needed.
#'
#' Rule IDs are assigned by the application: leave `ruleId` blank for a new rule. A `ruleId`
#' that is not in the current rubric is an error (rules cannot be renamed), a changed `type`
#' gives the rule the next ID for its new type, and removed IDs are never reused.
#'
#' @param conditionName Condition/rubric name.
#' @param outputDirectory Root output directory for rubric artifacts.
#' @param editedConciseRubricPath Optional path to an edited concise rubric CSV. When
#' `NULL`, the latest concise rubric CSV in the condition directory is used.
#' @param llmClient LLM client used for SQL regeneration and optional summary refresh.
#' @param promptDir Optional prompt folder override.
#' @param updateSummary Logical; regenerate concise rubric summary text.
#' @param verbose Logical; when `TRUE`, print progress messages.
#' @param refreshConceptSets Logical; when `TRUE`, use the LLM to infer updated
#'   needed concept-set names when a rule's criteria changes before regenerating SQL.
#' @param adjustableRuleId Optional rule IDs (for example `"ER1"` or `c("ER1", "ER5")`) that
#'   are re-scored at report time. The edit stops if any of them is removed or is no longer an
#'   evidence rule. [applyConciseRubricEditsFromConfig()] passes the condition's
#'   `adjustableRuleId` from conditionSpecs.
#'
#' @return Invisibly returns the updated short rubric data frame.
#' @examples
#' \dontrun{
#' applyConciseRubricEdits(
#'   conditionName = "Gastroesophageal reflux disease",
#'   outputDirectory = "P:/llm/rubrics",
#'   editedConciseRubricPath = "P:/llm/rubrics/Gastroesophageal reflux disease/Gastroesophageal reflux disease_conciseRubric_EDIT.csv"
#' )
#' }
#' @export
applyConciseRubricEdits <- function(conditionName,
                                    outputDirectory,
                                    editedConciseRubricPath = NULL,
                                    llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                                    promptDir = NULL,
                                    updateSummary = TRUE,
                                    verbose = TRUE,
                                    refreshConceptSets = TRUE,
                                    adjustableRuleId = NULL) {
  ithApplyConciseRubricEdits(
    conditionName = conditionName,
    outputDirectory = outputDirectory,
    editedConciseRubricPath = editedConciseRubricPath,
    llmClient = llmClient,
    promptDir = promptDir,
    updateSummary = updateSummary,
    verbose = verbose,
    refreshConceptSets = refreshConceptSets,
    adjustableRuleId = adjustableRuleId
  )
}

#' Score subjects using rubric SQL rules
#'
#' @param conditionList List of condition specifications.
#' @param database List of database specifications.
#' @param outputDirectory Root output directory for rubric artifacts.
#' @param seedLibraryPath Optional concept set library file path, or a directory
#' containing `rubricConceptSetLibrary.duckdb`, used during scoring.
#' @param conceptLibraryVersion Concept set library version used for scoring. `NULL` (default), blank, or `"latest"` uses the newest library version; a positive integer pins that snapshot.
#' @param cohortTableName Cohort table name input used for cohort generation/reads and scoring SQL rendering.
#' Can be `NULL`, a single table name applied to all databases, a named character/list keyed by
#' `databaseId`, or an unnamed character/list with one value per database (in the same order as `database`).
#' @param useExisting Logical; reuse existing score files when available.
#' @param generateCohort Logical; generate cohorts before scoring.
#' @param cohortDefinitionSet Cohort definition data frame with `cohortId`, `cohortName`, and `sql`.
#' @param webapiBaseUrl Optional OHDSI WebAPI base URL used when `cohortDefinitionSet` is `NULL`.
#' @param llmClient LLM client used for SQL correction.
#' @param promptDir Optional prompt folder override.
#' @param scoringWorkers Number of parallel worker processes that run the rule queries (1, the
#'   default, runs them one at a time in this session). Each worker opens its own database
#'   connection; SQL corrections and retries stay in this session.
#' @param cohortWorkers Number of parallel worker processes that generate cohorts (one task per
#'   cohort and database) when `generateCohort = TRUE` (1, the default, generates them one at a time).
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Named list of scoring outputs by condition and database, including
#' `scoringSummary` generated from the latest saved artifacts for each job.
#' @examples
#' \dontrun{
#' cohortDefinitionSet <- ithMakeCohortDefinitionSet(
#'   cohortIds = c(14412),
#'   cohortSql = c("SELECT 1 AS subject_id")
#' )
#'
#' scoreSubjects(
#'   conditionList = list(list(rubricName = "Gastroesophageal reflux disease", randomCohortDefinitionId = 14412)),
#'   database = list(CCAE_OHDA_DBSpec),
#'   outputDirectory = "P:/llm/rubrics",
#'   seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#'   conceptLibraryVersion = 142,
#'   generateCohort = FALSE,
#'   cohortDefinitionSet = cohortDefinitionSet
#' )
#' }
#' @export
scoreSubjects <- function(conditionList,
                         database,
                         outputDirectory,
                         seedLibraryPath = NULL,
                         conceptLibraryVersion = NULL,
                         cohortTableName = NULL,
                         useExisting = FALSE,
                         generateCohort = FALSE,
                         cohortDefinitionSet = NULL,
                         webapiBaseUrl = NULL,
                         llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                         promptDir = NULL,
                         scoringWorkers = 1L,
                         cohortWorkers = 1L,
                         verbose = TRUE) {
  ithScoreSubjects(
    conditionSpecs = conditionList,
    databases = database,
    outputDirectory = outputDirectory,
    seedLibraryPath = seedLibraryPath,
    conceptLibraryVersion = conceptLibraryVersion,
    cohortTableName = cohortTableName,
    useExisting = useExisting,
    generateCohorts = generateCohort,
    cohortDefinitionSet = cohortDefinitionSet,
    webapiBaseUrl = webapiBaseUrl,
    llmClient = llmClient,
    promptDir = promptDir,
    scoringWorkers = scoringWorkers,
    cohortWorkers = cohortWorkers,
    verbose = verbose
  )
}

#' Score subjects directly from a full scoring artifact
#'
#' Reads a `fullScoringFile_*.rds` artifact, aggregates rubric totals per
#' subject, and computes rubric-level and final case labels.
#'
#' @param fullScoringFile Path to `fullScoringFile_*.rds`.
#' @param excludeNegativeScores Logical; when `TRUE`, scoring rows with
#' `score < 0` are excluded before aggregation, except AE1 (the fixed zero-occurrence
#' penalty), which is always applied.
#' @param yesScoreOverride Optional numeric threshold override(s) for positive
#' case assignment. Can be one value for all rubrics, a named numeric vector
#' keyed by `rubricId`, or an unnamed numeric vector with one value per rubric.
#' @param noScoreOverride Optional numeric threshold override(s) for uncertain
#' case lower bound. Can be one value for all rubrics, a named numeric vector
#' keyed by `rubricId`, or an unnamed numeric vector with one value per rubric.
#' @param saveAdjustedOutput Logical; when `TRUE`, writes adjusted scoring
#' artifacts using filenames that include `Adjusted`.
#' @param outputDirectory Optional output directory for adjusted artifacts.
#' Defaults to the folder containing the resolved `fullScoringFile`.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return List containing `sumCohortScores`, `finalScores`, `thresholdTable`,
#' saved adjusted file paths, and run metadata.
#' @examples
#' \dontrun{
#' scoreResult <- scoreFullScoringFileAdjusted(
#'   fullScoringFile = "P:/llm/rubrics/scoring/GERD/CCAE/fullScoringFile_Cohort14412_V1.rds",
#'   excludeNegativeScores = TRUE,
#'   yesScoreOverride = 1,
#'   noScoreOverride = 0,
#'   saveAdjustedOutput = TRUE
#' )
#' }
#' @export
scoreFullScoringFileAdjusted <- function(fullScoringFile,
                                 excludeNegativeScores = FALSE,
                                 yesScoreOverride = NULL,
                                 noScoreOverride = NULL,
                                 saveAdjustedOutput = TRUE,
                                 outputDirectory = NULL,
                                 verbose = TRUE) {
  ithScoreFullScoringFile(
    fullScoringFile = fullScoringFile,
    excludeNegativeScores = excludeNegativeScores,
    yesScoreOverride = yesScoreOverride,
    noScoreOverride = noScoreOverride,
    saveAdjustedOutput = saveAdjustedOutput,
    outputDirectory = outputDirectory,
    verbose = verbose
  )
}

#' Backfill scoring case labels from existing artifacts
#'
#' Recomputes rubric-level and subject-level case labels from an existing
#' `sumCohortScores` artifact using rubric decision thresholds, then writes new
#' versioned scoring artifacts (and optionally summary artifacts).
#'
#' This is useful for legacy runs where case labels were computed from a rubric
#' schema that used different field naming conventions.
#'
#' @param conditionName Condition/rubric name.
#' @param databaseId Database identifier used in scoring output path.
#' @param randomCohortDefinitionId Random cohort definition identifier.
#' @param outputDirectory Root output directory for rubric artifacts.
#' @param rubricPath Optional rubric object path (file or directory). When
#' omitted, uses the latest `rubricList.rds` from
#' `file.path(outputDirectory, conditionName)`.
#' @param saveSummary Logical; when `TRUE`, regenerate summary artifacts after
#' backfill.
#' @param verbose Logical; when `TRUE`, print progress messages.
#' @param recomputeFromRuleScores Logical; when `TRUE`, rubric totals are also re-summed
#' from the latest `fullScoringFile` using the latest short rubric rule scores, and new
#' `fullScoringFile`/`subjectCount` versions carrying those scores are saved. Use this
#' after changing rule scores and/or yes/no thresholds. It stops with an error if rules
#' were added or removed or if any rule's criteria changed (rerun scoring in that case).
#' @param skipIfUnchanged Logical; when `TRUE`, nothing is saved (and `skipped = TRUE` is
#' returned) if the rule scores, thresholds, totals, and case labels are identical to the
#' latest saved scoring artifacts.
#'
#' @return Invisible list containing saved artifact paths and case-count totals.
#' @examples
#' \dontrun{
#' backfillScoringCaseLabels(
#'   conditionName = "Confusion",
#'   databaseId = "Optum DOD",
#'   randomCohortDefinitionId = 26165001,
#'   outputDirectory = "P:/llm/largeScalePhenotyping",
#'   saveSummary = TRUE
#' )
#' }
#' @export
backfillScoringCaseLabels <- function(conditionName,
                                      databaseId,
                                      randomCohortDefinitionId,
                                      outputDirectory,
                                      rubricPath = NULL,
                                      saveSummary = TRUE,
                                      verbose = TRUE,
                                      recomputeFromRuleScores = FALSE,
                                      skipIfUnchanged = FALSE) {
  ithBackfillScoringCaseLabels(
    conditionName = conditionName,
    databaseId = databaseId,
    randomCohortDefinitionId = randomCohortDefinitionId,
    outputDirectory = outputDirectory,
    rubricPath = rubricPath,
    saveSummary = saveSummary,
    verbose = verbose,
    recomputeFromRuleScores = recomputeFromRuleScores,
    skipIfUnchanged = skipIfUnchanged
  )
}

#' Check that every concept set used in rule SQL is listed in neededConceptSets
#'
#' For each condition in the config (including `conditionSpecsCsvPath`), loads the latest
#' short rubric and compares, rule by rule, the `@` concept set references in `sqlStatement`
#' against the rule's `neededConceptSets`. When `addMissing = TRUE`, missing names are appended
#' to the rule's `neededConceptSets` in a new short rubric and concise rubric CSV version, and to
#' `conceptSets_needed` and `neededConcepts` in a new `rubricList.rds` version. SQL is not changed.
#' Rerun `createRubricConceptSetsFromConfig.R` afterwards so the added concept sets are matched.
#'
#' @param conditionNames Optional character vector of condition names (`rubricName` or
#'   `testCondition`, case-insensitive) to check. When `NULL`, all configured conditions are checked.
#' @param configPath Path to YAML configuration file.
#' @param addMissing Logical; when `TRUE` (default), add the missing concept sets to
#'   `neededConceptSets`; when `FALSE`, only report them.
#'
#' @return Data frame with one row per rule whose SQL uses concept sets missing from
#'   `neededConceptSets`: `conditionName`, `rubric`, `ruleId`, `missingConceptSets`, and
#'   `neededConceptSets` (the value before any additions). Conditions without a short rubric are
#'   reported with `rubric = NA` and a note in `missingConceptSets`. Zero rows means every
#'   condition is consistent.
#' @examples
#' \dontrun{
#' mismatches <- checkSqlConceptSetsInNeededConceptsFromConfig()
#' mismatches <- checkSqlConceptSetsInNeededConceptsFromConfig("Confusion", addMissing = FALSE)
#' }
#' @export
checkSqlConceptSetsInNeededConceptsFromConfig <- function(conditionNames = NULL,
                                                          configPath = "config.yaml",
                                                          addMissing = TRUE) {
  ithCheckConditionNamesNotConfig(conditionNames)
  config <- ithReadIthacaConditionConfig(configPath)
  if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
    stop("outputDirectory is not defined in config: ", configPath)
  }
  outputDirectory <- as.character(config$outputDirectory[[1]])

  specNames <- vapply(
    config$conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    missingNames <- conditionNames[!tolower(trimws(conditionNames)) %in% tolower(specNames)]
    if (length(missingNames) > 0) {
      stop("Requested conditionNames are not configured: ", paste(missingNames, collapse = ", "), call. = FALSE)
    }
    specNames <- specNames[tolower(specNames) %in% tolower(trimws(conditionNames))]
  }

  resultRow <- function(conditionName, rubric, ruleId, missingConceptSets, neededConceptSets) {
    data.frame(
      conditionName = conditionName,
      rubric = rubric,
      ruleId = ruleId,
      missingConceptSets = missingConceptSets,
      neededConceptSets = neededConceptSets,
      stringsAsFactors = FALSE
    )
  }

  rows <- list()
  for (conditionName in specNames) {
    shortRubric <- tryCatch(ithLoadLatestShortRubric(conditionName, outputDirectory), error = function(e) NULL)
    if (is.null(shortRubric)) {
      rows[[length(rows) + 1L]] <- resultRow(conditionName, NA_character_, NA_character_, "no short rubric found", NA_character_)
      next
    }

    conditionDir <- file.path(outputDirectory, conditionName)
    rubricObject <- if (isTRUE(addMissing)) ithLoadLatest(conditionDir, "rubricList.rds") else NULL
    synced <- ithSyncSqlConceptSetsToNeeded(shortRubric, rubricObject$rubricList)
    mismatches <- synced$mismatches
    if (nrow(mismatches) == 0L) next
    rows[[length(rows) + 1L]] <- resultRow(
      rep(conditionName, nrow(mismatches)),
      mismatches$rubric,
      mismatches$ruleId,
      mismatches$missingConceptSets,
      mismatches$neededConceptSets
    )

    if (!isTRUE(addMissing)) next

    ithSaveShortRubric(synced$shortRubric, conditionName, outputDirectory, yesCombinationReport = FALSE)
    if (!is.null(rubricObject)) {
      rubricObject$rubricList <- synced$rubricList
      rubricObject$neededConcepts <- ithBuildNeededConceptsFromShortRubric(synced$shortRubric)
      ithSaveVersioned(saveDirectory = conditionDir, objectToSave = rubricObject, fileName = "rubricList.rds")
    }
    message("Added missing concept sets to neededConceptSets for ", nrow(mismatches), " rule(s) in: ", conditionName)
  }

  if (length(rows) == 0L) {
    message("All SQL concept sets are listed in neededConceptSets for ", length(specNames), " condition(s).")
    return(resultRow(character(), character(), character(), character(), character()))
  }

  result <- do.call(rbind, rows)
  message(nrow(result), " rule(s)/condition(s) have SQL concept sets missing from neededConceptSets.")
  if (isTRUE(addMissing)) {
    message("Rerun createRubricConceptSetsFromConfig.R so the added concept sets are matched before scoring.")
  }
  result
}

#' Clean malformed concept-set names in the saved rubrics of every configured condition
#'
#' For each condition in the config (including `conditionSpecsCsvPath`), rewrites concept-set
#' names with repeated adjacent tokens or other malformed characters (for example
#' `bile_duct_procedure_procedure_codes` to `bile_duct_procedure_codes`) in the latest short
#' rubric (`neededConceptSets` and SQL `@` references) and `rubricList.rds`
#' (`conceptSets_needed` and `neededConcepts`), saving new versions of the short rubric, concise
#' rubric CSV, and `rubricList.rds`. Conditions whose latest concise rubric CSV has edits not yet
#' applied are skipped so those edits are not overwritten.
#'
#' @param conditionNames Optional character vector of condition names (`rubricName` or
#'   `testCondition`, case-insensitive) to clean. When `NULL`, all configured conditions are cleaned.
#' @param configPath Path to YAML configuration file.
#' @param dryRun Logical; when `TRUE`, report the renames without saving anything.
#'
#' @return Data frame with one row per condition: `conditionName`, `status` (`cleaned`,
#'   `unchanged`, `would clean` for dry runs, `skipped: ...`, or `error: ...`), and `renames`
#'   (`from -> to` pairs).
#' @examples
#' \dontrun{
#' cleanRubricConceptSetNamesFromConfig(dryRun = TRUE)
#' cleanRubricConceptSetNamesFromConfig()
#' }
#' @export
cleanRubricConceptSetNamesFromConfig <- function(conditionNames = NULL,
                                                 configPath = "config.yaml",
                                                 dryRun = FALSE) {
  ithCheckConditionNamesNotConfig(conditionNames)
  config <- ithReadIthacaConditionConfig(configPath)
  if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
    stop("outputDirectory is not defined in config: ", configPath)
  }
  outputDirectory <- as.character(config$outputDirectory[[1]])

  specNames <- vapply(
    config$conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    missingNames <- conditionNames[!tolower(trimws(conditionNames)) %in% tolower(specNames)]
    if (length(missingNames) > 0) {
      stop("Requested conditionNames are not configured: ", paste(missingNames, collapse = ", "), call. = FALSE)
    }
    specNames <- specNames[tolower(specNames) %in% tolower(trimws(conditionNames))]
  }

  statusRow <- function(conditionName, status, renames = "") {
    data.frame(conditionName = conditionName, status = status, renames = renames, stringsAsFactors = FALSE)
  }

  results <- lapply(specNames, function(conditionName) {
    conditionDir <- file.path(outputDirectory, conditionName)
    tryCatch({
      shortRubric <- tryCatch(ithLoadLatestShortRubric(conditionName, outputDirectory), error = function(e) NULL)
      if (is.null(shortRubric)) {
        return(statusRow(conditionName, "skipped: no short rubric found"))
      }
      latestConcise <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
      if (!is.null(latestConcise)) {
        conciseRubric <- utils::read.csv(file.path(conditionDir, latestConcise), stringsAsFactors = FALSE)
        changes <- ithDiffConciseRubric(conciseRubric, shortRubric)
        if (isTRUE(changes$differs)) {
          changed <- setdiff(names(changes)[vapply(changes, isTRUE, logical(1))], "differs")
          return(statusRow(conditionName, paste0(
            "skipped: ", latestConcise, " differs from the short rubric (", paste(changed, collapse = ", "),
            "); run applyConciseRubricEdits(\"", conditionName, "\") first"
          )))
        }
      }

      rubricObject <- ithLoadLatest(conditionDir, "rubricList.rds")
      cleaned <- ithCleanRubricConceptSetNames(shortRubric, rubricObject$rubricList)
      if (nrow(cleaned$renames) == 0L) {
        return(statusRow(conditionName, "unchanged"))
      }
      renameText <- paste(unique(paste(cleaned$renames$from, "->", cleaned$renames$to)), collapse = "; ")
      if (isTRUE(dryRun)) {
        return(statusRow(conditionName, "would clean", renameText))
      }

      ithSaveShortRubric(cleaned$shortRubric, conditionName, outputDirectory, yesCombinationReport = FALSE)
      if (!is.null(rubricObject)) {
        rubricObject$rubricList <- cleaned$rubricList
        rubricObject$neededConcepts <- ithBuildNeededConceptsFromShortRubric(cleaned$shortRubric)
        ithSaveVersioned(saveDirectory = conditionDir, objectToSave = rubricObject, fileName = "rubricList.rds")
      }
      message("Cleaned concept-set names for ", conditionName, ": ", renameText)
      statusRow(conditionName, "cleaned", renameText)
    }, error = function(e) {
      statusRow(conditionName, paste("error:", conditionMessage(e)))
    })
  })

  results <- do.call(rbind, results)
  if (!isTRUE(dryRun) && any(results$status == "cleaned")) {
    message("Rerun createRubricConceptSetsFromConfig.R so the cleaned concept-set names are matched before scoring.")
  }
  results
}
