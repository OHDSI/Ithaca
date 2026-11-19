ithInitializeConceptSetLibrary <- function(outputDirectory,
                                               seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
                                               outputFileName = "rubricConceptSetLibrary.duckdb",
                                               verbose = TRUE) {
  dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)

  libraryPath <- file.path(outputDirectory, ithCanonicalConceptLibraryFileName(outputFileName))
  if (file.exists(libraryPath)) {
    return(invisible(libraryPath))
  }

  seedPath <- if (!is.null(seedLibraryPath) && nzchar(as.character(seedLibraryPath[[1]]))) {
    as.character(seedLibraryPath[[1]])
  } else {
    NULL
  }

  ithDuckdbInitializeLibrary(
    libraryPath = libraryPath,
    seedLibraryPath = seedPath,
    verbose = verbose
  )

  invisible(libraryPath)
}

ithGetEmbeddingVectorsInternal <- function(text, reportTime = TRUE, silent = FALSE) {
  if (exists("getEmbeddingVectors", mode = "function", inherits = TRUE)) {
    return(get("getEmbeddingVectors", mode = "function", inherits = TRUE)(
      text = text,
      reportTime = reportTime,
      silent = silent
    ))
  }

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

ithLoadLatestConceptSetLibrary <- function(outputDirectory, fileName = "rubricConceptSetLibrary.duckdb") {
  libraryPath <- ithResolveDuckdbLibraryPath(
    libraryPath = outputDirectory,
    defaultFileName = ithCanonicalConceptLibraryFileName(fileName),
    mustExist = TRUE
  )

  ithDuckdbGetConceptSetLibrary(libraryPath = libraryPath)
}

ithCanonicalConceptLibraryFileName <- function(fileName) {
  ext <- tolower(tools::file_ext(fileName))
  if (!nzchar(ext)) {
    ext <- "duckdb"
  }

  baseName <- basename(tools::file_path_sans_ext(fileName))
  normalizedBase <- gsub("(_V[0-9]+)+$", "", baseName)
  if (!nzchar(normalizedBase)) {
    normalizedBase <- baseName
  }

  paste0(normalizedBase, ".", ext)
}

ithResolveConceptLibraryLocation <- function(libraryPath,
                                             defaultFileName = "rubricConceptSetLibrary.duckdb") {
  if (is.null(libraryPath) || !nzchar(as.character(libraryPath[[1]]))) {
    stop("libraryPath must be a non-empty path to a directory or concept set library file")
  }

  resolvedPath <- ithResolveDuckdbLibraryPath(
    libraryPath = libraryPath,
    defaultFileName = ithCanonicalConceptLibraryFileName(defaultFileName),
    mustExist = FALSE
  )

  libraryDirectory <- dirname(resolvedPath)
  libraryFileName <- basename(resolvedPath)

  list(
    libraryDirectory = libraryDirectory,
    libraryFileName = libraryFileName,
    resolvedPath = resolvedPath
  )
}

ithScoringConceptLibraryPath <- function(outputDirectory,
                                         seedLibraryPath = NULL,
                                         fileName = "rubricConceptSetLibrary.duckdb") {
  ithResolveDuckdbLibraryPath(
    libraryPath = if (is.null(seedLibraryPath) || !nzchar(as.character(seedLibraryPath[[1]]))) outputDirectory else seedLibraryPath,
    defaultFileName = ithCanonicalConceptLibraryFileName(fileName),
    mustExist = TRUE
  )
}

# NULL, blank, NA, or "latest" means the newest library version; a positive integer pins that version.
ithResolveScoringConceptLibraryVersion <- function(conceptLibraryVersion,
                                                   outputDirectory,
                                                   seedLibraryPath = NULL,
                                                   fileName = "rubricConceptSetLibrary.duckdb",
                                                   verbose = TRUE) {
  raw <- if (is.null(conceptLibraryVersion) || length(conceptLibraryVersion) == 0L) NA else conceptLibraryVersion[[1]]
  rawText <- trimws(as.character(raw))
  if (is.na(raw) || !nzchar(rawText) || identical(tolower(rawText), "latest")) {
    libraryPath <- ithScoringConceptLibraryPath(outputDirectory, seedLibraryPath, fileName)
    latest <- ithDuckdbGetLatestLibraryVersion(libraryPath)
    if (is.null(latest)) {
      stop("Concept set library has no versions yet: ", libraryPath, ". Run createRubricConceptSetsFromConfig.R first.")
    }
    ithMaybeMessage(verbose, "Using latest concept library version ", latest, " (", libraryPath, ").")
    return(as.integer(latest))
  }
  pinned <- suppressWarnings(as.integer(rawText))
  if (is.na(pinned) || pinned < 1L || !grepl("^[0-9]+$", rawText)) {
    stop("conceptLibraryVersion must be blank, 'latest', or a positive integer; got: ", rawText)
  }
  ithMaybeMessage(verbose, "Using pinned concept library version ", pinned, " (conceptLibraryVersion in config).")
  pinned
}

ithLoadConceptSetLibraryForScoring <- function(outputDirectory,
                                               seedLibraryPath = NULL,
                                               fileName = "rubricConceptSetLibrary.duckdb",
                                               libraryVersion = NULL) {
  libraryVersion <- ithResolveScoringConceptLibraryVersion(
    libraryVersion, outputDirectory, seedLibraryPath, fileName, verbose = FALSE
  )
  resolvedLibraryPath <- ithScoringConceptLibraryPath(outputDirectory, seedLibraryPath, fileName)

  ithDuckdbGetConceptSetLibrary(
    libraryPath = resolvedLibraryPath,
    libraryVersion = as.integer(libraryVersion[[1]])
  )
}

ithResolveLatestConceptLibraryPath <- function(libraryPath,
                                               fileName = "rubricConceptSetLibrary.duckdb") {
  ithResolveDuckdbLibraryPath(
    libraryPath = libraryPath,
    defaultFileName = ithCanonicalConceptLibraryFileName(fileName),
    mustExist = TRUE
  )
}

ithUpdateConceptSetLibraryFromJson <- function(libraryPath,
                                               csName,
                                               conceptSetJsonPath,
                                               cleanName = NULL,
                                               runId = "manualUpdate",
                                               conditionName = NA_character_,
                                               cohortId = NA_integer_,
                                               verbose = TRUE) {
  if (is.null(csName) || !nzchar(as.character(csName[[1]]))) {
    stop("csName must be a non-empty string")
  }

  if (is.null(conceptSetJsonPath) || !nzchar(as.character(conceptSetJsonPath[[1]]))) {
    stop("conceptSetJsonPath must be a non-empty file path")
  }

  jsonPath <- as.character(conceptSetJsonPath[[1]])
  if (!file.exists(jsonPath)) {
    stop("conceptSetJsonPath does not exist: ", jsonPath)
  }

  resolvedLibraryPath <- ithResolveLatestConceptLibraryPath(
    libraryPath = libraryPath,
    fileName = "rubricConceptSetLibrary.duckdb"
  )

  if (!file.exists(resolvedLibraryPath)) {
    ithDuckdbInitializeLibrary(libraryPath = resolvedLibraryPath, verbose = verbose)
  }

  ithMaybeMessage(verbose, "Resolved concept set library: ", resolvedLibraryPath)

  csLibrary <- ithDuckdbGetConceptSetLibrary(libraryPath = resolvedLibraryPath)

  parsedJson <- tryCatch(
    jsonlite::fromJSON(jsonPath, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse concept set JSON file: ", jsonPath, " ; ", e$message)
    }
  )

  prepared <- ithPrepareConceptSetForResolution(parsedJson)
  if (is.null(prepared) || !is.list(prepared) || is.null(prepared$items)) {
    stop("JSON concept set is not in a supported format: ", jsonPath)
  }

  conceptSetName <- ithNormalizeConceptSetName(csName[[1]])
  resolvedCleanName <- cleanName
  if (is.null(resolvedCleanName) || !nzchar(as.character(resolvedCleanName[[1]]))) {
    resolvedCleanName <- gsub("_", " ", conceptSetName)
  } else {
    resolvedCleanName <- as.character(resolvedCleanName[[1]])
  }

  existingEmbed <- NULL
  existingCleanName <- resolvedCleanName
  conceptIdx <- which(tolower(as.character(csLibrary[[1]]$conceptSetName)) == tolower(conceptSetName))
  if (length(conceptIdx) > 0) {
    candidateCleanName <- as.character(csLibrary[[1]]$cleanName[[conceptIdx[[1]]]])
    if (nzchar(candidateCleanName)) {
      existingCleanName <- candidateCleanName
    }
  }

  embedIdx <- which(tolower(as.character(csLibrary[[2]]$conceptSetName)) == tolower(conceptSetName))
  if (length(embedIdx) > 0) {
    existingEmbed <- csLibrary[[2]]$embed[[embedIdx[[1]]]]
  }

  embeddingVec <- tryCatch(
    ithGetEmbeddingVectorsInternal(tolower(resolvedCleanName), reportTime = FALSE, silent = TRUE)[[1]],
    error = function(e) {
      ithMaybeMessage(
        verbose,
        "Embedding generation failed for concept set '",
        conceptSetName,
        "'. Error: ",
        e$message,
        ". Reusing prior embedding or placeholder."
      )
      NULL
    }
  )

  if (is.null(embeddingVec) || !is.numeric(embeddingVec) || length(embeddingVec) <= 1 || any(!is.finite(embeddingVec))) {
    if (!is.null(existingEmbed) && is.numeric(existingEmbed) && length(existingEmbed) > 1 && all(is.finite(existingEmbed))) {
      embeddingVec <- existingEmbed
    } else {
      embeddingVec <- NA_real_
    }
  }

  upsertResult <- ithDuckdbUpsertConceptSet(
    libraryPath = resolvedLibraryPath,
    conceptSetName = conceptSetName,
    cleanName = existingCleanName,
    conceptSet = parsedJson,
    embedding = embeddingVec,
    runId = runId,
    userName = Sys.info()[["user"]],
    conditionName = conditionName,
    cohortId = cohortId,
    sourceFile = jsonPath
  )

  savedPath <- resolvedLibraryPath

  ithMaybeMessage(verbose, "Saved updated concept set library: ", savedPath)

  invisible(list(
    libraryInputPath = as.character(libraryPath[[1]]),
    resolvedLibraryPath = resolvedLibraryPath,
    savedLibraryPath = savedPath,
    conceptSetName = conceptSetName,
    cleanName = existingCleanName,
    conceptSetJsonPath = jsonPath,
    updateMode = "insertOrReplaceWithEmbedding",
    libraryVersion = upsertResult$libraryVersion
  ))
}

ithNormalizeConceptSetLibraryStorage <- function(libraryPath,
                                                 outputFileName = NULL,
                                                 replaceUnreadableWithEmpty = TRUE,
                                                 runId = "normalizeConceptSetLibraryStorage",
                                                 verbose = TRUE) {
  resolvedLibraryPath <- ithResolveLatestConceptLibraryPath(
    libraryPath = libraryPath,
    fileName = "rubricConceptSetLibrary.duckdb"
  )

  ithMaybeMessage(verbose, "Resolved concept set library: ", resolvedLibraryPath)

  csLibrary <- ithDuckdbGetConceptSetLibrary(libraryPath = resolvedLibraryPath)
  if (!is.list(csLibrary) || length(csLibrary) < 2) {
    stop("Concept set library must be a list with concept and embedding tables")
  }

  conceptTable <- csLibrary[[1]]
  if (!is.data.frame(conceptTable) || !all(c("conceptSetName", "cleanName", "conceptSet") %in% names(conceptTable))) {
    stop("Concept set table is missing required columns: conceptSetName, cleanName, conceptSet")
  }

  if (nrow(conceptTable) == 0) {
    stop("Concept set table has no rows to normalize")
  }

  normalizedCount <- 0L
  replacedUnreadableCount <- 0L
  unreadableConceptSetNames <- character()

  for (rowUp in seq_len(nrow(conceptTable))) {
    conceptSetName <- as.character(conceptTable$conceptSetName[[rowUp]])
    prepared <- tryCatch(
      ithPrepareConceptSetForResolution(conceptTable$conceptSet[[rowUp]]),
      error = function(e) NULL
    )

    if (is.list(prepared) && !is.null(prepared$items)) {
      conceptTable$conceptSet[[rowUp]] <- list(items = prepared$items)
      normalizedCount <- normalizedCount + 1L
      next
    }

    unreadableConceptSetNames <- c(unreadableConceptSetNames, conceptSetName)

    if (isTRUE(replaceUnreadableWithEmpty)) {
      conceptTable$conceptSet[[rowUp]] <- list(items = list())
      replacedUnreadableCount <- replacedUnreadableCount + 1L
      next
    }
  }

  latestVersion <- ithDuckdbGetLatestLibraryVersion(resolvedLibraryPath)
  if (is.null(latestVersion)) {
    latestVersion <- 1L
  }

  for (rowUp in seq_len(nrow(conceptTable))) {
    conceptSetName <- as.character(conceptTable$conceptSetName[[rowUp]])
    cleanName <- as.character(conceptTable$cleanName[[rowUp]])

    embedIdx <- which(tolower(as.character(csLibrary[[2]]$conceptSetName)) == tolower(conceptSetName))
    embedValue <- if (length(embedIdx) > 0) csLibrary[[2]]$embed[[embedIdx[[1]]]] else NA_real_

    ithDuckdbUpsertConceptSet(
      libraryPath = resolvedLibraryPath,
      conceptSetName = conceptSetName,
      cleanName = cleanName,
      conceptSet = conceptTable$conceptSet[[rowUp]],
      embedding = embedValue,
      runId = runId,
      userName = Sys.info()[["user"]],
      conditionName = NA_character_,
      cohortId = NA_integer_,
      sourceFile = as.character(outputFileName)
    )
  }

  finalVersion <- ithDuckdbGetLatestLibraryVersion(resolvedLibraryPath)
  savedPath <- resolvedLibraryPath

  ithMaybeMessage(verbose, "Saved normalized concept set library: ", savedPath)

  invisible(list(
    libraryInputPath = as.character(libraryPath[[1]]),
    resolvedLibraryPath = resolvedLibraryPath,
    savedLibraryPath = savedPath,
    normalizedCount = normalizedCount,
    replacedUnreadableCount = replacedUnreadableCount,
    unreadableConceptSetNames = unique(unreadableConceptSetNames),
    replaceUnreadableWithEmpty = isTRUE(replaceUnreadableWithEmpty),
    startLibraryVersion = latestVersion,
    finalLibraryVersion = finalVersion
  ))
}

ithDeleteConceptSetFromLibrary <- function(libraryPath,
                                           conceptSetName,
                                           dryRun = TRUE,
                                           verbose = TRUE,
                                           cleanNameOnly = FALSE) {
  if (is.null(conceptSetName) || !nzchar(as.character(conceptSetName[[1]]))) {
    stop("conceptSetName must be a non-empty string")
  }

  resolvedLibraryPath <- ithResolveLatestConceptLibraryPath(
    libraryPath = libraryPath,
    fileName = "rubricConceptSetLibrary.duckdb"
  )

  connection <- ithConnectDuckdbConceptLibrary(resolvedLibraryPath, readOnly = FALSE)
  on.exit(ithDisconnectDuckdbConceptLibrary(connection), add = TRUE)
  ithValidateDuckdbConceptLibrarySchema(connection, libraryPath = resolvedLibraryPath)

  targetName <- trimws(as.character(conceptSetName[[1]]))
  matchPredicate <- if (isTRUE(cleanNameOnly)) {
    paste0(
      "lower(cleanName) = lower(",
      DBI::dbQuoteString(connection, targetName),
      ")"
    )
  } else {
    paste0(
      "lower(conceptSetName) = lower(",
      DBI::dbQuoteString(connection, targetName),
      ") OR lower(cleanName) = lower(",
      DBI::dbQuoteString(connection, targetName),
      ")"
    )
  }

  preview <- DBI::dbGetQuery(
    connection,
    paste0(
      "SELECT conceptSetName, cleanName, libraryVersion, runId, conditionName, updatedAt ",
      "FROM conceptSetCurrent ",
      "WHERE ", matchPredicate, " ",
      "ORDER BY updatedAt DESC"
    )
  )

  if (nrow(preview) == 0) {
    ithMaybeMessage(verbose, "No concept set matched for deletion: ", targetName)
    return(invisible(list(
      libraryInputPath = as.character(libraryPath[[1]]),
      resolvedLibraryPath = resolvedLibraryPath,
      conceptSetName = targetName,
      matchedConceptSets = character(),
      revisionRows = data.frame(),
      deleted = FALSE,
      dryRun = isTRUE(dryRun)
    )))
  }

  matchedConceptSets <- unique(as.character(preview$conceptSetName))
  matchedQuoted <- paste(DBI::dbQuoteString(connection, matchedConceptSets), collapse = ", ")
  revisionRows <- DBI::dbGetQuery(
    connection,
    paste0(
      "SELECT revisionId, conceptSetName, libraryVersion, runId, conditionName ",
      "FROM conceptSetRevision ",
      "WHERE conceptSetName IN (", matchedQuoted, ") ",
      "ORDER BY conceptSetName, libraryVersion, revisionId"
    )
  )

  ithMaybeMessage(
    verbose,
    "Matched concept set(s) for deletion: ",
    paste(matchedConceptSets, collapse = ", "),
    " (revision rows: ",
    nrow(revisionRows),
    ")"
  )

  if (isTRUE(dryRun)) {
    return(invisible(list(
      libraryInputPath = as.character(libraryPath[[1]]),
      resolvedLibraryPath = resolvedLibraryPath,
      conceptSetName = targetName,
      matchedConceptSets = matchedConceptSets,
      revisionRows = revisionRows,
      deleted = FALSE,
      dryRun = TRUE
    )))
  }

  DBI::dbExecute(connection, "BEGIN TRANSACTION")
  rollbackPending <- TRUE
  on.exit({
    if (rollbackPending) {
      tryCatch(DBI::dbExecute(connection, "ROLLBACK"), error = function(e) NULL)
    }
  }, add = TRUE)

  DBI::dbExecute(
    connection,
    paste0("DELETE FROM conceptSetRevision WHERE conceptSetName IN (", matchedQuoted, ")")
  )
  DBI::dbExecute(
    connection,
    paste0("DELETE FROM conceptSetCurrent WHERE conceptSetName IN (", matchedQuoted, ")")
  )

  DBI::dbExecute(connection, "DELETE FROM conceptSetCurrent")
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO conceptSetCurrent(",
      "conceptSetName, cleanName, conceptSetJson, embedJson, libraryVersion,",
      "updatedAt, runId, userName, conditionName, cohortId, sourceFile",
      ")",
      "SELECT conceptSetName, cleanName, conceptSetJson, embedJson, libraryVersion,",
      "CURRENT_TIMESTAMP AS updatedAt, runId, userName, conditionName, cohortId, sourceFile",
      "FROM conceptSetRevision",
      "QUALIFY row_number() OVER (PARTITION BY lower(conceptSetName) ORDER BY libraryVersion DESC, revisionId DESC) = 1"
    )
  )

  DBI::dbExecute(connection, "COMMIT")
  rollbackPending <- FALSE

  invisible(list(
    libraryInputPath = as.character(libraryPath[[1]]),
    resolvedLibraryPath = resolvedLibraryPath,
    conceptSetName = targetName,
    matchedConceptSets = matchedConceptSets,
    revisionRows = revisionRows,
    deleted = TRUE,
    dryRun = FALSE
  ))
}

ithExtractNeededConceptSetNames <- function(neededConcepts) {
  allCs <- character()
  for (rowUp in seq_len(nrow(neededConcepts))) {
    allCs <- c(allCs, ithParseConceptSets(neededConcepts$conceptSetsNeeded[[rowUp]]))
  }
  unique(tolower(allCs))
}

ithNormalizeConceptSetName <- function(x) {
  normalized <- tolower(trimws(as.character(x)))
  normalized <- gsub("[[:space:]]+", "_", normalized)
  normalized <- gsub("[^a-z0-9_]+", "_", normalized)
  normalized <- gsub("_+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized <- ithCollapseDuplicateConceptSetTokens(normalized)
  normalized
}

ithCollapseDuplicateConceptSetTokens <- function(x) {
  value <- as.character(x)
  repeat {
    collapsed <- gsub(
      "(^|_)([a-z0-9]+)_\\2(?=_|$)",
      "\\1\\2",
      value,
      perl = TRUE
    )
    if (identical(collapsed, value)) {
      return(value)
    }
    value <- collapsed
  }
}

ithNormalizeConceptSetCoreName <- function(x) {
  ithStripConceptSetSuffix(ithNormalizeConceptSetName(x))
}

ithInferConceptSetFamily <- function(nameNormalized) {
  name <- ithNormalizeConceptSetName(nameNormalized)

  if (grepl("_procedure_codes$", name)) {
    return("procedure")
  }

  if (grepl("_(diagnosis|condition)_codes$", name)) {
    return("conditionDiagnosis")
  }

  if (grepl("_(medication|drug)_codes$", name)) {
    return("medicationDrug")
  }

  if (grepl("_(lab|labs|measurement)_codes$", name)) {
    return("labMeasurement")
  }

  if (grepl("_(visit|visits_visit)_codes$", name)) {
    return("visit")
  }

  if (grepl("_observation_codes$", name)) {
    return("observation")
  }

  if (grepl("_device_codes$", name)) {
    return("device")
  }

  "unknown"
}

ithIsConceptSetFamilyCompatible <- function(targetFamily, candidateFamily) {
  if (is.na(targetFamily) || !nzchar(targetFamily) || identical(targetFamily, "unknown")) {
    return(TRUE)
  }

  if (is.na(candidateFamily) || !nzchar(candidateFamily) || identical(candidateFamily, "unknown")) {
    return(TRUE)
  }

  identical(targetFamily, candidateFamily)
}

ithComputeFuzzyNameSimilarity <- function(targetName, candidateName) {
  target <- as.character(targetName)
  candidate <- as.character(candidateName)

  if (!nzchar(target) || !nzchar(candidate)) {
    return(0)
  }

  lev <- utils::adist(target, candidate)[1]
  maxLen <- max(nchar(target), nchar(candidate))
  levSim <- if (maxLen == 0) 0 else 1 - (lev / maxLen)

  targetTokens <- unique(strsplit(target, "_", fixed = TRUE)[[1]])
  candidateTokens <- unique(strsplit(candidate, "_", fixed = TRUE)[[1]])
  tokenUnion <- unique(c(targetTokens, candidateTokens))
  tokenIntersection <- intersect(targetTokens, candidateTokens)
  tokenJaccard <- if (length(tokenUnion) == 0) 0 else length(tokenIntersection) / length(tokenUnion)

  max(levSim, tokenJaccard)
}

ithGenerateFuzzyNameVariants <- function(nameNormalized) {
  variants <- unique(c(
    nameNormalized,
    gsub("condition_codes", "diagnosis_codes", nameNormalized, fixed = TRUE),
    gsub("diagnosis_codes", "condition_codes", nameNormalized, fixed = TRUE),
    gsub("visits_visit_codes", "visit_codes", nameNormalized, fixed = TRUE),
    gsub("visit_codes", "visits_visit_codes", nameNormalized, fixed = TRUE)
  ))

  variants <- variants[nzchar(variants)]
  unique(variants)
}

ithStripConceptSetSuffix <- function(nameNormalized) {
  normalized <- ithNormalizeConceptSetName(nameNormalized)

  suffixPatterns <- c(
    "_procedure_codes$",
    "_diagnosis_codes$",
    "_condition_codes$",
    "_medication_codes$",
    "_drug_codes$",
    "_lab_codes$",
    "_labs_codes$",
    "_measurement_codes$",
    "_visit_codes$",
    "_visits_visit_codes$",
    "_observation_codes$",
    "_device_codes$"
  )

  stripOne <- function(value) {
    stripped <- value

    if (grepl("_(visit|visits_visit)_codes$", stripped)) {
      stripped <- gsub("_codes$", "", stripped)
      if (nzchar(stripped)) {
        return(stripped)
      }
    }

    for (patternUp in suffixPatterns) {
      candidate <- gsub(patternUp, "", stripped)
      if (!identical(candidate, stripped) && nzchar(candidate)) {
        stripped <- candidate
        break
      }
    }

    fallback <- gsub("_codes$", "", stripped)
    if (nzchar(fallback)) {
      stripped <- fallback
    }

    stripped
  }

  strippedAll <- vapply(normalized, stripOne, character(1))
  if (length(strippedAll) == 1) {
    return(strippedAll[[1]])
  }

  strippedAll
}

ithInferHecateSearchFilters <- function(targetName) {
  normalized <- tolower(ithNormalizeConceptSetName(targetName))
  if (grepl("_(diagnosis|condition)_codes$", normalized)) {
    return(list(domains = "Condition", conceptClasses = NULL))
  }
  if (grepl("_(medication|drug)_codes$", normalized)) {
    return(list(domains = "Drug", conceptClasses = NULL))
  }
  if (grepl("_procedure_codes$", normalized)) {
    return(list(domains = "Procedure", conceptClasses = NULL))
  }
  if (grepl("_(measurement|lab|labs)_codes$", normalized)) {
    return(list(domains = "Measurement", conceptClasses = NULL))
  }
  if (grepl("_observation_codes$", normalized)) {
    return(list(domains = "Observation", conceptClasses = NULL))
  }
  if (grepl("_device_codes$", normalized)) {
    return(list(domains = "Device", conceptClasses = NULL))
  }
  if (grepl("_(visit|visits_visit)_codes$", normalized)) {
    return(list(domains = "Visit", conceptClasses = NULL))
  }
  list(domains = NULL, conceptClasses = NULL)
}

ithGetHecateSearchTerms <- function(searchTerm, llmClient = NULL) {
  if (is.null(llmClient)) {
    return(searchTerm)
  }

  prompt <- paste0(
    "Generate up to 10 clinically meaningful search phrases for the concept-set name '" ,
    searchTerm,
    "'. Expand abbreviations and acronyms. Return only a semicolon-separated list of phrases, with no explanation."
  )
  synonymText <- tryCatch(
    ithQueryLlm(llmClient, prompt, output = "text", retries = 2, silent = TRUE),
    error = function(e) ""
  )
  synonyms <- unlist(strsplit(as.character(synonymText), "[;\\n]", perl = TRUE), use.names = FALSE)
  synonyms <- trimws(gsub("^[\\\"']|[\\\"']$", "", synonyms))
  synonyms <- unique(synonyms[nzchar(synonyms) & !is.na(synonyms)])
  unique(c(searchTerm, head(synonyms, 10L)))
}

ithGetHecateCandidateDetails <- function(targetName, limit = 5, llmClient = NULL) {
  targetNormalized <- ithNormalizeConceptSetName(targetName)
  searchTerm <- ithStripConceptSetSuffix(targetNormalized)

  if (!nzchar(searchTerm)) {
    searchTerm <- targetNormalized
  }

  searchTerms <- ithGetHecateSearchTerms(searchTerm, llmClient = llmClient)
  searchFilters <- ithInferHecateSearchFilters(targetName)

  hecateRows <- do.call(rbind, lapply(searchTerms, function(term) {
    tryCatch(
      ithVectorSearchStandard(
        term = term,
        domains = searchFilters$domains,
        conceptClasses = searchFilters$conceptClasses,
        limit = limit,
        maxRetries = 2,
        waitTime = 1
      ),
      error = function(e) NULL
    )
  }))

  if (is.null(hecateRows) || nrow(hecateRows) == 0 || !("conceptName" %in% names(hecateRows))) {
    return(list(
      searchTerm = paste(searchTerms, collapse = " | "),
      candidates = character(),
      rawCandidates = character(),
      rejectedCandidates = character(),
      usedFallback = FALSE
    ))
  }

  candidates <- unique(as.character(hecateRows$conceptName))
  candidates <- candidates[nzchar(candidates) & !is.na(candidates)]

  if (length(candidates) == 0) {
    return(list(
      searchTerm = paste(searchTerms, collapse = " | "),
      candidates = character(),
      rawCandidates = character(),
      rejectedCandidates = character(),
      usedFallback = FALSE
    ))
  }

  searchCores <- unique(ithNormalizeConceptSetCoreName(searchTerms))
  targetCore <- ithNormalizeConceptSetCoreName(targetName)
  targetTokens <- unique(strsplit(targetCore, "_", fixed = TRUE)[[1]])

  keepCandidate <- function(candidateName) {
    candidateCore <- ithNormalizeConceptSetCoreName(candidateName)
    candidateTokens <- unique(strsplit(candidateCore, "_", fixed = TRUE)[[1]])

    searchSimilarities <- vapply(
      searchCores,
      function(searchCore) ithComputeFuzzyNameSimilarity(searchCore, candidateCore),
      numeric(1)
    )
    bestSearchSimilarity <- max(searchSimilarities, na.rm = TRUE)
    if (length(searchTerms) > 1L && bestSearchSimilarity >= 0.72) {
      return(TRUE)
    }

    tokenIntersection <- intersect(targetTokens, candidateTokens)
    tokenUnion <- unique(c(targetTokens, candidateTokens))
    tokenJaccard <- if (length(tokenUnion) == 0) 0 else length(tokenIntersection) / length(tokenUnion)
    fuzzySimilarity <- ithComputeFuzzyNameSimilarity(targetCore, candidateCore)

    if (length(targetTokens) >= 4) {
      return(length(tokenIntersection) >= 3 || fuzzySimilarity >= 0.85 || tokenJaccard >= 0.65)
    }

    if (length(targetTokens) == 3) {
      return(length(tokenIntersection) >= 2 || fuzzySimilarity >= 0.82 || tokenJaccard >= 0.60)
    }

    if (length(targetTokens) == 2) {
      return(length(tokenIntersection) >= 2 || fuzzySimilarity >= 0.78)
    }

    fuzzySimilarity >= 0.72
  }

  filteredCandidates <- candidates[vapply(candidates, keepCandidate, logical(1))]
  rejectedCandidates <- setdiff(candidates, filteredCandidates)
  usedFallback <- FALSE

  if (length(filteredCandidates) == 0) {
    filteredCandidates <- candidates
    rejectedCandidates <- character()
    usedFallback <- TRUE
  }

  list(
    searchTerm = paste(searchTerms, collapse = " | "),
    candidates = filteredCandidates[seq_len(min(length(filteredCandidates), limit))],
    rawCandidates = candidates[seq_len(min(length(candidates), limit))],
    rejectedCandidates = rejectedCandidates[seq_len(min(length(rejectedCandidates), limit))],
    usedFallback = usedFallback
  )
}

ithGetHecateCandidateNames <- function(targetName, limit = 5) {
  ithGetHecateCandidateDetails(targetName = targetName, limit = limit)$candidates
}

ithBuildFuzzyNamesTriedText <- function(fuzzyVariantsText, hecateCandidatesText, includeHecateCandidates) {
  if (!isTRUE(includeHecateCandidates)) {
    return(fuzzyVariantsText)
  }

  hecateText <- hecateCandidatesText
  if (is.null(hecateText) || !nzchar(hecateText)) {
    hecateText <- "(none)"
  }

  paste0(
    "variants=", fuzzyVariantsText,
    " ; hecateTopCandidates=", hecateText
  )
}

ithConceptSetMatchAdjudicationType <- function() {
  ellmer::type_object(
    decision = ellmer::type_string(),
    selectedConceptSetName = ellmer::type_string(),
    confidence = ellmer::type_string(),
    rationale = ellmer::type_string()
  )
}

ithBuildConceptSetMatchAdjudicationPrompt <- function(targetName,
                                                      conditionName,
                                                      candidateTableJson,
                                                      similarityThreshold,
                                                      fuzzyThreshold,
                                                      top2Gap) {
  conditionText <- if (!is.null(conditionName) && nzchar(as.character(conditionName[[1]]))) {
    as.character(conditionName[[1]])
  } else {
    "(not provided)"
  }

  template <- tryCatch(
    ithReadPrompt("conceptSetMatchAdjudication.txt"),
    error = function(e) NULL
  )

  if (is.null(template) || !nzchar(template)) {
    template <- paste0(
      "You are adjudicating a concept set name match for a phenotype rubric pipeline.\n",
      "Condition context: <CONDITION_NAME>\n",
      "Requested concept set name: <TARGET_NAME>\n\n",
      "Candidate concepts are ranked by embedding and fuzzy signals.\n",
      "Primary threshold guidance:\n",
      "- embedding similarity threshold: <SIMILARITY_THRESHOLD>\n",
      "- fuzzy similarity threshold: <FUZZY_THRESHOLD>\n",
      "- current top-2 embedding gap: <TOP2_GAP>\n\n",
      "Decision rules:\n",
      "1) Choose ONE candidate only if it is a clinically and semantically strong match.\n",
      "2) If no candidate is a good match, return NO_MATCH.\n",
      "3) selectedConceptSetName must exactly match one candidate conceptSetName OR be NO_MATCH.\n",
      "4) confidence should be a number between 0 and 1.\n\n",
      "Candidate table JSON:\n",
      "<CANDIDATE_TABLE_JSON>\n\n",
      "Return fields:\n",
      "- decision: select_candidate | no_good_match\n",
      "- selectedConceptSetName\n",
      "- confidence\n",
      "- rationale"
    )
  }

  prompt <- template
  prompt <- gsub("<CONDITION_NAME>", conditionText, prompt, fixed = TRUE)
  prompt <- gsub("<TARGET_NAME>", as.character(targetName), prompt, fixed = TRUE)
  prompt <- gsub("<SIMILARITY_THRESHOLD>", sprintf("%.4f", similarityThreshold), prompt, fixed = TRUE)
  prompt <- gsub("<FUZZY_THRESHOLD>", sprintf("%.4f", fuzzyThreshold), prompt, fixed = TRUE)
  prompt <- gsub("<TOP2_GAP>", ifelse(is.na(top2Gap), "NA", sprintf("%.4f", top2Gap)), prompt, fixed = TRUE)
  prompt <- gsub("<CANDIDATE_TABLE_JSON>", candidateTableJson, prompt, fixed = TRUE)
  prompt
}

ithAdjudicateConceptSetMatch <- function(targetName,
                                         conditionName,
                                         candidateTable,
                                         similarityThreshold,
                                         fuzzyThreshold,
                                         top2Gap,
                                         llmClient,
                                         verbose = TRUE) {
  if (is.null(llmClient)) {
    return(list(
      used = FALSE,
      available = FALSE,
      selectedConceptSetName = "",
      decision = "llm_unavailable",
      confidence = NA_real_,
      rationale = "LLM client is NULL",
      error = "LLM client is NULL"
    ))
  }

  candidateJson <- jsonlite::toJSON(candidateTable, auto_unbox = TRUE, pretty = TRUE)
  prompt <- ithBuildConceptSetMatchAdjudicationPrompt(
    targetName = targetName,
    conditionName = conditionName,
    candidateTableJson = candidateJson,
    similarityThreshold = similarityThreshold,
    fuzzyThreshold = fuzzyThreshold,
    top2Gap = top2Gap
  )

  decisionRaw <- tryCatch(
    ithQueryLlm(
      llmClient = llmClient,
      prompt = prompt,
      output = "list",
      ellmerTypeObject = ithConceptSetMatchAdjudicationType(),
      silent = !isTRUE(verbose)
    ),
    error = function(e) {
      list(`__error__` = e$message)
    }
  )

  if (!is.null(decisionRaw$`__error__`)) {
    return(list(
      used = TRUE,
      available = FALSE,
      selectedConceptSetName = "",
      decision = "llm_error",
      confidence = NA_real_,
      rationale = "",
      error = as.character(decisionRaw$`__error__`)
    ))
  }

  decision <- tolower(trimws(as.character(decisionRaw$decision %||% "")))
  selectedName <- as.character(decisionRaw$selectedConceptSetName %||% "")
  confidenceRaw <- as.character(decisionRaw$confidence %||% "")
  rationale <- as.character(decisionRaw$rationale %||% "")

  confidence <- suppressWarnings(as.numeric(confidenceRaw))
  if (is.na(confidence)) {
    confidence <- NA_real_
  }

  if (!nzchar(selectedName)) {
    selectedName <- "NO_MATCH"
  }

  list(
    used = TRUE,
    available = TRUE,
    selectedConceptSetName = selectedName,
    decision = decision,
    confidence = confidence,
    rationale = rationale,
    error = ""
  )
}

ithMatchConceptSets <- function(neededNames,
                                conceptSetNamesLibrary,
                                similarityThreshold = 0.94,
                                fuzzyThreshold = 0.94,
                                returnDiagnostics = FALSE,
                                topCandidates = 5,
                                includeHecateCandidates = FALSE,
                                useLlmAdjudication = TRUE,
                                llmClient = NULL,
                                llmTopCandidates = 25,
                                llmAmbiguityMinScore = NULL,
                                llmAmbiguityMaxScore = NULL,
                                llmTop2GapThreshold = 0.015,
                                llmConfidenceThreshold = 0.60,
                                conditionName = "",
                                verbose = TRUE,
                                skipInitialLlmPass = FALSE) {
  emptyMatch <- data.frame(csName = character(), libCsName = character(), coSim = numeric(), stringsAsFactors = FALSE)
  emptyDiagnostics <- data.frame(
    csName = character(),
    csNameNormalized = character(),
    matchMethod = character(),
    matchedLibCsName = character(),
    matchedCoSim = numeric(),
    matchedEmbeddingSim = numeric(),
    matchedFuzzySim = numeric(),
    matchedCombinedSim = numeric(),
    bestCandidate = character(),
    bestCandidateCoSim = numeric(),
    bestCandidateEmbeddingSim = numeric(),
    bestCandidateFuzzySim = numeric(),
    bestCandidateCombinedSim = numeric(),
    fuzzyNamesTried = character(),
    fuzzyNameVariantsTried = character(),
    topCandidates = character(),
    libraryTopCandidates = character(),
    hecateSearchTerm = character(),
    hecateTopCandidates = character(),
    hecateTopCandidatesRaw = character(),
    hecateTopCandidatesAccepted = character(),
    hecateTopCandidatesRejected = character(),
    similarityThreshold = numeric(),
    fuzzyThreshold = numeric(),
    note = character(),
    stringsAsFactors = FALSE
  )

  if (length(neededNames) == 0) {
    if (isTRUE(returnDiagnostics)) {
      return(list(matches = emptyMatch, diagnostics = emptyDiagnostics))
    }
    return(emptyMatch)
  }

  if (isTRUE(includeHecateCandidates) && isTRUE(useLlmAdjudication) &&
      !is.null(llmClient) && !isTRUE(skipInitialLlmPass)) {
    initialResult <- ithMatchConceptSets(
      neededNames = neededNames,
      conceptSetNamesLibrary = conceptSetNamesLibrary,
      similarityThreshold = similarityThreshold,
      fuzzyThreshold = fuzzyThreshold,
      returnDiagnostics = TRUE,
      topCandidates = topCandidates,
      includeHecateCandidates = FALSE,
      useLlmAdjudication = FALSE,
      llmClient = NULL,
      llmTopCandidates = llmTopCandidates,
      llmAmbiguityMinScore = llmAmbiguityMinScore,
      llmAmbiguityMaxScore = llmAmbiguityMaxScore,
      llmTop2GapThreshold = llmTop2GapThreshold,
      llmConfidenceThreshold = llmConfidenceThreshold,
      conditionName = conditionName,
      verbose = verbose,
      skipInitialLlmPass = TRUE
    )

    unresolvedNames <- setdiff(
      unique(tolower(as.character(neededNames))),
      unique(tolower(as.character(initialResult$matches$csName)))
    )
    if (length(unresolvedNames) == 0L) {
      return(if (isTRUE(returnDiagnostics)) initialResult else initialResult$matches)
    }

    expandedResult <- ithMatchConceptSets(
      neededNames = unresolvedNames,
      conceptSetNamesLibrary = conceptSetNamesLibrary,
      similarityThreshold = similarityThreshold,
      fuzzyThreshold = fuzzyThreshold,
      returnDiagnostics = TRUE,
      topCandidates = topCandidates,
      includeHecateCandidates = TRUE,
      useLlmAdjudication = TRUE,
      llmClient = llmClient,
      llmTopCandidates = llmTopCandidates,
      llmAmbiguityMinScore = llmAmbiguityMinScore,
      llmAmbiguityMaxScore = llmAmbiguityMaxScore,
      llmTop2GapThreshold = llmTop2GapThreshold,
      llmConfidenceThreshold = llmConfidenceThreshold,
      conditionName = conditionName,
      verbose = verbose,
      skipInitialLlmPass = TRUE
    )

    # Keep one diagnostics row per name: the expanded pass supersedes the first-pass row.
    initialDiagnostics <- initialResult$diagnostics
    if (!is.null(initialDiagnostics) && nrow(initialDiagnostics) > 0) {
      initialDiagnostics <- initialDiagnostics[!(tolower(as.character(initialDiagnostics$csName)) %in% unresolvedNames), , drop = FALSE]
    }
    combinedResult <- list(
      matches = rbind(initialResult$matches, expandedResult$matches),
      diagnostics = rbind(initialDiagnostics, expandedResult$diagnostics)
    )
    return(if (isTRUE(returnDiagnostics)) combinedResult else combinedResult$matches)
  }

  if (nrow(conceptSetNamesLibrary) == 0 || !("conceptSetName" %in% names(conceptSetNamesLibrary))) {
    neededNamesUnique <- unique(tolower(neededNames))
    normalizedNeeded <- ithNormalizeConceptSetName(neededNamesUnique)
    hecateCandidates <- rep("", length(neededNamesUnique))
    hecateCandidatesRaw <- rep("", length(neededNamesUnique))
    hecateCandidatesRejected <- rep("", length(neededNamesUnique))
    hecateSearchTerms <- rep("", length(neededNamesUnique))
    if (isTRUE(includeHecateCandidates)) {
      hecateDetails <- lapply(
        neededNamesUnique,
        function(x) ithGetHecateCandidateDetails(
          x,
          limit = topCandidates,
          llmClient = if (isTRUE(useLlmAdjudication)) llmClient else NULL
        )
      )
      hecateSearchTerms <- vapply(hecateDetails, function(x) x$searchTerm, character(1))
      hecateCandidates <- vapply(hecateDetails, function(x) paste(x$candidates, collapse = " | "), character(1))
      hecateCandidatesRaw <- vapply(hecateDetails, function(x) paste(x$rawCandidates, collapse = " | "), character(1))
      hecateCandidatesRejected <- vapply(hecateDetails, function(x) paste(x$rejectedCandidates, collapse = " | "), character(1))
    }

    diagnostics <- data.frame(
      csName = neededNamesUnique,
      csNameNormalized = normalizedNeeded,
      matchMethod = "none",
      matchedLibCsName = "",
      matchedCoSim = NA_real_,
      matchedEmbeddingSim = NA_real_,
      matchedFuzzySim = NA_real_,
      matchedCombinedSim = NA_real_,
      bestCandidate = "",
      bestCandidateCoSim = NA_real_,
      bestCandidateEmbeddingSim = NA_real_,
      bestCandidateFuzzySim = NA_real_,
      bestCandidateCombinedSim = NA_real_,
      fuzzyNamesTried = vapply(
        seq_along(normalizedNeeded),
        function(i) {
          variantsText <- paste(ithGenerateFuzzyNameVariants(normalizedNeeded[[i]]), collapse = " | ")
          ithBuildFuzzyNamesTriedText(variantsText, hecateCandidates[[i]], includeHecateCandidates)
        },
        character(1)
      ),
      fuzzyNameVariantsTried = vapply(normalizedNeeded, function(x) paste(ithGenerateFuzzyNameVariants(x), collapse = " | "), character(1)),
      topCandidates = "",
      libraryTopCandidates = "",
      hecateSearchTerm = hecateSearchTerms,
      hecateTopCandidates = hecateCandidates,
      hecateTopCandidatesRaw = hecateCandidatesRaw,
      hecateTopCandidatesAccepted = hecateCandidates,
      hecateTopCandidatesRejected = hecateCandidatesRejected,
      similarityThreshold = similarityThreshold,
      fuzzyThreshold = fuzzyThreshold,
      note = "Library is empty or missing conceptSetName column",
      stringsAsFactors = FALSE
    )

    if (isTRUE(returnDiagnostics)) {
      return(list(matches = emptyMatch, diagnostics = diagnostics))
    }
    return(emptyMatch)
  }

  neededNames <- unique(tolower(neededNames))
  neededNorm <- ithNormalizeConceptSetName(neededNames)
  neededCore <- ithNormalizeConceptSetCoreName(neededNames)
  libraryNames <- as.character(conceptSetNamesLibrary$conceptSetName)
  libraryNorm <- ithNormalizeConceptSetName(libraryNames)
  libraryCore <- ithNormalizeConceptSetCoreName(libraryNames)
  libraryFamily <- vapply(libraryNorm, ithInferConceptSetFamily, character(1))
  hasEmbeddings <- ("embed" %in% names(conceptSetNamesLibrary))

  results <- emptyMatch
  diagnostics <- emptyDiagnostics

  for (neededUp in seq_along(neededNames)) {
    target <- neededNames[[neededUp]]
    targetNorm <- neededNorm[[neededUp]]
    targetCore <- neededCore[[neededUp]]
    targetFamily <- ithInferConceptSetFamily(targetNorm)
    compatibleFamily <- vapply(
      libraryFamily,
      function(candidateFamily) ithIsConceptSetFamilyCompatible(targetFamily, candidateFamily),
      logical(1)
    )

    if (!any(compatibleFamily)) {
      compatibleFamily <- rep(TRUE, length(libraryFamily))
    }

    hecateSearchTerm <- ""
    hecateCandidateNames <- character()
    hecateCandidatesText <- ""
    hecateCandidateNamesRaw <- character()
    hecateCandidatesRawText <- ""
    hecateCandidateNamesRejected <- character()
    hecateCandidatesRejectedText <- ""
    if (isTRUE(includeHecateCandidates)) {
      hecateDetails <- ithGetHecateCandidateDetails(
        target,
        limit = topCandidates,
        llmClient = if (isTRUE(useLlmAdjudication)) llmClient else NULL
      )
      hecateSearchTerm <- hecateDetails$searchTerm
      hecateCandidateNames <- hecateDetails$candidates
      hecateCandidateNamesRaw <- hecateDetails$rawCandidates
      hecateCandidateNamesRejected <- hecateDetails$rejectedCandidates
      hecateCandidatesText <- paste(hecateCandidateNames, collapse = " | ")
      hecateCandidatesRawText <- paste(hecateCandidateNamesRaw, collapse = " | ")
      hecateCandidatesRejectedText <- paste(hecateCandidateNamesRejected, collapse = " | ")
    }

    fuzzyVariants <- unique(c(
      ithGenerateFuzzyNameVariants(targetNorm),
      ithGenerateFuzzyNameVariants(targetCore),
      targetCore
    ))

    if (length(hecateCandidateNames) > 0) {
      hecateNormalized <- ithNormalizeConceptSetName(hecateCandidateNames)
      hecateCore <- ithNormalizeConceptSetCoreName(hecateCandidateNames)
      fuzzyVariants <- unique(c(
        fuzzyVariants,
        hecateNormalized[nzchar(hecateNormalized)],
        hecateCore[nzchar(hecateCore)]
      ))
    }

    fuzzyVariantsText <- paste(fuzzyVariants, collapse = " | ")

    fuzzyNamesTriedText <- ithBuildFuzzyNamesTriedText(fuzzyVariantsText, hecateCandidatesText, includeHecateCandidates)

    exactIdx <- which(libraryCore == targetCore & compatibleFamily)

    if (length(exactIdx) > 0) {
      matchedName <- libraryNames[[exactIdx[[1]]]]
      results <- rbind(
        results,
        data.frame(
          csName = target,
          libCsName = matchedName,
          coSim = 1,
          stringsAsFactors = FALSE
        )
      )

      diagnostics <- rbind(
        diagnostics,
        data.frame(
          csName = target,
          csNameNormalized = targetNorm,
          matchMethod = "exact",
          matchedLibCsName = matchedName,
          matchedCoSim = 1,
          matchedEmbeddingSim = 1,
          matchedFuzzySim = 1,
          matchedCombinedSim = 1,
          bestCandidate = matchedName,
          bestCandidateCoSim = 1,
          bestCandidateEmbeddingSim = 1,
          bestCandidateFuzzySim = 1,
          bestCandidateCombinedSim = 1,
          fuzzyNamesTried = fuzzyNamesTriedText,
          fuzzyNameVariantsTried = fuzzyVariantsText,
          topCandidates = matchedName,
          libraryTopCandidates = matchedName,
          hecateSearchTerm = hecateSearchTerm,
          hecateTopCandidates = hecateCandidatesText,
          hecateTopCandidatesRaw = hecateCandidatesRawText,
          hecateTopCandidatesAccepted = hecateCandidatesText,
          hecateTopCandidatesRejected = hecateCandidatesRejectedText,
          similarityThreshold = similarityThreshold,
          fuzzyThreshold = fuzzyThreshold,
          note = "Matched by normalized exact name",
          stringsAsFactors = FALSE
        )
      )
      next
    }

    if (!hasEmbeddings) {
      diagnostics <- rbind(
        diagnostics,
        data.frame(
          csName = target,
          csNameNormalized = targetNorm,
          matchMethod = "none",
          matchedLibCsName = "",
          matchedCoSim = NA_real_,
          matchedEmbeddingSim = NA_real_,
          matchedFuzzySim = NA_real_,
          matchedCombinedSim = NA_real_,
          bestCandidate = "",
          bestCandidateCoSim = NA_real_,
          bestCandidateEmbeddingSim = NA_real_,
          bestCandidateFuzzySim = NA_real_,
          bestCandidateCombinedSim = NA_real_,
          fuzzyNamesTried = fuzzyNamesTriedText,
          fuzzyNameVariantsTried = fuzzyVariantsText,
          topCandidates = "",
          libraryTopCandidates = "",
          hecateSearchTerm = hecateSearchTerm,
          hecateTopCandidates = hecateCandidatesText,
          hecateTopCandidatesRaw = hecateCandidatesRawText,
          hecateTopCandidatesAccepted = hecateCandidatesText,
          hecateTopCandidatesRejected = hecateCandidatesRejectedText,
          similarityThreshold = similarityThreshold,
          fuzzyThreshold = fuzzyThreshold,
          note = "Library has no embed column",
          stringsAsFactors = FALSE
        )
      )
      next
    }

    targetVec <- tryCatch(
      ithGetEmbeddingVectorsInternal(targetCore, reportTime = FALSE, silent = TRUE)[[1]],
      error = function(e) {
        ithMaybeMessage(
          verbose,
          "Embedding generation failed for target concept set '",
          target,
          "'. Error: ",
          e$message,
          ". Skipping embedding-based matching for this target."
        )
        NULL
      }
    )

    if (is.null(targetVec) || !is.numeric(targetVec) || length(targetVec) <= 1 || any(!is.finite(targetVec))) {
      diagnostics <- rbind(
        diagnostics,
        data.frame(
          csName = target,
          csNameNormalized = targetNorm,
          matchMethod = "none",
          matchedLibCsName = "",
          matchedCoSim = NA_real_,
          matchedEmbeddingSim = NA_real_,
          matchedFuzzySim = NA_real_,
          matchedCombinedSim = NA_real_,
          bestCandidate = "",
          bestCandidateCoSim = NA_real_,
          bestCandidateEmbeddingSim = NA_real_,
          bestCandidateFuzzySim = NA_real_,
          bestCandidateCombinedSim = NA_real_,
          fuzzyNamesTried = fuzzyNamesTriedText,
          fuzzyNameVariantsTried = fuzzyVariantsText,
          topCandidates = "",
          libraryTopCandidates = "",
          hecateSearchTerm = hecateSearchTerm,
          hecateTopCandidates = hecateCandidatesText,
          hecateTopCandidatesRaw = hecateCandidatesRawText,
          hecateTopCandidatesAccepted = hecateCandidatesText,
          hecateTopCandidatesRejected = hecateCandidatesRejectedText,
          similarityThreshold = similarityThreshold,
          fuzzyThreshold = fuzzyThreshold,
          note = "Target embedding unavailable",
          stringsAsFactors = FALSE
        )
      )
      next
    }

    embeddingSimilarity <- rep(NA_real_, nrow(conceptSetNamesLibrary))
    combinedSimilarity <- rep(NA_real_, nrow(conceptSetNamesLibrary))

    fuzzyMatrix <- vapply(
      fuzzyVariants,
      function(v) vapply(libraryCore, function(candidate) ithComputeFuzzyNameSimilarity(v, candidate), numeric(1)),
      numeric(nrow(conceptSetNamesLibrary))
    )
    if (is.vector(fuzzyMatrix)) {
      fuzzyMatrix <- matrix(fuzzyMatrix, nrow = nrow(conceptSetNamesLibrary))
    }

    fuzzySimilarity <- apply(fuzzyMatrix, 1, max)

    rankScore <- rep(0, nrow(conceptSetNamesLibrary))
    avgRankAccumulator <- rep(0, nrow(conceptSetNamesLibrary))
    for (variantUp in seq_len(ncol(fuzzyMatrix))) {
      rankVec <- rank(-fuzzyMatrix[, variantUp], ties.method = "first")
      scoreVec <- nrow(conceptSetNamesLibrary) - rankVec + 1
      rankScore <- rankScore + scoreVec
      avgRankAccumulator <- avgRankAccumulator + rankVec
    }
    fuzzyRankScore <- rankScore / max(rankScore)

    for (libUp in seq_len(nrow(conceptSetNamesLibrary))) {
      if (!compatibleFamily[[libUp]]) {
        embeddingSimilarity[[libUp]] <- NA_real_
        combinedSimilarity[[libUp]] <- NA_real_
        next
      }

      candidateEmbed <- conceptSetNamesLibrary$embed[[libUp]]
      candidateEmbedValid <- is.numeric(candidateEmbed) &&
        length(candidateEmbed) > 1 &&
        length(candidateEmbed) == length(targetVec) &&
        all(is.finite(candidateEmbed))

      if (candidateEmbedValid) {
        embeddingSimilarity[[libUp]] <- Helios::computeSimilarity(targetVec, candidateEmbed)
      }

      if (!is.na(embeddingSimilarity[[libUp]])) {
        combinedSimilarity[[libUp]] <- mean(c(embeddingSimilarity[[libUp]], fuzzySimilarity[[libUp]], fuzzyRankScore[[libUp]]))
      } else {
        combinedSimilarity[[libUp]] <- mean(c(fuzzySimilarity[[libUp]], fuzzyRankScore[[libUp]]))
      }
    }

    valid <- which(!is.na(combinedSimilarity))
    if (length(valid) == 0) {
      familyNote <- if (identical(targetFamily, "unknown")) {
        "All library embeddings are NULL/invalid"
      } else {
        "No compatible concept set family candidates after filtering"
      }

      diagnostics <- rbind(
        diagnostics,
        data.frame(
          csName = target,
          csNameNormalized = targetNorm,
          matchMethod = "none",
          matchedLibCsName = "",
          matchedCoSim = NA_real_,
          matchedEmbeddingSim = NA_real_,
          matchedFuzzySim = NA_real_,
          matchedCombinedSim = NA_real_,
          bestCandidate = "",
          bestCandidateCoSim = NA_real_,
          bestCandidateEmbeddingSim = NA_real_,
          bestCandidateFuzzySim = NA_real_,
          bestCandidateCombinedSim = NA_real_,
          fuzzyNamesTried = fuzzyNamesTriedText,
          fuzzyNameVariantsTried = fuzzyVariantsText,
          topCandidates = "",
          libraryTopCandidates = "",
          hecateSearchTerm = hecateSearchTerm,
          hecateTopCandidates = hecateCandidatesText,
          hecateTopCandidatesRaw = hecateCandidatesRawText,
          hecateTopCandidatesAccepted = hecateCandidatesText,
          hecateTopCandidatesRejected = hecateCandidatesRejectedText,
          similarityThreshold = similarityThreshold,
          fuzzyThreshold = fuzzyThreshold,
          note = familyNote,
          stringsAsFactors = FALSE
        )
      )
      next
    }

    ranked <- valid[order(combinedSimilarity[valid], fuzzyRankScore[valid], decreasing = TRUE)]
    topIdx <- ranked[seq_len(min(length(ranked), topCandidates))]
    topSummary <- paste0(
      libraryNames[topIdx],
      ":emb=", sprintf("%.4f", embeddingSimilarity[topIdx]),
      ",fuz=", sprintf("%.4f", fuzzySimilarity[topIdx]),
      ",rnk=", sprintf("%.4f", fuzzyRankScore[topIdx]),
      ",cmb=", sprintf("%.4f", combinedSimilarity[topIdx]),
      collapse = " | "
    )

    bestIdx <- ranked[[1]]
    bestName <- libraryNames[[bestIdx]]
    bestEmbed <- embeddingSimilarity[[bestIdx]]
    bestFuzzy <- fuzzySimilarity[[bestIdx]]
    bestCombined <- combinedSimilarity[[bestIdx]]
    bestSim <- if (!is.na(bestEmbed)) bestEmbed else bestCombined
    matchedName <- ""
    matchedSim <- NA_real_
    matchedEmbed <- NA_real_
    matchedFuzzy <- NA_real_
    matchedCombined <- NA_real_
    matchMethod <- "none"
    note <- "Best candidate below embedding/fuzzy thresholds"

    llmAudit <- ""

    ambiguousLower <- if (!is.null(llmAmbiguityMinScore) && !is.na(suppressWarnings(as.numeric(llmAmbiguityMinScore[[1]])))) {
      as.numeric(llmAmbiguityMinScore[[1]])
    } else {
      max(0, similarityThreshold - 0.03)
    }

    ambiguousUpper <- if (!is.null(llmAmbiguityMaxScore) && !is.na(suppressWarnings(as.numeric(llmAmbiguityMaxScore[[1]])))) {
      as.numeric(llmAmbiguityMaxScore[[1]])
    } else {
      min(1, similarityThreshold + 0.03)
    }

    embeddingRanked <- valid[order(embeddingSimilarity[valid], combinedSimilarity[valid], decreasing = TRUE, na.last = NA)]
    if (length(embeddingRanked) == 0) {
      embeddingRanked <- ranked
    }

    secondEmbed <- if (length(embeddingRanked) > 1) embeddingSimilarity[[embeddingRanked[[2]]]] else NA_real_
    top2Gap <- if (!is.na(bestEmbed) && !is.na(secondEmbed)) (bestEmbed - secondEmbed) else NA_real_

    ambiguousByScore <- !is.na(bestEmbed) && bestEmbed >= ambiguousLower && bestEmbed <= ambiguousUpper
    ambiguousByGap <- !is.na(top2Gap) && top2Gap <= llmTop2GapThreshold
    shouldAdjudicate <- isTRUE(useLlmAdjudication) && (ambiguousByScore || ambiguousByGap)

    embeddingPass <- !is.na(bestEmbed) && bestEmbed >= similarityThreshold
    fuzzyPass <- !is.na(bestFuzzy) && bestFuzzy >= fuzzyThreshold

    if (embeddingPass || fuzzyPass) {
      matchedName <- bestName
      matchedEmbed <- bestEmbed
      matchedFuzzy <- bestFuzzy
      matchedCombined <- bestCombined

      if (embeddingPass && fuzzyPass) {
        matchedSim <- bestCombined
        matchMethod <- "embedding+fuzzy"
        note <- "Matched by both embedding and fuzzy similarity"
      } else if (embeddingPass) {
        matchedSim <- bestEmbed
        matchMethod <- "embedding"
        note <- "Matched by embedding similarity"
      } else {
        matchedSim <- bestFuzzy
        matchMethod <- "fuzzy"
        note <- "Matched by fuzzy name similarity"
      }
    }

    if (isTRUE(shouldAdjudicate)) {
      adjudicationIdx <- embeddingRanked[seq_len(min(length(embeddingRanked), llmTopCandidates))]
      adjudicationTable <- data.frame(
        conceptSetName = as.character(libraryNames[adjudicationIdx]),
        embeddingSimilarity = as.numeric(embeddingSimilarity[adjudicationIdx]),
        fuzzySimilarity = as.numeric(fuzzySimilarity[adjudicationIdx]),
        fuzzyRankScore = as.numeric(fuzzyRankScore[adjudicationIdx]),
        combinedSimilarity = as.numeric(combinedSimilarity[adjudicationIdx]),
        stringsAsFactors = FALSE
      )

      adjudicationResult <- ithAdjudicateConceptSetMatch(
        targetName = target,
        conditionName = conditionName,
        candidateTable = adjudicationTable,
        similarityThreshold = similarityThreshold,
        fuzzyThreshold = fuzzyThreshold,
        top2Gap = top2Gap,
        llmClient = llmClient,
        verbose = verbose
      )

      if (!isTRUE(adjudicationResult$available)) {
        llmAudit <- paste0(
          "LLM adjudication fallback; reason=", adjudicationResult$decision,
          ifelse(nzchar(adjudicationResult$error), paste0(" ; error=", adjudicationResult$error), "")
        )
      } else {
        selectedName <- as.character(adjudicationResult$selectedConceptSetName)
        selectedNorm <- tolower(trimws(selectedName))
        selectedIdx <- which(tolower(libraryNames) == selectedNorm)
        selectedInPool <- length(selectedIdx) > 0 && any(selectedIdx %in% adjudicationIdx)
        llmConf <- adjudicationResult$confidence
        llmConfValid <- !is.na(llmConf) && llmConf >= llmConfidenceThreshold

        llmAudit <- paste0(
          "LLM adjudication decision=", adjudicationResult$decision,
          " ; selected=", selectedName,
          " ; confidence=", ifelse(is.na(llmConf), "NA", sprintf("%.4f", llmConf)),
          " ; rationale=", gsub("[\r\n]+", " ", adjudicationResult$rationale)
        )

        if (selectedNorm %in% c("no_match", "no-match", "none", "")) {
          if (llmConfValid) {
            matchedName <- ""
            matchedSim <- NA_real_
            matchedEmbed <- NA_real_
            matchedFuzzy <- NA_real_
            matchedCombined <- NA_real_
            matchMethod <- "none"
            note <- paste0("LLM adjudication rejected all candidates. ", llmAudit)
          } else {
            note <- paste0(note, " ; ", llmAudit, " ; below confidence threshold, fallback to similarity")
          }
        } else if (selectedInPool && llmConfValid) {
          chosenIdx <- selectedIdx[[1]]
          matchedName <- libraryNames[[chosenIdx]]
          matchedEmbed <- embeddingSimilarity[[chosenIdx]]
          matchedFuzzy <- fuzzySimilarity[[chosenIdx]]
          matchedCombined <- combinedSimilarity[[chosenIdx]]
          matchedSim <- if (!is.na(matchedEmbed)) matchedEmbed else matchedCombined
          matchMethod <- "llmAdjudicated"
          note <- paste0("Matched by LLM adjudication. ", llmAudit)
        } else {
          note <- paste0(
            note,
            " ; ",
            llmAudit,
            " ; invalid candidate or below confidence threshold, fallback to similarity"
          )
        }
      }
    }

    if (nzchar(llmAudit) && !grepl("LLM adjudication", note, fixed = TRUE)) {
      note <- paste0(note, " ; ", llmAudit)
    }

    if (nzchar(matchedName)) {
      results <- rbind(
        results,
        data.frame(
          csName = target,
          libCsName = matchedName,
          coSim = ifelse(is.na(matchedSim), 0, matchedSim),
          stringsAsFactors = FALSE
        )
      )
    }

    diagnostics <- rbind(
      diagnostics,
      data.frame(
        csName = target,
        csNameNormalized = targetNorm,
        matchMethod = matchMethod,
        matchedLibCsName = matchedName,
        matchedCoSim = matchedSim,
        matchedEmbeddingSim = matchedEmbed,
        matchedFuzzySim = matchedFuzzy,
        matchedCombinedSim = matchedCombined,
        bestCandidate = bestName,
        bestCandidateCoSim = bestSim,
        bestCandidateEmbeddingSim = bestEmbed,
        bestCandidateFuzzySim = bestFuzzy,
        bestCandidateCombinedSim = bestCombined,
        fuzzyNamesTried = fuzzyNamesTriedText,
        fuzzyNameVariantsTried = fuzzyVariantsText,
        topCandidates = topSummary,
        libraryTopCandidates = topSummary,
        hecateSearchTerm = hecateSearchTerm,
        hecateTopCandidates = hecateCandidatesText,
        hecateTopCandidatesRaw = hecateCandidatesRawText,
        hecateTopCandidatesAccepted = hecateCandidatesText,
        hecateTopCandidatesRejected = hecateCandidatesRejectedText,
        similarityThreshold = similarityThreshold,
        fuzzyThreshold = fuzzyThreshold,
        note = note,
        stringsAsFactors = FALSE
      )
    )
  }

  if (nrow(results) > 0) {
    results <- dplyr::group_by(results, csName) |>
      dplyr::slice_max(coSim, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  if (isTRUE(returnDiagnostics)) {
    return(list(matches = results, diagnostics = diagnostics))
  }

  results
}

ithCreateRubricConceptSets <- function(conditionSpecs,
                                           outputDirectory,
                                           conceptLibraryDirectory = outputDirectory,
                                           database,
                                           llmClientReasoning = ithGetLlmClientFromConfig(list(), "reasoning"),
                                           llmClientNonReasoning = ithGetLlmClientFromConfig(list(), "nonReasoning"),
                                           conceptLibraryFile = "rubricConceptSetLibrary.duckdb",
                                           similarityThreshold = 0.94,
                                           fuzzyThreshold = 0.94,
                                           useLlmAdjudication = TRUE,
                                           llmTopCandidates = 25,
                                           llmAmbiguityMinScore = NULL,
                                           llmAmbiguityMaxScore = NULL,
                                           llmTop2GapThreshold = 0.015,
                                           llmConfidenceThreshold = 0.60,
                                           runId = NULL,
                                           conceptSetWorkers = 1L,
                                           conceptSetFolder = NULL,
                                           llmConfig = list(),
                                           conceptSetBuilder = NULL,
                                           emptyConceptSet = "placeholder",
                                           emptyConceptSetFallback = TRUE,
                                           rebuildEmptyConceptSets = FALSE,
                                           verbose = TRUE) {
  ithMaybeMessage(verbose, "Starting concept set generation for ", length(conditionSpecs), " condition(s).")

  emptyConceptSet <- match.arg(as.character(emptyConceptSet[[1]]), c("placeholder", "stop"))
  emptyConceptSetFallback <- isTRUE(emptyConceptSetFallback)
  rebuildEmptyConceptSets <- isTRUE(rebuildEmptyConceptSets)

  conceptSetWorkers <- max(1L, as.integer(ithCoalesce(conceptSetWorkers, 1L)[[1]]))
  if (is.null(conceptSetFolder) || length(conceptSetFolder) == 0L || !nzchar(as.character(conceptSetFolder[[1]]))) {
    conceptSetFolder <- if (conceptSetWorkers > 1L) file.path(outputDirectory, "conceptSetBuild") else NULL
  } else {
    conceptSetFolder <- as.character(conceptSetFolder[[1]])
  }
  # Directory for one concept set's builder files: the shared concept set folder when configured,
  # otherwise the condition folder.
  conceptSetBuildDirectory <- function(conditionDir, conceptSetName) {
    file.path(if (is.null(conceptSetFolder)) conditionDir else conceptSetFolder, conceptSetName)
  }

  conceptLibrarySourcePath <- file.path(
    conceptLibraryDirectory,
    ithCanonicalConceptLibraryFileName(conceptLibraryFile)
  )

  if (!file.exists(conceptLibrarySourcePath)) {
    ithDuckdbInitializeLibrary(
      libraryPath = conceptLibrarySourcePath,
      verbose = verbose
    )
  }

  ithMaybeMessage(verbose, "Loading concept set library from: ", conceptLibrarySourcePath)
  csLibrary <- ithDuckdbGetConceptSetLibrary(
    libraryPath = conceptLibrarySourcePath,
    libraryVersion = ithDuckdbGetLatestLibraryVersion(conceptLibrarySourcePath)
  )

  # Known-empty records are dropped from the in-memory library so they match as missing and are
  # rebuilt; the stored library is only changed if the rebuild produces something.
  if (rebuildEmptyConceptSets && nrow(csLibrary[[1]]) > 0L) {
    emptyRows <- vapply(seq_len(nrow(csLibrary[[1]])), function(i) ithIsKnownEmptyConceptSet(csLibrary[[1]]$conceptSet[[i]]), logical(1))
    if (any(emptyRows)) {
      emptyLibraryNames <- tolower(as.character(csLibrary[[1]]$conceptSetName[emptyRows]))
      ithMaybeMessage(verbose, "Rebuilding ", length(emptyLibraryNames), " known-empty concept set(s): ", paste(emptyLibraryNames, collapse = ", "))
      csLibrary[[1]] <- csLibrary[[1]][!emptyRows, , drop = FALSE]
      if (nrow(csLibrary[[2]]) > 0L) {
        csLibrary[[2]] <- csLibrary[[2]][!(tolower(as.character(csLibrary[[2]]$conceptSetName)) %in% emptyLibraryNames), , drop = FALSE]
      }
    }
  }

  if (is.null(runId) || !nzchar(as.character(runId[[1]]))) {
    runId <- paste0("createRubricConceptSets_", format(Sys.time(), "%Y%m%d%H%M%S"))
  } else {
    runId <- as.character(runId[[1]])
  }

  currentUser <- Sys.info()[["user"]]

  upsertConceptSetInLibrary <- function(csLibraryName,
                                        conceptSetName,
                                        cleanName,
                                        finalConceptSet,
                                        conditionName,
                                        cohortId,
                                        sourceFile = NA_character_) {
    conceptSetNameNorm <- tolower(as.character(conceptSetName))
    existingEmbed <- NULL

    if (nrow(csLibraryName[[2]]) > 0) {
      existingEmbedIdx <- which(tolower(as.character(csLibraryName[[2]]$conceptSetName)) == conceptSetNameNorm)
      if (length(existingEmbedIdx) > 0) {
        existingEmbed <- csLibraryName[[2]]$embed[[existingEmbedIdx[[1]]]]
      }
    }

    if (nrow(csLibraryName[[1]]) > 0) {
      keepIdx1 <- tolower(as.character(csLibraryName[[1]]$conceptSetName)) != conceptSetNameNorm
      csLibraryName[[1]] <- csLibraryName[[1]][keepIdx1, , drop = FALSE]
    }

    if (nrow(csLibraryName[[2]]) > 0) {
      keepIdx2 <- tolower(as.character(csLibraryName[[2]]$conceptSetName)) != conceptSetNameNorm
      csLibraryName[[2]] <- csLibraryName[[2]][keepIdx2, , drop = FALSE]
    }

    embeddingVec <- tryCatch(
      ithGetEmbeddingVectorsInternal(tolower(as.character(cleanName)), reportTime = FALSE, silent = TRUE)[[1]],
      error = function(e) {
        ithMaybeMessage(
          verbose,
          "Embedding generation failed for concept set '",
          conceptSetName,
          "'. Error: ",
          e$message,
          ". Reusing prior embedding or placeholder."
        )
        NULL
      }
    )

    if (is.null(embeddingVec) || !is.numeric(embeddingVec) || length(embeddingVec) <= 1 || any(!is.finite(embeddingVec))) {
      if (!is.null(existingEmbed) && is.numeric(existingEmbed) && length(existingEmbed) > 1 && all(is.finite(existingEmbed))) {
        embeddingVec <- existingEmbed
      } else {
        embeddingVec <- NA_real_
      }
    }

    embedded <- data.frame(
      conceptSetName = as.character(conceptSetName),
      cleanName = as.character(cleanName),
      embed = I(list(embeddingVec)),
      stringsAsFactors = FALSE
    )

    finalDf <- data.frame(
      conceptSetName = as.character(conceptSetName),
      cleanName = as.character(cleanName),
      conceptSet = I(list(finalConceptSet)),
      stringsAsFactors = FALSE
    )

    csLibraryName[[2]] <- rbind(csLibraryName[[2]], embedded)
    csLibraryName[[1]] <- rbind(csLibraryName[[1]], finalDf)

    ithDuckdbUpsertConceptSet(
      libraryPath = conceptLibrarySourcePath,
      conceptSetName = as.character(conceptSetName),
      cleanName = as.character(cleanName),
      conceptSet = finalConceptSet,
      embedding = embeddingVec,
      runId = runId,
      userName = currentUser,
      conditionName = conditionName,
      cohortId = cohortId,
      sourceFile = sourceFile
    )

    csLibraryName
  }

  hasReadableConceptSet <- function(csLibraryName, requestedName, matchedName) {
    conceptSetTable <- csLibraryName[[1]]
    if (is.null(conceptSetTable) || nrow(conceptSetTable) == 0 || !all(c("conceptSetName", "cleanName", "conceptSet") %in% names(conceptSetTable))) {
      return(FALSE)
    }

    lookupKeys <- ithBuildConceptSetLookupKeys(matchedName, requestedName)
    conceptSetNameNormalized <- ithNormalizeConceptSetName(as.character(conceptSetTable$conceptSetName))
    cleanNameNormalized <- ithNormalizeConceptSetName(as.character(conceptSetTable$cleanName))
    candidateIdx <- unique(c(
      which(conceptSetNameNormalized %in% lookupKeys),
      which(cleanNameNormalized %in% lookupKeys)
    ))

    if (length(candidateIdx) == 0) {
      return(FALSE)
    }

    for (candidateUp in candidateIdx) {
      prepared <- ithPrepareConceptSetForResolution(conceptSetTable$conceptSet[[candidateUp]])
      if (is.list(prepared) && !is.null(prepared$items) && length(prepared$items) > 0) {
        return(TRUE)
      }
    }

    FALSE
  }

  # One build attempt: list(conceptSet, isPlaceholder, message, outcome).
  buildOne <- function(conceptSetName, csDirectory) {
    conceptSetTarget <- gsub("_", " ", conceptSetName)
    if (!is.null(conceptSetBuilder)) {
      built <- tryCatch(
        conceptSetBuilder(conceptSetTarget = conceptSetTarget, csDirectory = csDirectory),
        error = function(e) list(conceptSet = ithPlaceholderConceptSet(), isPlaceholder = TRUE, message = conditionMessage(e), outcome = "error")
      )
      if (is.null(built$outcome)) {
        built$outcome <- if (isTRUE(built$isPlaceholder)) "error" else "ok"
      }
      return(built)
    }
    ithCreateConceptSetSafe(
      conceptSetTarget = conceptSetTarget,
      csDirectory = csDirectory,
      database = database,
      llmClientReasoning = llmClientReasoning,
      llmClientNonReasoning = llmClientNonReasoning,
      clinicalDefinition = "",
      bucketSize = 100,
      verbose = verbose
    )
  }

  aliasMap <- list()
  buildSummary <- list()
  stoppedEmptyNames <- character()
  addSummary <- function(...) {
    buildSummary[[length(buildSummary) + 1L]] <<- ithConceptSetSummaryRow(...)
  }

  recordKnownEmpty <- function(nameUp, outcome, reason, attempts, conditionDir, conditionName, cohortId) {
    csLibrary <<- upsertConceptSetInLibrary(
      csLibraryName = csLibrary,
      conceptSetName = nameUp,
      cleanName = gsub("_", " ", nameUp),
      finalConceptSet = ithPlaceholderConceptSet(),
      conditionName = conditionName,
      cohortId = cohortId,
      sourceFile = paste0("knownEmpty: ", outcome, ": ", substr(reason, 1, 200))
    )
    recordPath <- tryCatch(
      ithWriteKnownEmptyRecord(conceptSetBuildDirectory(conditionDir, nameUp), nameUp, outcome, reason, attempts, runId),
      error = function(e) NA_character_
    )
    ithMaybeMessage(verbose, "Concept set '", nameUp, "' is empty (", outcome, "); recorded as known empty.")
    invisible(recordPath)
  }

  # Handles a build that did not produce a usable concept set. Only empty/unreadable results are
  # recoverable: one retry for an unreadable result, then a simplified-name alias, then a
  # known-empty record. A raised error is never stored, so a rerun builds the set again.
  # Returns list(status, outcome, reason, aliasTarget); status is built, aliased, knownEmpty or error.
  resolveFailedBuild <- function(nameUp, outcome, message, conditionDir, conditionName, cohortId) {
    attempts <- character()
    if (identical(outcome, "unreadable")) {
      attempts <- c(attempts, paste0("retry ", nameUp))
      retry <- buildOne(nameUp, conceptSetBuildDirectory(conditionDir, nameUp))
      if (identical(retry$outcome, "ok")) {
        csLibrary <<- upsertConceptSetInLibrary(
          csLibraryName = csLibrary, conceptSetName = nameUp, cleanName = gsub("_", " ", nameUp),
          finalConceptSet = retry$conceptSet, conditionName = conditionName, cohortId = cohortId,
          sourceFile = conceptSetBuildDirectory(conditionDir, nameUp)
        )
        return(list(status = "built", outcome = "ok", reason = "", aliasTarget = ""))
      }
      outcome <- retry$outcome
      message <- retry$message
    }
    if (!(outcome %in% c("empty", "unreadable"))) {
      return(list(status = "error", outcome = outcome, reason = message, aliasTarget = ""))
    }
    if (identical(emptyConceptSet, "stop")) {
      stoppedEmptyNames <<- c(stoppedEmptyNames, nameUp)
      return(list(status = "error", outcome = outcome, reason = message, aliasTarget = ""))
    }

    if (emptyConceptSetFallback) {
      for (candidate in ithSimplifiedConceptSetNames(nameUp)) {
        candidateState <- ithLibraryConceptSetState(csLibrary[[1]], candidate)
        attempts <- c(attempts, candidate)
        if (identical(candidateState, "readable")) {
          aliasMap[[nameUp]] <<- candidate
          return(list(status = "aliased", outcome = outcome, reason = message, aliasTarget = candidate))
        }
        if (identical(candidateState, "missing")) {
          candidateDir <- conceptSetBuildDirectory(conditionDir, candidate)
          built <- buildOne(candidate, candidateDir)
          if (identical(built$outcome, "ok")) {
            csLibrary <<- upsertConceptSetInLibrary(
              csLibraryName = csLibrary, conceptSetName = candidate, cleanName = gsub("_", " ", candidate),
              finalConceptSet = built$conceptSet, conditionName = conditionName, cohortId = cohortId,
              sourceFile = candidateDir
            )
            aliasMap[[nameUp]] <<- candidate
            return(list(status = "aliased", outcome = outcome, reason = message, aliasTarget = candidate))
          }
          break
        }
      }
    }

    recordKnownEmpty(nameUp, outcome, message, attempts, conditionDir, conditionName, cohortId)
    list(status = "knownEmpty", outcome = outcome, reason = message, aliasTarget = "")
  }

  startCondition <- function(conditionUp) {
    condition <- conditionSpecs[[conditionUp]]
    conditionRandomRaw <- condition$randomCohortDefinitionId
    conditionRandomCohortDefinitionId <- suppressWarnings(as.integer(
      if (is.null(conditionRandomRaw) || length(conditionRandomRaw) == 0) NA else conditionRandomRaw[[1]]
    ))
    if (length(conditionRandomCohortDefinitionId) == 0 || is.na(conditionRandomCohortDefinitionId)) {
      conditionRandomCohortDefinitionId <- NA_integer_
    }
    ithMaybeMessage(verbose, "Resolving concept sets for condition: ", condition$testCondition)
    conditionDir <- file.path(outputDirectory, condition$testCondition)
    rubricObject <- ithLoadLatest(conditionDir, "rubricList.rds")
    if (is.null(rubricObject)) {
      stop("No rubricList found for condition: ", condition$testCondition)
    }

    neededNames <- ithExtractNeededConceptSetNames(rubricObject$neededConcepts)
    # Older rubrics may contain malformed names (e.g. "p pulmonology_provider_codes"); creating a
    # concept set for those would add junk to the library.
    nameCheck <- ithCheckRuleConceptSets(paste(neededNames, collapse = ","), neededNames, requireControlledSuffix = FALSE)
    if (length(nameCheck$problems) > 0L) {
      stop(
        "Invalid concept-set names in the rubric for '", condition$testCondition, "': ",
        paste(nameCheck$problems, collapse = "; "),
        ". Fix them in the concise rubric CSV and run applyConciseRubricEdits, then rerun concept-set creation.",
        call. = FALSE
      )
    }
    if (length(nameCheck$repairs) > 0L) {
      message(
        "Repaired concept-set name(s) for '", condition$testCondition, "': ", paste(nameCheck$repairs, collapse = "; "),
        ". Run applyConciseRubricEdits to save the repair in the rubric."
      )
      neededNames <- unique(tolower(ithParseConceptSets(nameCheck$value)))
    }
    # The fixed AE1 concept set is built from Hecate and added to the library before matching, so
    # matching finds it by exact name.
    ae1Name <- tolower(ithAe1ConceptSetName(condition$testCondition))
    if (ae1Name %in% neededNames) {
      ae1Result <- tryCatch(
        ithEnsureAe1ConceptSetInLibrary(
          conditionName = condition$testCondition,
          libraryPath = conceptLibrarySourcePath,
          llmClient = llmClientReasoning,
          additionalInformation = ithCoalesce(condition$additionalInformation, ""),
          cohortId = conditionRandomCohortDefinitionId,
          runId = runId,
          artifactDirectory = conceptSetBuildDirectory(conditionDir, ae1Name),
          verbose = verbose
        ),
        error = function(e) {
          warning(
            condition$testCondition, ": could not build ", ae1Name, " from Hecate (", conditionMessage(e),
            "); falling back to standard concept set creation.",
            call. = FALSE, immediate. = TRUE
          )
          NULL
        }
      )
      if (!is.null(ae1Result) && identical(ae1Result$status, "created")) {
        csLibrary <<- ithDuckdbGetConceptSetLibrary(
          libraryPath = conceptLibrarySourcePath,
          libraryVersion = ithDuckdbGetLatestLibraryVersion(conceptLibrarySourcePath)
        )
      }
    }
    # Hecate/LLM matching varies between runs, so a match saved earlier is kept while its library set
    # is still readable and only the remaining names are matched again.
    matchWithReuse <- function() {
      previous <- ithLoadLatest(conditionDir, "matchedConceptSets.rds")
      reused <- NULL
      if (is.data.frame(previous) && all(c("csName", "libCsName") %in% names(previous))) {
        previous <- previous[tolower(as.character(previous$csName)) %in% neededNames & !duplicated(tolower(as.character(previous$csName))), , drop = FALSE]
        keep <- vapply(
          seq_len(nrow(previous)),
          function(i) hasReadableConceptSet(csLibrary, as.character(previous$csName[[i]]), as.character(previous$libCsName[[i]])),
          logical(1)
        )
        reused <- previous[keep, , drop = FALSE]
      }
      toMatch <- setdiff(neededNames, if (is.null(reused)) character() else tolower(as.character(reused$csName)))
      if (!is.null(reused) && nrow(reused) > 0L) {
        ithMaybeMessage(verbose, "Reusing ", nrow(reused), " previously matched concept set(s); matching ", length(toMatch), " other(s).")
      }
      fresh <- if (length(toMatch) > 0L) {
        ithMatchConceptSets(
          toMatch,
          csLibrary[[2]],
          similarityThreshold = similarityThreshold,
          fuzzyThreshold = fuzzyThreshold,
          returnDiagnostics = TRUE,
          topCandidates = 5,
          includeHecateCandidates = TRUE,
          useLlmAdjudication = useLlmAdjudication,
          llmClient = llmClientNonReasoning,
          llmTopCandidates = llmTopCandidates,
          llmAmbiguityMinScore = llmAmbiguityMinScore,
          llmAmbiguityMaxScore = llmAmbiguityMaxScore,
          llmTop2GapThreshold = llmTop2GapThreshold,
          llmConfidenceThreshold = llmConfidenceThreshold,
          conditionName = condition$testCondition,
          verbose = verbose
        )
      } else {
        list(matches = NULL, diagnostics = NULL)
      }
      parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, list(reused, fresh$matches))
      list(
        matches = if (length(parts) > 0L) ithBindRows(parts) else fresh$matches,
        diagnostics = fresh$diagnostics
      )
    }
    matchStart <- matchWithReuse()
    matches <- matchStart$matches
    diagnosticsStart <- matchStart$diagnostics

    if (!is.null(diagnosticsStart) && nrow(diagnosticsStart) > 0) {
      diagnosticsStart$phase <- "beforeCreateMissing"
      diagnosticsStart$conditionName <- condition$testCondition
      diagnosticsStartPath <- ithSaveVersioned(
        saveDirectory = conditionDir,
        objectToSave = diagnosticsStart,
        fileName = "conceptSetMatchDiagnostics.csv"
      )

      nExact <- sum(diagnosticsStart$matchMethod == "exact", na.rm = TRUE)
      nEmbedding <- sum(diagnosticsStart$matchMethod == "embedding", na.rm = TRUE)
      nFuzzy <- sum(diagnosticsStart$matchMethod == "fuzzy", na.rm = TRUE)
      nHybrid <- sum(diagnosticsStart$matchMethod == "embedding+fuzzy", na.rm = TRUE)
      nNone <- sum(diagnosticsStart$matchMethod == "none", na.rm = TRUE)
      ithMaybeMessage(verbose, "Matching diagnostics (pre-create): exact=", nExact, ", embedding=", nEmbedding, ", fuzzy=", nFuzzy, ", embedding+fuzzy=", nHybrid, ", none=", nNone)
      ithMaybeMessage(verbose, "Saved concept set matching diagnostics: ", diagnosticsStartPath)
    }

    if (!is.null(matches) && nrow(matches) > 0) {
      regeneratedUnreadable <- character()

      for (matchUp in seq_len(nrow(matches))) {
        requestedName <- as.character(matches$csName[[matchUp]])
        matchedName <- as.character(matches$libCsName[[matchUp]])
        if (is.na(requestedName) || !nzchar(requestedName)) {
          next
        }
        if (is.na(matchedName) || !nzchar(matchedName)) {
          matchedName <- requestedName
        }

        if (hasReadableConceptSet(csLibrary, requestedName = requestedName, matchedName = matchedName)) {
          next
        }

        ithMaybeMessage(
          verbose,
          "Matched concept set '", matchedName,
          "' for '", requestedName,
          "' is unreadable. Regenerating concept set and replacing library JSON."
        )

        csDirectory <- conceptSetBuildDirectory(conditionDir, requestedName)
        rebuilt <- buildOne(requestedName, csDirectory)
        if (identical(rebuilt$outcome, "ok")) {
          csLibrary <<- upsertConceptSetInLibrary(
            csLibraryName = csLibrary,
            conceptSetName = matchedName,
            cleanName = gsub("_", " ", matchedName),
            finalConceptSet = rebuilt$conceptSet,
            conditionName = condition$testCondition,
            cohortId = conditionRandomCohortDefinitionId,
            sourceFile = csDirectory
          )
          regeneratedUnreadable <- c(regeneratedUnreadable, matchedName)
        } else if (rebuilt$outcome %in% c("empty", "unreadable") && identical(emptyConceptSet, "placeholder")) {
          recordKnownEmpty(
            matchedName, rebuilt$outcome, rebuilt$message, character(),
            conditionDir, condition$testCondition, conditionRandomCohortDefinitionId
          )
          regeneratedUnreadable <- c(regeneratedUnreadable, matchedName)
        } else {
          warning(
            condition$testCondition, ": matched concept set '", matchedName, "' is unreadable and could not be rebuilt (",
            rebuilt$message, ").",
            call. = FALSE, immediate. = TRUE
          )
        }
      }

      if (length(regeneratedUnreadable) > 0) {
        regeneratedUnreadable <- unique(regeneratedUnreadable)
        ithMaybeMessage(
          verbose,
          "Regenerated and replaced unreadable matched concept set(s): ",
          paste(regeneratedUnreadable, collapse = ", ")
        )

        matchStart <- matchWithReuse()
        matches <- matchStart$matches
        diagnosticsStart <- matchStart$diagnostics
      }
    }

    list(
      condition = condition,
      conditionDir = conditionDir,
      conditionRandomCohortDefinitionId = conditionRandomCohortDefinitionId,
      neededNames = neededNames,
      matches = matches,
      diagnosticsStart = diagnosticsStart
    )
  }

  missingNamesOf <- function(state) {
    setdiff(state$neededNames, unique(tolower(state$matches$csName)))
  }

  # Matches the names created for this condition, saves the match files, and warns about names
  # that are still unresolved. With createMissing = FALSE the missing sets must already be in the
  # library (the parallel path builds them first).
  finishCondition <- function(state, createMissing) {
    condition <- state$condition
    conditionDir <- state$conditionDir
    conditionRandomCohortDefinitionId <- state$conditionRandomCohortDefinitionId
    neededNames <- state$neededNames
    matches <- state$matches
    diagnosticsStart <- state$diagnosticsStart

    matchedNames <- unique(tolower(matches$csName))
    missingNames <- setdiff(neededNames, matchedNames)
    ithMaybeMessage(verbose, "Matched ", length(matchedNames), " concept set(s); creating ", length(missingNames), " missing concept set(s).")

    for (nameUp in if (createMissing) missingNames else character()) {
      if (!is.null(aliasMap[[nameUp]])) {
        next
      }
      ithMaybeMessage(verbose, "Creating concept set: ", nameUp)
      csDirectory <- conceptSetBuildDirectory(conditionDir, nameUp)
      startedAt <- Sys.time()
      built <- buildOne(nameUp, csDirectory)
      if (identical(built$outcome, "ok")) {
        csLibrary <<- upsertConceptSetInLibrary(
          csLibraryName = csLibrary,
          conceptSetName = nameUp,
          cleanName = gsub("_", " ", nameUp),
          finalConceptSet = built$conceptSet,
          conditionName = condition$testCondition,
          cohortId = conditionRandomCohortDefinitionId,
          sourceFile = csDirectory
        )
        addSummary(nameUp, "built", "ok", seconds = difftime(Sys.time(), startedAt, units = "secs"))
      } else {
        resolved <- resolveFailedBuild(
          nameUp, built$outcome, built$message, conditionDir,
          condition$testCondition, conditionRandomCohortDefinitionId
        )
        addSummary(
          nameUp, resolved$status, resolved$outcome, resolved$reason, resolved$aliasTarget,
          seconds = difftime(Sys.time(), startedAt, units = "secs")
        )
      }
    }

    aliasedNames <- intersect(missingNames, names(aliasMap))
    missingNames <- setdiff(missingNames, aliasedNames)

    # Only newly created names are matched again. Re-matching everything could drop names already
    # matched above, because Hecate/LLM results vary between calls, and those names were never created.
    matchFinal <- if (length(missingNames) > 0L) {
      ithMatchConceptSets(
        missingNames,
        csLibrary[[2]],
        similarityThreshold = similarityThreshold,
        fuzzyThreshold = fuzzyThreshold,
        returnDiagnostics = TRUE,
        topCandidates = 5,
        includeHecateCandidates = TRUE,
        useLlmAdjudication = useLlmAdjudication,
        llmClient = llmClientNonReasoning,
        llmTopCandidates = llmTopCandidates,
        llmAmbiguityMinScore = llmAmbiguityMinScore,
        llmAmbiguityMaxScore = llmAmbiguityMaxScore,
        llmTop2GapThreshold = llmTop2GapThreshold,
        llmConfidenceThreshold = llmConfidenceThreshold,
        conditionName = condition$testCondition,
        verbose = verbose
      )
    } else {
      list(matches = NULL, diagnostics = NULL)
    }
    keptDiagnostics <- diagnosticsStart
    if (!is.null(keptDiagnostics) && nrow(keptDiagnostics) > 0) {
      keptDiagnostics <- keptDiagnostics[!(tolower(as.character(keptDiagnostics$csName)) %in% c(missingNames, aliasedNames)), , drop = FALSE]
      keptDiagnostics$phase <- NULL
      keptDiagnostics$conditionName <- NULL
    }
    aliasRows <- NULL
    aliasDiagnostics <- NULL
    if (length(aliasedNames) > 0L) {
      aliasRows <- data.frame(
        csName = aliasedNames,
        libCsName = unlist(aliasMap[aliasedNames], use.names = FALSE),
        coSim = 1,
        stringsAsFactors = FALSE
      )
      aliasDiagnostics <- aliasRows
      aliasDiagnostics$matchMethod <- "fallbackSimplified"
      if (!is.null(matches) && ncol(matches) > 0L) {
        for (extraColumn in setdiff(names(matches), names(aliasRows))) {
          aliasRows[[extraColumn]] <- NA
        }
        aliasRows <- aliasRows[, names(matches), drop = FALSE]
      }
      ithMaybeMessage(
        verbose, "Aliased empty concept set(s): ",
        paste0(aliasRows$csName, " -> ", aliasRows$libCsName, collapse = ", ")
      )
    }
    matches <- rbind(matches, matchFinal$matches, aliasRows)
    diagnosticsFinal <- dplyr::bind_rows(keptDiagnostics, matchFinal$diagnostics, aliasDiagnostics)
    stillMissing <- setdiff(neededNames, unique(tolower(matches$csName)))
    if (length(stillMissing) > 0L) {
      warning(
        condition$testCondition, ": ", length(stillMissing), " concept set(s) could not be created or matched: ",
        paste(stillMissing, collapse = ", "), ". Scoring will stop until they are resolved.",
        call. = FALSE, immediate. = TRUE
      )
    }

    if (!is.null(diagnosticsFinal) && nrow(diagnosticsFinal) > 0) {
      diagnosticsFinal$phase <- "afterCreateMissing"
      diagnosticsFinal$conditionName <- condition$testCondition
      diagnosticsFinalPath <- ithSaveVersioned(
        saveDirectory = conditionDir,
        objectToSave = diagnosticsFinal,
        fileName = "conceptSetMatchDiagnostics.csv"
      )

      nExactFinal <- sum(diagnosticsFinal$matchMethod == "exact", na.rm = TRUE)
      nEmbeddingFinal <- sum(diagnosticsFinal$matchMethod == "embedding", na.rm = TRUE)
      nFuzzyFinal <- sum(diagnosticsFinal$matchMethod == "fuzzy", na.rm = TRUE)
      nHybridFinal <- sum(diagnosticsFinal$matchMethod == "embedding+fuzzy", na.rm = TRUE)
      nNoneFinal <- sum(diagnosticsFinal$matchMethod == "none", na.rm = TRUE)
      ithMaybeMessage(verbose, "Matching diagnostics (post-create): exact=", nExactFinal, ", embedding=", nEmbeddingFinal, ", fuzzy=", nFuzzyFinal, ", embedding+fuzzy=", nHybridFinal, ", none=", nNoneFinal)
      ithMaybeMessage(verbose, "Saved concept set matching diagnostics: ", diagnosticsFinalPath)
    }

    ithSaveVersioned(
      saveDirectory = conditionDir,
      objectToSave = matches,
      fileName = "matchedConceptSets.rds"
    )
    tryCatch(
      ithFlagRulesForEmptyConceptSets(
        conditionName = condition$testCondition,
        outputDirectory = outputDirectory,
        emptyNames = ithKnownEmptyMatchedNames(matches, csLibrary[[1]]),
        llmClient = llmClientReasoning,
        verbose = verbose
      ),
      error = function(e) {
        warning(condition$testCondition, ": could not flag rules with empty concept sets: ", conditionMessage(e),
                call. = FALSE, immediate. = TRUE)
      }
    )
    ithMaybeMessage(verbose, "Completed concept set resolution for condition: ", condition$testCondition)
  }

  if (conceptSetWorkers <= 1L) {
    for (conditionUp in seq_along(conditionSpecs)) {
      finishCondition(startCondition(conditionUp), createMissing = TRUE)
    }
  } else {
    # Match every condition first, build the union of missing sets in parallel, write them to the
    # library once all builds finish, then finish each condition's matching.
    states <- lapply(seq_along(conditionSpecs), startCondition)
    missingByCondition <- lapply(states, missingNamesOf)
    allMissing <- unique(unlist(missingByCondition))
    ithMaybeMessage(
      verbose,
      length(allMissing), " distinct concept set(s) to build across ", length(states), " condition(s)."
    )

    buildResults <- ithBuildConceptSetsParallel(
      conceptSetNames = allMissing,
      conceptSetFolder = conceptSetFolder,
      workers = conceptSetWorkers,
      database = database,
      llmConfig = llmConfig,
      builder = conceptSetBuilder,
      verbose = verbose
    )

    failedBuilds <- buildResults[!(buildResults$status %in% c("ok", "reused")), , drop = FALSE]
    ownerOf <- function(nameUp) states[[which(vapply(missingByCondition, function(x) nameUp %in% x, logical(1)))[[1]]]]
    unbuilt <- character()
    for (i in seq_len(nrow(failedBuilds))) {
      nameUp <- failedBuilds$conceptSetName[[i]]
      owner <- ownerOf(nameUp)
      resolved <- resolveFailedBuild(
        nameUp, failedBuilds$outcome[[i]], failedBuilds$message[[i]], owner$conditionDir,
        owner$condition$testCondition, owner$conditionRandomCohortDefinitionId
      )
      addSummary(nameUp, resolved$status, resolved$outcome, resolved$reason, resolved$aliasTarget, failedBuilds$seconds[[i]])
      if (identical(resolved$status, "error")) {
        unbuilt <- c(unbuilt, nameUp)
      }
    }
    for (i in seq_len(nrow(buildResults))) {
      if (buildResults$status[[i]] %in% c("ok", "reused")) {
        addSummary(buildResults$conceptSetName[[i]], if (identical(buildResults$status[[i]], "ok")) "built" else "reused", "ok", seconds = buildResults$seconds[[i]])
      }
    }
    if (length(unbuilt) > 0L) {
      warning(
        length(unbuilt), " concept set(s) could not be built and were not added to the library (rerun to retry): ",
        paste(unbuilt, collapse = ", "),
        ". See error.txt in each set's folder under ", conceptSetFolder, ".",
        call. = FALSE, immediate. = TRUE
      )
    }

    for (nameUp in setdiff(allMissing, failedBuilds$conceptSetName)) {
      owner <- states[[which(vapply(missingByCondition, function(x) nameUp %in% x, logical(1)))[[1]]]]
      csLibrary <- upsertConceptSetInLibrary(
        csLibraryName = csLibrary,
        conceptSetName = nameUp,
        cleanName = gsub("_", " ", nameUp),
        finalConceptSet = readRDS(ithConceptSetBuildResultPath(conceptSetFolder, nameUp)),
        conditionName = owner$condition$testCondition,
        cohortId = owner$conditionRandomCohortDefinitionId,
        sourceFile = file.path(conceptSetFolder, nameUp)
      )
    }

    for (state in states) {
      finishCondition(state, createMissing = FALSE)
    }
  }

  if (length(buildSummary) > 0L) {
    buildSummaryTable <- do.call(rbind, buildSummary)
    summaryPath <- ithSaveVersioned(
      saveDirectory = outputDirectory,
      objectToSave = buildSummaryTable,
      fileName = "conceptSetBuildSummary.csv"
    )
    ithMaybeMessage(
      verbose, "Concept set build summary (", paste(names(table(buildSummaryTable$status)), table(buildSummaryTable$status), sep = "=", collapse = ", "),
      "): ", summaryPath
    )
    if (verbose && any(!(buildSummaryTable$status %in% c("built", "reused")))) {
      print(buildSummaryTable[!(buildSummaryTable$status %in% c("built", "reused")), c("conceptSetName", "status", "outcome", "aliasTarget", "reason")], row.names = FALSE)
    }
  }
  if (length(stoppedEmptyNames) > 0L) {
    stop(
      length(stoppedEmptyNames), " concept set(s) came back empty and conceptSetBuilding.emptyConceptSet is 'stop': ",
      paste(unique(stoppedEmptyNames), collapse = ", "),
      ". Set it to 'placeholder' to record them as known empty.",
      call. = FALSE
    )
  }

  latestLibraryVersion <- ithDuckdbGetLatestLibraryVersion(conceptLibrarySourcePath)
  ithMaybeMessage(
    verbose,
    "Saved concept set library to: ",
    conceptLibrarySourcePath,
    " (latest libraryVersion=",
    latestLibraryVersion,
    ")"
  )
  ithMaybeMessage(verbose, "Concept set generation completed.")
}

ithResolveConceptSetPlugin <- function(pluginName,
                                           matchedConceptSets,
                                           conceptSetJsonLibrary,
                                           database) {
  resolution <- ithResolveConceptSetPluginDetails(
    pluginName = pluginName,
    matchedConceptSets = matchedConceptSets,
    conceptSetJsonLibrary = conceptSetJsonLibrary,
    database = database
  )

  resolution$value
}

ithPrepareConceptSetForResolution <- function(conceptSetJson) {
  parsed <- conceptSetJson

  requiredItemColumns <- c("concept", "isExcluded", "includeDescendants", "includeMapped")

  normalizeConceptRecord <- function(conceptValue) {
    if (is.null(conceptValue)) {
      return(NULL)
    }

    if (is.data.frame(conceptValue)) {
      if (nrow(conceptValue) == 0) {
        return(NULL)
      }
      return(as.list(conceptValue[1, , drop = FALSE]))
    }

    if (is.list(conceptValue) && length(conceptValue) == 1) {
      firstValue <- conceptValue[[1]]
      if (is.data.frame(firstValue)) {
        if (nrow(firstValue) == 0) {
          return(NULL)
        }
        return(as.list(firstValue[1, , drop = FALSE]))
      }
      if (is.list(firstValue) && !is.null(names(firstValue))) {
        return(firstValue)
      }
    }

    conceptValue
  }

  if (is.character(parsed) && length(parsed) > 0 && nzchar(parsed[[1]])) {
    parsed <- tryCatch(
      jsonlite::fromJSON(parsed[[1]], simplifyVector = FALSE),
      error = function(e) NULL
    )
  }

  if (is.null(parsed)) {
    return(NULL)
  }

  if (is.data.frame(parsed) && all(requiredItemColumns %in% names(parsed))) {
    items <- lapply(seq_len(nrow(parsed)), function(rowUp) {
      list(
        concept = normalizeConceptRecord(parsed$concept[rowUp, , drop = FALSE]),
        isExcluded = as.logical(parsed$isExcluded[[rowUp]]),
        includeDescendants = as.logical(parsed$includeDescendants[[rowUp]]),
        includeMapped = as.logical(parsed$includeMapped[[rowUp]])
      )
    })
    return(list(items = items))
  }

  if (is.list(parsed) && all(requiredItemColumns %in% names(parsed))) {
    itemCount <- max(length(parsed$isExcluded), length(parsed$includeDescendants), length(parsed$includeMapped))
    if (!is.null(parsed$concept) && is.data.frame(parsed$concept)) {
      itemCount <- max(itemCount, nrow(parsed$concept))
    }

    if (itemCount > 0) {
      items <- lapply(seq_len(itemCount), function(rowUp) {
        conceptValue <- if (!is.null(parsed$concept) && is.data.frame(parsed$concept) && nrow(parsed$concept) >= rowUp) {
          parsed$concept[rowUp, , drop = FALSE]
        } else if (!is.null(parsed$concept) && length(parsed$concept) >= rowUp) {
          parsed$concept[[rowUp]]
        } else {
          NULL
        }

        list(
          concept = normalizeConceptRecord(conceptValue),
          isExcluded = if (length(parsed$isExcluded) >= rowUp) as.logical(parsed$isExcluded[[rowUp]]) else FALSE,
          includeDescendants = if (length(parsed$includeDescendants) >= rowUp) as.logical(parsed$includeDescendants[[rowUp]]) else FALSE,
          includeMapped = if (length(parsed$includeMapped) >= rowUp) as.logical(parsed$includeMapped[[rowUp]]) else FALSE
        )
      })
      return(list(items = items))
    }
  }

  if (is.list(parsed) && length(parsed) > 0) {
    firstEntry <- parsed[[1]]
    looksLikeBareItems <-
      is.null(names(parsed)) &&
      is.list(firstEntry) &&
      all(c("concept", "isExcluded", "includeDescendants", "includeMapped") %in% names(firstEntry))

    if (looksLikeBareItems) {
      return(list(items = parsed))
    }
  }

  if (is.list(parsed) && !is.null(parsed$expression) && !is.null(parsed$expression$items) && is.null(parsed$items)) {
    return(parsed$expression)
  }

  parsed
}

ithBuildConceptSetLookupKeys <- function(csName, pluginName) {
  rawNames <- unique(c(csName, pluginName))
  rawNames <- rawNames[!is.na(rawNames) & nzchar(rawNames)]

  normalizedKeys <- character()
  for (nameUp in rawNames) {
    normalizedBase <- ithNormalizeConceptSetName(nameUp)
    normalizedKeys <- c(normalizedKeys, normalizedBase, ithGenerateFuzzyNameVariants(normalizedBase))
  }

  unique(normalizedKeys[nzchar(normalizedKeys)])
}

# Picks the library concept set that scoring would resolve for one rubric plugin, without touching
# the database. status is "selected", "missingMatch", "missingConceptSetJson" or "emptyConceptSetItems".
ithSelectLibraryConceptSet <- function(pluginName,
                                       matchedConceptSets,
                                       conceptSetJsonLibrary) {
  pluginNameNormalized <- ithNormalizeConceptSetName(pluginName)
  matchedNamesNormalized <- ithNormalizeConceptSetName(matchedConceptSets$csName)

  csName <- matchedConceptSets$libCsName[
    matchedNamesNormalized == pluginNameNormalized
  ]

  if (length(csName) == 0) {
    return(list(status = "missingMatch", csName = character(0)))
  }

  lookupKeys <- ithBuildConceptSetLookupKeys(csName[[1]], pluginName)
  conceptSetNames <- if ("conceptSetName" %in% names(conceptSetJsonLibrary)) {
    as.character(conceptSetJsonLibrary$conceptSetName)
  } else {
    character()
  }
  cleanNames <- if ("cleanName" %in% names(conceptSetJsonLibrary)) {
    as.character(conceptSetJsonLibrary$cleanName)
  } else {
    rep(NA_character_, nrow(conceptSetJsonLibrary))
  }

  candidateIdx <- integer(0)
  if (length(conceptSetNames) > 0) {
    conceptSetNameNormalized <- ithNormalizeConceptSetName(conceptSetNames)
    candidateIdx <- c(candidateIdx, which(conceptSetNameNormalized %in% lookupKeys))
  }
  if (length(cleanNames) > 0) {
    cleanNameNormalized <- ithNormalizeConceptSetName(cleanNames)
    candidateIdx <- c(candidateIdx, which(cleanNameNormalized %in% lookupKeys))
  }
  candidateIdx <- unique(candidateIdx)

  if (length(candidateIdx) == 0) {
    return(list(status = "missingConceptSetJson", csName = csName))
  }

  csJson <- NULL
  chosenIndex <- NA_integer_
  for (candidateUp in candidateIdx) {
    candidateJson <- conceptSetJsonLibrary$conceptSet[[candidateUp]]
    prepared <- ithPrepareConceptSetForResolution(candidateJson)
    if (is.list(prepared) && !is.null(prepared$items) && length(prepared$items) > 0) {
      csJson <- prepared
      chosenIndex <- candidateUp
      break
    }
  }

  if (is.null(csJson) || is.null(csJson$items) || length(csJson$items) == 0) {
    return(list(status = "emptyConceptSetItems", csName = csName))
  }

  list(
    status = "selected",
    csName = csName,
    csJson = csJson,
    chosenIndex = chosenIndex,
    conceptSetNames = conceptSetNames,
    cleanNames = cleanNames
  )
}

resolveConceptSet <- function(conceptSet,
                              connectionDetails,
                              cdmDatabaseSchema) {
  connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  json_txt <- jsonlite::toJSON(
    conceptSet,
    pretty = TRUE,
    simplifyVector = FALSE,
    auto_unbox = TRUE
  )

  sql <- CirceR::buildConceptSetQuery(json_txt)
  sql <- SqlRender::render(sql = sql, vocabulary_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  conceptIdList <- DatabaseConnector::querySql(
    connection = connection,
    sql,
    snakeCaseToCamelCase = TRUE
  )

  conceptIdList
}

ithResolveConceptSetPluginDetails <- function(pluginName,
                                              matchedConceptSets,
                                              conceptSetJsonLibrary,
                                              database) {
  fallback <- "36433327"

  selection <- ithSelectLibraryConceptSet(pluginName, matchedConceptSets, conceptSetJsonLibrary)
  if (!identical(selection$status, "selected")) {
    return(list(
      value = fallback,
      status = selection$status,
      matchedConceptSetName = if (length(selection$csName) == 0) NA_character_ else selection$csName[[1]],
      resolvedConceptCount = 0L
    ))
  }
  csName <- selection$csName
  csJson <- selection$csJson
  chosenIndex <- selection$chosenIndex
  conceptSetNames <- selection$conceptSetNames
  cleanNames <- selection$cleanNames
  targetNormalized <- ithNormalizeConceptSetName(csName[[1]])
  pluginNormalized <- ithNormalizeConceptSetName(pluginName)

  resolveConceptSetWithFallback <- function(conceptSetObject) {
    resolveWithDirectCirce <- function(conceptSetJsonText) {
      tryCatch({
        conceptSetSql <- CirceR::buildConceptSetQuery(conceptSetJsonText)
        schemaParameters <- list(
          vocabulary_database_schema = database$cdmDatabaseSchema,
          cdm_database_schema = database$cdmDatabaseSchema,
          cdmDatabaseSchema = database$cdmDatabaseSchema
        )
        # SqlRender warns about parameters the query does not use, so pass only the ones it references.
        schemaParameters <- schemaParameters[vapply(
          names(schemaParameters), function(name) grepl(paste0("@", name), conceptSetSql, fixed = TRUE), logical(1)
        )]
        renderedSql <- do.call(SqlRender::render, c(list(sql = conceptSetSql), schemaParameters))
        translatedSql <- SqlRender::translate(renderedSql, database$connectionDetails$dbms)

        conn <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
        on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

        resolved <- DatabaseConnector::querySql(
          conn,
          translatedSql,
          snakeCaseToCamelCase = TRUE,
          integer64AsNumeric = FALSE
        )

        if (!is.null(resolved) && "concept_id" %in% names(resolved) && !("conceptId" %in% names(resolved))) {
          names(resolved)[names(resolved) == "concept_id"] <- "conceptId"
        }

        if (is.null(resolved) || nrow(resolved) == 0) {
          return(list(result = resolved, strategy = "directCirce"))
        }

        if (!("conceptId" %in% names(resolved))) {
          stop("Direct Circe resolution returned no conceptId column")
        }

        list(result = resolved, strategy = "directCirce")
      }, error = function(e) {
        list(result = NULL, strategy = "directCirceFailed", errorMessage = e$message)
      })
    }

    resolveWithStrategies <- function(conceptSetInput) {
      strategyConceptSet <- tryCatch(
        resolveConceptSet(
          conceptSet = conceptSetInput,
          connectionDetails = database$connectionDetails,
          cdmDatabaseSchema = database$cdmDatabaseSchema
        ),
        error = function(e) e
      )
      if (!inherits(strategyConceptSet, "error")) {
        return(list(result = strategyConceptSet, strategy = "conceptSet"))
      }

      list(
        result = NULL,
        strategy = "failed",
        errorMessage = strategyConceptSet$message
      )
    }

    conceptSetJsonText <- jsonlite::toJSON(
      conceptSetObject,
      pretty = FALSE,
      simplifyVector = FALSE,
      auto_unbox = TRUE,
      null = "null"
    )
    conceptSetJsonText <- paste(as.character(conceptSetJsonText), collapse = "")

    if (!nzchar(conceptSetJsonText)) {
      return(list(
        concepts = NULL,
        usedJsonTextPath = FALSE,
        callStrategy = "none",
        errorMessage = "Generated conceptSet JSON text is empty"
      ))
    }

    directCirceAttempt <- resolveWithDirectCirce(conceptSetJsonText)

    if (!is.null(directCirceAttempt$result)) {
      return(list(
        concepts = directCirceAttempt$result,
        usedJsonTextPath = TRUE,
        callStrategy = directCirceAttempt$strategy,
        errorMessage = NULL
      ))
    }

    primaryAttempt <- resolveWithStrategies(conceptSetJsonText)

    if (!is.null(primaryAttempt$result)) {
      return(list(
        concepts = primaryAttempt$result,
        usedJsonTextPath = TRUE,
        callStrategy = primaryAttempt$strategy,
        errorMessage = NULL
      ))
    }

    listFallbackAttempt <- resolveWithStrategies(conceptSetObject)

    if (!is.null(listFallbackAttempt$result)) {
      return(list(
        concepts = listFallbackAttempt$result,
        usedJsonTextPath = FALSE,
        callStrategy = listFallbackAttempt$strategy,
        errorMessage = NULL
      ))
    }

    jsonInputDiagnostics <- paste0(
      "jsonClass=", paste(class(conceptSetJsonText), collapse = "/"),
      "; jsonLength=", length(conceptSetJsonText),
      "; jsonNchar=", ifelse(length(conceptSetJsonText) == 1 && !is.na(conceptSetJsonText), nchar(conceptSetJsonText), NA_integer_)
    )

    list(
      concepts = NULL,
      usedJsonTextPath = FALSE,
      callStrategy = "failed",
      errorMessage = paste0(
        "jsonPrimary: ", primaryAttempt$errorMessage,
        " | listFallback: ", listFallbackAttempt$errorMessage,
        " | directCirce: ", directCirceAttempt$errorMessage,
        " | ", jsonInputDiagnostics
      )
    )
  }

  resolvedConceptSet <- resolveConceptSetWithFallback(csJson)
  if (is.null(resolvedConceptSet$concepts)) {
    return(list(
      value = fallback,
      status = "resolveConceptSetError",
      matchedConceptSetName = csName[[1]],
      resolvedConceptCount = 0L,
      note = resolvedConceptSet$errorMessage
    ))
  }

  concepts <- resolvedConceptSet$concepts

  resolvedMatchedName <- csName[[1]]
  if (!is.na(chosenIndex) && length(conceptSetNames) >= chosenIndex && !is.na(conceptSetNames[[chosenIndex]]) && nzchar(conceptSetNames[[chosenIndex]])) {
    resolvedMatchedName <- conceptSetNames[[chosenIndex]]
  } else if (!is.na(chosenIndex) && length(cleanNames) >= chosenIndex && !is.na(cleanNames[[chosenIndex]]) && nzchar(cleanNames[[chosenIndex]])) {
    resolvedMatchedName <- cleanNames[[chosenIndex]]
  }

  if (nrow(concepts) == 0) {
    return(list(
      value = fallback,
      status = "zeroResolvedConcepts",
      matchedConceptSetName = resolvedMatchedName,
      resolvedConceptCount = 0L
    ))
  }

  uniqueConceptIds <- unique(concepts$conceptId)
  baseStatus <- if (ithNormalizeConceptSetName(resolvedMatchedName) %in% c(targetNormalized, pluginNormalized)) {
    "resolved"
  } else {
    "resolvedAlias"
  }

  resolutionStatus <- if (!is.null(resolvedConceptSet$callStrategy) && identical(resolvedConceptSet$callStrategy, "directCirce")) {
    paste0(baseStatus, "DirectCirce")
  } else if (isTRUE(resolvedConceptSet$usedJsonTextPath)) {
    paste0(baseStatus, "JsonPrimary")
  } else {
    baseStatus
  }

  list(
    value = paste(uniqueConceptIds, collapse = ","),
    status = resolutionStatus,
    matchedConceptSetName = resolvedMatchedName,
    resolvedConceptCount = length(uniqueConceptIds)
  )
}

ithScoringSqlConceptSets <- function(sqlStatements) {
  plugins <- unique(unlist(stringr::str_extract_all(as.character(sqlStatements), "\\B@[A-Za-z0-9_]+")))
  plugins <- plugins[!(plugins %in% c("@databaseSchema", "@cohortSchema", "@cohortTable", "@cohortId"))]
  substr(plugins, 2, nchar(plugins))
}

# Stops before any database work when a condition has no matchedConceptSets_V*.rds or its latest
# short rubric uses concept sets that file does not cover; scoring would otherwise substitute a
# placeholder concept and silently write zero-hit results for those rules.
ithCheckConceptSetsReadyForScoring <- function(conditionSpecs, outputDirectory) {
  problems <- character(0)
  for (condition in conditionSpecs) {
    conditionName <- as.character(condition$rubricName[[1]])
    conditionDir <- file.path(outputDirectory, conditionName)
    shortRubric <- tryCatch(ithLoadLatestShortRubric(conditionName, outputDirectory), error = function(e) NULL)
    if (is.null(shortRubric)) {
      problems <- c(problems, paste0(conditionName, ": no short rubric (run getRubricsFromConfig.R)"))
      next
    }
    used <- ithScoringSqlConceptSets(shortRubric$sqlStatement)
    matched <- ithLoadLatest(conditionDir, "matchedConceptSets.rds")
    if (is.null(matched) || !("csName" %in% names(matched))) {
      problems <- c(problems, paste0(conditionName, ": no matchedConceptSets_V*.rds (", length(used), " concept sets needed; run createRubricConceptSetsFromConfig.R)"))
      next
    }
    missing <- used[!(ithNormalizeConceptSetName(used) %in% ithNormalizeConceptSetName(matched$csName))]
    ithWarnRulesWithEmptyConceptSets(conditionName, conditionDir)
    if (length(missing) == 0L) next
    # Names the SQL uses but no rule lists in neededConceptSets were invented during SQL generation;
    # rerunning concept-set creation cannot fix those, so report them per rule.
    listed <- ithNormalizeConceptSetName(unlist(lapply(as.character(shortRubric$neededConceptSets), ithParseConceptSets)))
    unlisted <- missing[!(ithNormalizeConceptSetName(missing) %in% listed)]
    stale <- setdiff(missing, unlisted)
    if (length(stale) > 0L) {
      problems <- c(problems, paste0(
        conditionName, ": ", length(stale), " concept set(s) in neededConceptSets but not in the latest ",
        "matchedConceptSets (rubric edited since concept sets were created? rerun createRubricConceptSetsFromConfig.R): ",
        paste(stale, collapse = ", ")
      ))
    }
    for (i in seq_len(nrow(shortRubric))) {
      ruleUnlisted <- intersect(ithScoringSqlConceptSets(shortRubric$sqlStatement[[i]]), unlisted)
      if (length(ruleUnlisted) == 0L) next
      problems <- c(problems, paste0(
        conditionName, " ", shortRubric$rubric[[i]], " ", shortRubric$ruleId[[i]],
        ": SQL uses concept set(s) not listed in the rule's neededConceptSets: ", paste(ruleUnlisted, collapse = ", "),
        " (fix the rule in the concise rubric CSV and run applyConciseRubricEdits to regenerate its SQL, ",
        "or add the names to neededConceptSets and rerun createRubricConceptSetsFromConfig.R)"
      ))
    }
  }
  if (length(problems) > 0L) {
    stop(
      "Concept sets are not ready for scoring:\n  ", paste(problems, collapse = "\n  "),
      "\nAfter running createRubricConceptSetsFromConfig.R, score again (if conceptLibraryVersion is pinned ",
      "in config.yaml, set it to the new libraryVersion or blank it to use the latest). ",
      "Use the conditionSpecs 'ignore' column to score the other conditions now.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ithScoringArtifactsComplete <- function(cohortDir, randomCohortDefinitionId) {
  requiredFiles <- c(
    paste0("sumCohortScores_Cohort", randomCohortDefinitionId, ".rds"),
    paste0("finalScores_Cohort", randomCohortDefinitionId, ".rds"),
    paste0("subjectCount_Cohort", randomCohortDefinitionId, ".csv"),
    paste0("fullScoringFile_Cohort", randomCohortDefinitionId, ".rds")
  )
  all(vapply(requiredFiles, function(fileName) {
    !is.null(ithFindLatestVersion(cohortDir, fileName))
  }, logical(1)))
}

ithScoreConditionDatabase <- function(condition,
                                         database,
                                         outputDirectory,
                                         seedLibraryPath = NULL,
                                         conceptLibraryVersion = NULL,
                                         cohortTableName = NULL,
                                         useExisting = FALSE,
                                         retryLimit = 10,
                                         llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                                         promptDir = NULL,
                                         cluster = NULL,
                                         verbose = TRUE) {
  randomCohortDefinitionIdRaw <- condition$randomCohortDefinitionId
  randomCohortDefinitionId <- suppressWarnings(as.integer(
    if (is.null(randomCohortDefinitionIdRaw) || length(randomCohortDefinitionIdRaw) == 0) NA else randomCohortDefinitionIdRaw[[1]]
  ))
  if (length(randomCohortDefinitionId) == 0 || is.na(randomCohortDefinitionId)) {
    stop(
      "condition must include randomCohortDefinitionId for scoring (rubricName=",
      as.character(condition$rubricName),
      ")"
    )
  }

  ithMaybeMessage(verbose, "Scoring condition '", condition$rubricName, "' on database '", database$databaseId, "'.")
  scoringDir <- file.path(outputDirectory, condition$rubricName)
  shortRubric <- ithLoadLatestShortRubric(condition$rubricName, outputDirectory)
  shortRubric <- shortRubric[order(shortRubric$rubric, ithRuleIdOrder(shortRubric$ruleId)), ]

  fullRubric <- ithLoadLatest(scoringDir, "rubricList.rds")$rubricList
  csLibrary <- ithLoadConceptSetLibraryForScoring(
    outputDirectory = outputDirectory,
    seedLibraryPath = seedLibraryPath,
    libraryVersion = conceptLibraryVersion
  )
  conceptSetJsonLibrary <- csLibrary[[1]]
  matched <- ithLoadLatest(scoringDir, "matchedConceptSets.rds")

  tempTable <- if (is.null(cohortTableName)) ithDefaultCohortTable(database) else cohortTableName
  cohortDir <- file.path(outputDirectory, "scoring", condition$rubricName, database$databaseId)
  dir.create(cohortDir, recursive = TRUE, showWarnings = FALSE)
  workDirectory <- getwd()
  lastQueryDir <- file.path(workDirectory, "lastQuery")
  dir.create(lastQueryDir, recursive = TRUE, showWarnings = FALSE)
  lastQueryPath <- file.path(lastQueryDir, "query.sql")

  if (useExisting && ithScoringArtifactsComplete(cohortDir, randomCohortDefinitionId)) {
    ithMaybeMessage(verbose, "Using existing scoring outputs for condition '", condition$rubricName, "' on database '", database$databaseId, "'.")
    scoredFile <- ithLoadLatest(cohortDir, paste0("fullScoringFile_Cohort", randomCohortDefinitionId, ".rds"))
    subjectCounts <- ithLoadLatest(cohortDir, paste0("subjectCount_Cohort", randomCohortDefinitionId, ".csv"))
    return(list(scoredFile = scoredFile, subjectCounts = subjectCounts))
  } else if (useExisting) {
    ithMaybeMessage(
      verbose,
      "Existing scoring outputs for condition '", condition$rubricName,
      "' on database '", database$databaseId,
      "' are incomplete; rerunning scoring."
    )
  }

  conn <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  sqlChanged <- FALSE
  ruleCount <- nrow(shortRubric)

  rulePlugins <- lapply(shortRubric$sqlStatement, function(sqlStatement) {
    plugins <- unique(stringr::str_extract_all(sqlStatement, "\\B@[A-Za-z0-9_]+")[[1]])
    plugins[!(plugins %in% c("@databaseSchema", "@cohortSchema", "@cohortTable", "@cohortId"))]
  })
  uniquePluginNames <- unique(unlist(lapply(rulePlugins, function(plugins) substr(plugins, 2, nchar(plugins)))))
  pluginResolutionCache <- new.env(parent = emptyenv())
  resolvedPluginCount <- 0L
  resolvePluginCached <- function(pluginName) {
    cacheKey <- ithNormalizeConceptSetName(pluginName)
    if (exists(cacheKey, envir = pluginResolutionCache, inherits = FALSE)) {
      return(get(cacheKey, envir = pluginResolutionCache, inherits = FALSE))
    }
    resolvedPluginCount <<- resolvedPluginCount + 1L
    ithMaybeMessage(
      verbose,
      "Resolving scoring concept set ", resolvedPluginCount, "/", length(uniquePluginNames),
      ": ", pluginName
    )
    resolved <- ithResolveConceptSetPluginDetails(
      pluginName = pluginName,
      matchedConceptSets = matched,
      conceptSetJsonLibrary = conceptSetJsonLibrary,
      database = database
    )
    assign(cacheKey, resolved, envir = pluginResolutionCache)
    resolved
  }
  if (length(uniquePluginNames) > 0L) {
    ithMaybeMessage(
      verbose,
      "Preparing ", length(uniquePluginNames),
      " unique scoring concept set(s) before parallel rule queries."
    )
  }

  # Concept-set plugins are resolved once per unique plugin in the main process.
  preparedRules <- lapply(seq_len(ruleCount), function(rubricUp) {
    plugins <- rulePlugins[[rubricUp]]

    pluginValues <- NULL
    missingConceptSetDetails <- character(0)
    logRows <- NULL
    if (length(plugins) > 0) {
      pluginNames <- substr(plugins, 2, nchar(plugins))
      pluginDetails <- lapply(
        pluginNames,
        resolvePluginCached
      )

      pluginValues <- data.frame(
        replaceText = plugins,
        values = vapply(pluginDetails, function(x) x$value, character(1)),
        stringsAsFactors = FALSE
      )

      pluginStatuses <- vapply(pluginDetails, function(x) as.character(x$status), character(1))
      missingPluginIdx <- which(!grepl("^resolved", pluginStatuses))
      if (length(missingPluginIdx) > 0) {
        missingConceptSetDetails <- paste0(
          pluginNames[missingPluginIdx],
          "(",
          pluginStatuses[missingPluginIdx],
          ")"
        )
      }

      logRows <- do.call(rbind, lapply(seq_along(pluginNames), function(plugUp) {
        data.frame(
          conditionName = condition$rubricName,
          databaseId = database$databaseId,
          randomCohortDefinitionId = randomCohortDefinitionId,
          rubricId = shortRubric$rubric[[rubricUp]],
          ruleId = shortRubric$ruleId[[rubricUp]],
          pluginName = pluginNames[[plugUp]],
          conceptLibraryVersion = as.integer(conceptLibraryVersion[[1]]),
          matchedConceptSetName = pluginDetails[[plugUp]]$matchedConceptSetName,
          resolutionStatus = pluginDetails[[plugUp]]$status,
          resolvedConceptCount = pluginDetails[[plugUp]]$resolvedConceptCount,
          resolutionNote = if (!is.null(pluginDetails[[plugUp]]$note)) as.character(pluginDetails[[plugUp]]$note) else NA_character_,
          stringsAsFactors = FALSE
        )
      }))
    }
    list(pluginValues = pluginValues, missingConceptSetDetails = missingConceptSetDetails, logRows = logRows)
  })
  pluginResolutionLog <- do.call(rbind, lapply(preparedRules, function(x) x$logRows))

  renderRuleSql <- function(rubricUp, sqlText) {
    rendered <- SqlRender::render(
      sqlText,
      databaseSchema = database$cdmDatabaseSchema,
      cohortSchema = database$workDatabaseSchema,
      cohortTable = tempTable,
      cohortId = randomCohortDefinitionId
    )
    pluginValues <- preparedRules[[rubricUp]]$pluginValues
    if (!is.null(pluginValues)) {
      for (plugUp in seq_len(nrow(pluginValues))) {
        rendered <- gsub(
          pluginValues$replaceText[[plugUp]],
          pluginValues$values[[plugUp]],
          rendered,
          fixed = TRUE
        )
      }
    }
    SqlRender::translate(rendered, database$connectionDetails$dbms)
  }

  # Runs the remaining attempts for one rule in the main process; from the second attempt on, the
  # LLM corrects the SQL using the last error. `state` carries any attempt already run on a worker.
  runRuleAttempts <- function(rubricUp, state) {
    while (!state$success && state$attempt < retryLimit) {
      state$attempt <- state$attempt + 1L

      if (state$attempt > 1) {
        ithMaybeMessage(verbose, "Retrying SQL correction for rule ", shortRubric$ruleId[[rubricUp]], " (attempt ", state$attempt, "/", retryLimit, ").")
        corrected <- ithCorrectSqlStatement(shortRubric$sqlStatement[[rubricUp]], llmClient, error = state$errorMessage, promptDir = promptDir)
        shortRubric$sqlStatement[[rubricUp]] <<- corrected$correctedQuery
        sqlChanged <<- TRUE
      }

      sql <- renderRuleSql(rubricUp, shortRubric$sqlStatement[[rubricUp]])
      writeLines(sql, con = lastQueryPath)

      startedAt <- Sys.time()
      queryResult <- tryCatch(
        DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE),
        error = function(e) {
          state$errorMessage <<- e$message
          NULL
        }
      )
      if (state$attempt == 1L) {
        state$seconds <- as.numeric(difftime(Sys.time(), startedAt, units = "secs"))
      }

      if (!is.null(queryResult)) {
        state$success <- TRUE
        state$results <- queryResult
      }
    }
    state
  }

  initialState <- list(attempt = 0L, success = FALSE, results = NULL, errorMessage = NULL, seconds = NA_real_)
  ruleStates <- rep(list(initialState), ruleCount)

  if (!is.null(cluster) && ruleCount > 0L && retryLimit >= 1) {
    # First attempts run on the workers, slowest rules first; failures are retried below.
    tasks <- lapply(seq_len(ruleCount), function(rubricUp) {
      list(
        sql = renderRuleSql(rubricUp, shortRubric$sqlStatement[[rubricUp]]),
        connectionDetails = database$connectionDetails,
        databaseId = database$databaseId,
        lastQueryDir = lastQueryDir
      )
    })
    ithMaybeMessage(
      verbose,
      "Starting parallel scoring for condition '", condition$rubricName,
      "' on database '", database$databaseId, "': ", ruleCount,
      " rule(s) across ", length(cluster), " worker(s)."
    )
    dispatchedCount <- 0L
    ithRunOnCluster(
      cluster,
      tasks,
      ithRunScoringQueryOnWorker,
      dispatchOrder = ithScoringDispatchOrder(shortRubric, cohortDir),
      onDispatch = function(rubricUp) {
        dispatchedCount <<- dispatchedCount + 1L
        ithMaybeMessage(
          verbose,
          sprintf(
            "[%s] dispatched %d/%d %s/%s to scoring workers",
            format(Sys.time(), "%H:%M:%S"),
            dispatchedCount,
            ruleCount,
            shortRubric$rubric[[rubricUp]],
            shortRubric$ruleId[[rubricUp]]
          )
        )
      },
      onResult = function(rubricUp, value, done, total) {
        if (!is.list(value) || !("seconds" %in% names(value))) {
          value <- list(result = NULL, errorMessage = paste(as.character(value), collapse = " "), seconds = NA_real_)
        }
        ruleStates[[rubricUp]] <<- list(
          attempt = 1L,
          success = !is.null(value$result),
          results = value$result,
          errorMessage = value$errorMessage,
          seconds = value$seconds
        )
        ithMaybeMessage(
          verbose,
          sprintf(
            "[%s] finished %d/%d %s/%s: %s in %.0fs%s", format(Sys.time(), "%H:%M:%S"), done, total,
            shortRubric$rubric[[rubricUp]], shortRubric$ruleId[[rubricUp]],
            if (is.null(value$result)) "error" else "ok", ithCoalesce(value$seconds, NA_real_),
            if (is.null(value$result)) "" else paste0(", ", nrow(value$result), " rows")
          )
        )
      }
    )
    ithMaybeMessage(
      verbose,
      "Completed parallel scoring for condition '", condition$rubricName,
      "' on database '", database$databaseId, "': ", ruleCount,
      "/", ruleCount, " rule(s) finished."
    )
  }

  scoredPieces <- vector("list", ruleCount)
  subjectCountPieces <- vector("list", ruleCount)
  for (rubricUp in seq_len(ruleCount)) {
    state <- runRuleAttempts(rubricUp, ruleStates[[rubricUp]])
    ruleStates[[rubricUp]] <- state
    results <- state$results
    missingConceptSetDetails <- preparedRules[[rubricUp]]$missingConceptSetDetails

    includedSubjects <- if (is.null(results)) 0 else nrow(results)
    missingConceptSetSuffix <- if (length(missingConceptSetDetails) > 0) {
      paste0(
        " ; missingConceptSet=",
        paste(missingConceptSetDetails, collapse = ",")
      )
    } else {
      ""
    }
    ithMaybeMessage(
      verbose,
      "Scoring query complete: randomCohortDefinitionId=", randomCohortDefinitionId,
      ", rubricId=", shortRubric$rubric[[rubricUp]],
      ", ruleId=", shortRubric$ruleId[[rubricUp]],
      ", includedSubjects=", includedSubjects,
      missingConceptSetSuffix
    )

    if (!state$success) {
      ithMaybeMessage(
        verbose,
        "Scoring query failed after ", retryLimit,
        " attempt(s): randomCohortDefinitionId=", randomCohortDefinitionId,
        ", rubricId=", shortRubric$rubric[[rubricUp]],
        ", ruleId=", shortRubric$ruleId[[rubricUp]],
        ". Last error: ", state$errorMessage
      )
    }

    if (!is.null(results) && nrow(results) > 0) {
      scoredPieces[[rubricUp]] <- data.frame(
        cohortDefinitionId = results$cohortDefinitionId,
        subjectId = results$subjectId,
        cohortStartDate = results$cohortStartDate,
        rubricId = shortRubric$rubric[[rubricUp]],
        ruleId = shortRubric$ruleId[[rubricUp]],
        score = shortRubric$score[[rubricUp]],
        stringsAsFactors = FALSE
      )
    }

    subjectCountPieces[[rubricUp]] <- data.frame(
      cohortDefinitionId = randomCohortDefinitionId,
      rubricId = shortRubric$rubric[[rubricUp]],
      ruleId = shortRubric$ruleId[[rubricUp]],
      score = shortRubric$score[[rubricUp]],
      subjectCount = includedSubjects,
      criteria = shortRubric$criteria[[rubricUp]],
      stringsAsFactors = FALSE
    )
    # Drop the rows once copied so large results are not held twice.
    ruleStates[[rubricUp]]$results <- NULL
  }
  # Bound once at the end: binding inside the loop copies the growing table for every rule.
  scoredFile <- do.call(rbind, scoredPieces)
  subjectCounts <- do.call(rbind, subjectCountPieces)
  ithSaveScoringRuleTimes(
    cohortDir,
    shortRubric,
    seconds = vapply(ruleStates, function(x) as.numeric(ithCoalesce(x$seconds, NA_real_)), numeric(1)),
    rowCounts = subjectCounts$subjectCount
  )

  if (sqlChanged) {
    ithSaveShortRubric(shortRubric, condition$rubricName, outputDirectory)
  }

  if (!is.null(pluginResolutionLog) && nrow(pluginResolutionLog) > 0) {
    pluginResolutionPath <- ithSaveVersioned(
      saveDirectory = cohortDir,
      objectToSave = pluginResolutionLog,
      fileName = paste0("pluginResolution_Cohort", randomCohortDefinitionId, ".csv")
    )
    ithMaybeMessage(verbose, "Saved plugin resolution diagnostics: ", pluginResolutionPath)
  }

  ithSaveVersioned(
    saveDirectory = cohortDir,
    objectToSave = subjectCounts,
    fileName = paste0("subjectCount_Cohort", randomCohortDefinitionId, ".csv")
  )

  ithSaveVersioned(
    saveDirectory = cohortDir,
    objectToSave = scoredFile,
    fileName = paste0("fullScoringFile_Cohort", randomCohortDefinitionId, ".rds")
  )

  allSubjects <- data.table::as.data.table(
    ithGetCohortSubjects(randomCohortDefinitionId, database, cohortTable = tempTable, verbose = verbose)
  )
  ithMaybeMessage(
    verbose,
    "Loaded denominator cohort subjects: randomCohortDefinitionId=", randomCohortDefinitionId,
    ", subjectCount=", nrow(allSubjects)
  )

  scoredFile <- data.table::as.data.table(scoredFile)
  if (!is.null(scoredFile) && nrow(scoredFile) > 0) {
    scoredFile$subjectId <- as.double(scoredFile$subjectId)
    allSubjects$subjectId <- as.double(allSubjects$subjectId)
  }

  cohortScores <- scoredFile[
    allSubjects,
    on = .(cohortDefinitionId, subjectId, cohortStartDate)
  ]

  sumScoresPre <- cohortScores[
    , .(total = sum(score, na.rm = TRUE)),
    by = .(cohortDefinitionId, subjectId, cohortStartDate, rubricId)
  ]

  getRubricIdentifier <- function(rubricObj) {
    if (!is.null(rubricObj$rubricId) && length(rubricObj$rubricId) > 0) {
      return(as.character(rubricObj$rubricId[[1]]))
    }
    if (!is.null(rubricObj$rubric_id) && length(rubricObj$rubric_id) > 0) {
      return(as.character(rubricObj$rubric_id[[1]]))
    }
    NA_character_
  }

  getDecisionThresholds <- function(rubricObj) {
    thresholds <- NULL
    if (!is.null(rubricObj$decisionThresholds)) {
      thresholds <- rubricObj$decisionThresholds
    } else if (!is.null(rubricObj$decision_thresholds)) {
      thresholds <- rubricObj$decision_thresholds
    }

    if (is.null(thresholds)) {
      return(list(yes = NA_real_, no = NA_real_))
    }

    yesScore <- if (!is.null(thresholds$yes) && length(thresholds$yes) > 0) as.numeric(thresholds$yes[[1]]) else NA_real_
    noScore <- if (!is.null(thresholds$no) && length(thresholds$no) > 0) as.numeric(thresholds$no[[1]]) else NA_real_

    list(yes = yesScore, no = noScore)
  }

  allScoring <- NULL
  for (rubricUp in seq_along(fullRubric)) {
    rubricIdValue <- getRubricIdentifier(fullRubric[[rubricUp]])
    thresholds <- getDecisionThresholds(fullRubric[[rubricUp]])

    if (is.na(rubricIdValue) || !nzchar(rubricIdValue)) {
      ithMaybeMessage(verbose, "Skipping rubric threshold row due to missing rubric id at index ", rubricUp)
      next
    }

    if (is.na(thresholds$yes) || is.na(thresholds$no)) {
      ithMaybeMessage(verbose, "Skipping rubric threshold row for rubricId=", rubricIdValue, " due to missing decision thresholds.")
      next
    }

    allScoring <- rbind(
      allScoring,
      data.frame(
        rubricId = rubricIdValue,
        yesScore = thresholds$yes,
        noScore = thresholds$no,
        stringsAsFactors = FALSE
      )
    )
  }

  if (is.null(allScoring) || nrow(allScoring) == 0) {
    stop("No valid rubric thresholds available; unable to compute subject case labels.")
  }

  allScoring <- data.table::as.data.table(allScoring)

  allSubjects[, tmp := 1]
  allScoring[, tmp := 1]
  allSubjectsRubrics <- allSubjects[allScoring, on = "tmp", allow.cartesian = TRUE][, tmp := NULL]

  sumScores <- merge(allSubjectsRubrics, sumScoresPre, all.x = TRUE)
  sumScores$total[is.na(sumScores$total)] <- 0
  sumScores$case <- -1
  sumScores$case[sumScores$total >= sumScores$yesScore] <- 1
  sumScores$case[sumScores$total > sumScores$noScore & sumScores$total < sumScores$yesScore] <- 0

  finalScores <- sumScores[
    , .(total = sum(case, na.rm = TRUE)),
    by = .(cohortDefinitionId, subjectId, cohortStartDate)
  ]
  finalScores$case <- 0
  finalScores$case[finalScores$total > 0] <- 1
  finalScores$case[finalScores$total < 0] <- -1

  ithSaveVersioned(
    saveDirectory = cohortDir,
    objectToSave = sumScores,
    fileName = paste0("sumCohortScores_Cohort", randomCohortDefinitionId, ".rds")
  )

  ithSaveVersioned(
    saveDirectory = cohortDir,
    objectToSave = finalScores,
    fileName = paste0("finalScores_Cohort", randomCohortDefinitionId, ".rds")
  )

  ithMaybeMessage(verbose, "Completed scoring for condition '", condition$rubricName, "' on database '", database$databaseId, "'.")

  list(scoredFile = scoredFile, subjectCounts = subjectCounts, finalScores = finalScores)
}

ithScoreSubjects <- function(conditionSpecs,
                               databases,
                               outputDirectory,
                               seedLibraryPath = NULL,
                               conceptLibraryVersion = NULL,
                               cohortTableName = NULL,
                               useExisting = FALSE,
                               generateCohorts = FALSE,
                               cohortDefinitionSet = NULL,
                               webapiBaseUrl = NULL,
                               llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                               promptDir = NULL,
                               scoringWorkers = 1L,
                               cohortWorkers = 1L,
                               verbose = TRUE) {
  ithMaybeMessage(verbose, "Starting subject scoring across ", length(conditionSpecs), " condition(s) and ", length(databases), " database(s).")

  # Resolved once so every condition and database in this run uses the same snapshot.
  conceptLibraryVersion <- ithResolveScoringConceptLibraryVersion(
    conceptLibraryVersion, outputDirectory, seedLibraryPath, verbose = verbose
  )

  resolveCohortTableName <- function(cohortTableName, database, dbUp, databaseCount) {
    if (is.null(cohortTableName)) {
      return(NULL)
    }

    if (is.character(cohortTableName)) {
      if (length(cohortTableName) == 1 && nzchar(cohortTableName[[1]])) {
        return(cohortTableName[[1]])
      }

      if (!is.null(names(cohortTableName)) && database$databaseId %in% names(cohortTableName)) {
        resolved <- cohortTableName[[database$databaseId]]
        if (!is.null(resolved) && nzchar(as.character(resolved[[1]]))) {
          return(as.character(resolved[[1]]))
        }
      }

      if (is.null(names(cohortTableName)) && length(cohortTableName) == databaseCount) {
        resolved <- cohortTableName[[dbUp]]
        if (!is.null(resolved) && nzchar(as.character(resolved[[1]]))) {
          return(as.character(resolved[[1]]))
        }
      }
    }

    if (is.list(cohortTableName)) {
      if (!is.null(names(cohortTableName)) && database$databaseId %in% names(cohortTableName)) {
        resolved <- cohortTableName[[database$databaseId]]
        if (is.character(resolved) && length(resolved) == 1 && nzchar(resolved)) {
          return(resolved)
        }
      }

      if (is.null(names(cohortTableName)) && length(cohortTableName) == databaseCount) {
        resolved <- cohortTableName[[dbUp]]
        if (is.character(resolved) && length(resolved) == 1 && nzchar(resolved)) {
          return(resolved)
        }
      }
    }

    stop(
      "cohortTableName must be one of: NULL, one table name, a named character/list keyed by databaseId, ",
      "or an unnamed character/list with one entry per database. Could not resolve value for databaseId=",
      database$databaseId
    )
  }

  if (!isTRUE(useExisting)) {
    ithCheckConceptSetsReadyForScoring(conditionSpecs, outputDirectory)
  }

  if (generateCohorts) {
    cohortIds <- unique(as.integer(unlist(lapply(conditionSpecs, function(x) x$randomCohortDefinitionId))))

    if (any(is.na(cohortIds))) {
      stop("Each condition spec must provide a valid integer randomCohortDefinitionId for scoring")
    }

    if (is.null(cohortDefinitionSet) && is.null(webapiBaseUrl)) {
      stop("When generateCohorts = TRUE, provide cohortDefinitionSet or webapiBaseUrl")
    }

    cohortDefinitionSetToUse <- cohortDefinitionSet
    if (is.null(cohortDefinitionSetToUse)) {
      cohortDefinitionSetToUse <- ithGetWebapiCohortDefinitionSet(
        cohortIds = cohortIds,
        webapiBaseUrl = webapiBaseUrl,
        verbose = verbose
      )
    }

    generationJobs <- vector("list", length(databases))
    for (dbUp in seq_along(databases)) {
      resolvedCohortTable <- resolveCohortTableName(
        cohortTableName = cohortTableName,
        database = databases[[dbUp]],
        dbUp = dbUp,
        databaseCount = length(databases)
      )

      ithMaybeMessage(
        verbose,
        "Generating cohorts for database '",
        databases[[dbUp]]$databaseId,
        "' using cohort table '",
        if (is.null(resolvedCohortTable)) ithDefaultCohortTable(databases[[dbUp]]) else resolvedCohortTable,
        "'."
      )

      generationJobs[[dbUp]] <- list(
        database = databases[[dbUp]],
        cohortDefinitionSet = cohortDefinitionSetToUse,
        cohortTable = resolvedCohortTable
      )
    }
    ithGenerateCohortJobs(
      generationJobs,
      workers = cohortWorkers,
      stopOnError = TRUE,
      incremental = TRUE,
      createTables = TRUE,
      verbose = verbose
    )
  }

  if (!isTRUE(useExisting)) {
    ithCheckRandomCohortsReadyForScoring(
      conditionSpecs,
      databases,
      cohortTables = lapply(seq_along(databases), function(dbUp) {
        resolveCohortTableName(cohortTableName, databases[[dbUp]], dbUp, length(databases))
      })
    )
  }

  scoringWorkers <- max(1L, as.integer(ithCoalesce(scoringWorkers, 1L)[[1]]))
  scoringWorkNeeded <- !isTRUE(useExisting)
  if (!scoringWorkNeeded) {
    scoringWorkNeeded <- any(vapply(seq_along(conditionSpecs), function(conditionUp) {
      randomId <- suppressWarnings(as.integer(conditionSpecs[[conditionUp]]$randomCohortDefinitionId[[1]]))
      any(vapply(databases, function(database) {
        !ithScoringArtifactsComplete(
          file.path(outputDirectory, "scoring", conditionSpecs[[conditionUp]]$rubricName, database$databaseId),
          randomId
        )
      }, logical(1)))
    }, logical(1)))
  }
  cluster <- NULL
  if (scoringWorkers > 1L && scoringWorkNeeded) {
    ithMaybeMessage(verbose, "Starting ", scoringWorkers, " scoring worker(s).")
    cluster <- ithStartWorkerCluster(scoringWorkers)
    on.exit(ithStopWorkerCluster(cluster), add = TRUE)
  }

  allResults <- list()
  for (conditionUp in seq_along(conditionSpecs)) {
    for (dbUp in seq_along(databases)) {
      key <- paste(conditionSpecs[[conditionUp]]$rubricName, databases[[dbUp]]$databaseId, sep = "::")
      resolvedCohortTable <- resolveCohortTableName(
        cohortTableName = cohortTableName,
        database = databases[[dbUp]],
        dbUp = dbUp,
        databaseCount = length(databases)
      )
      ithMaybeMessage(verbose, "Running scoring job: ", key)
      scoringResult <- ithScoreConditionDatabase(
        condition = conditionSpecs[[conditionUp]],
        database = databases[[dbUp]],
        outputDirectory = outputDirectory,
        seedLibraryPath = seedLibraryPath,
        conceptLibraryVersion = as.integer(conceptLibraryVersion[[1]]),
        cohortTableName = resolvedCohortTable,
        useExisting = useExisting,
        llmClient = llmClient,
        promptDir = promptDir,
        cluster = cluster,
        verbose = verbose
      )

      scoringSummary <- ithSummarizeScoringResults(
        conditionName = conditionSpecs[[conditionUp]]$rubricName,
        databaseId = databases[[dbUp]]$databaseId,
        randomCohortDefinitionId = conditionSpecs[[conditionUp]]$randomCohortDefinitionId,
        outputDirectory = outputDirectory,
        topN = 10,
        saveSummary = TRUE,
        verbose = FALSE
      )

      if (!is.null(scoringSummary$savedFiles$summaryPath) || !is.null(scoringSummary$savedFiles$overviewPath)) {
        ithMaybeMessage(
          verbose,
          "Saved scoring summary files: ",
          scoringSummary$savedFiles$summaryPath,
          " ; ",
          scoringSummary$savedFiles$overviewPath
        )
      }

      scoringResult$scoringSummary <- scoringSummary
      allResults[[key]] <- scoringResult
    }
  }

  ithMaybeMessage(verbose, "Subject scoring workflow completed.")

  allResults
}

ithSummarizeScoringResults <- function(conditionName,
                                       databaseId,
                                       randomCohortDefinitionId,
                                       outputDirectory,
                                       topN = 10,
                                       saveSummary = TRUE,
                                       verbose = TRUE) {
  randomCohortDefinitionId <- suppressWarnings(as.integer(
    if (is.null(randomCohortDefinitionId) || length(randomCohortDefinitionId) == 0) NA else randomCohortDefinitionId[[1]]
  ))
  if (length(randomCohortDefinitionId) == 0 || is.na(randomCohortDefinitionId)) {
    stop("randomCohortDefinitionId must be a valid integer")
  }

  cohortDir <- file.path(outputDirectory, "scoring", conditionName, databaseId)
  if (!dir.exists(cohortDir)) {
    stop("Scoring output directory not found: ", cohortDir)
  }

  sumFileName <- paste0("sumCohortScores_Cohort", randomCohortDefinitionId, ".rds")
  finalFileName <- paste0("finalScores_Cohort", randomCohortDefinitionId, ".rds")
  subjectCountFileName <- paste0("subjectCount_Cohort", randomCohortDefinitionId, ".csv")
  fullScoringFileName <- paste0("fullScoringFile_Cohort", randomCohortDefinitionId, ".rds")

  sumFileVersion <- ithFindLatestVersion(cohortDir, sumFileName)
  finalFileVersion <- ithFindLatestVersion(cohortDir, finalFileName)
  subjectCountFileVersion <- ithFindLatestVersion(cohortDir, subjectCountFileName)
  fullScoringFileVersion <- ithFindLatestVersion(cohortDir, fullScoringFileName)

  if (is.null(sumFileVersion) || is.null(finalFileVersion) || is.null(subjectCountFileVersion) || is.null(fullScoringFileVersion)) {
    stop(
      "Missing one or more scoring artifacts in ", cohortDir,
      ". Expected latest versions of: ",
      sumFileName, ", ", finalFileName, ", ", subjectCountFileName, ", ", fullScoringFileName
    )
  }

  sumScores <- ithLoadLatest(cohortDir, sumFileName)
  finalScores <- ithLoadLatest(cohortDir, finalFileName)
  subjectCounts <- ithLoadLatest(cohortDir, subjectCountFileName)
  fullScoring <- ithLoadLatest(cohortDir, fullScoringFileName)

  finalCaseSummary <- as.data.frame(table(finalScores$case), stringsAsFactors = FALSE)
  names(finalCaseSummary) <- c("case", "count")
  finalCaseSummary$case <- as.integer(as.character(finalCaseSummary$case))
  finalCaseSummary$proportion <- finalCaseSummary$count / sum(finalCaseSummary$count)
  finalCaseSummary <- finalCaseSummary[order(finalCaseSummary$case), ]

  rubricCaseSummary <- data.frame()
  if (all(c("rubricId", "case") %in% names(sumScores))) {
    rubricIds <- sort(unique(as.character(sumScores$rubricId)))
    rubricCaseSummary <- do.call(
      rbind,
      lapply(
        rubricIds,
        function(rubricId) {
          caseValues <- sumScores$case[as.character(sumScores$rubricId) == rubricId]
          data.frame(
            rubricId = rubricId,
            n = length(caseValues),
            positive = sum(caseValues == 1, na.rm = TRUE),
            uncertain = sum(caseValues == 0, na.rm = TRUE),
            negative = sum(caseValues == -1, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }
      )
    )
  }

  topCriteriaHits <- data.frame()
  if (all(c("rubricId", "ruleId", "score", "subjectCount", "criteria") %in% names(subjectCounts))) {
    topCriteriaHits <- subjectCounts[order(-subjectCounts$subjectCount), c("rubricId", "ruleId", "score", "subjectCount", "criteria")]
    topCriteriaHits <- utils::head(topCriteriaHits, topN)
  }

  ruleRowCounts <- data.frame()
  if (all(c("rubricId", "ruleId") %in% names(fullScoring))) {
    ruleRowCounts <- as.data.frame(table(fullScoring$rubricId, fullScoring$ruleId), stringsAsFactors = FALSE)
    names(ruleRowCounts) <- c("rubricId", "ruleId", "rows")
    ruleRowCounts <- ruleRowCounts[ruleRowCounts$rows > 0, ]
    ruleRowCounts <- ruleRowCounts[order(-ruleRowCounts$rows), ]
  }

  totalSubjects <- nrow(finalScores)
  positiveSubjects <- if (any(finalCaseSummary$case == 1)) finalCaseSummary$count[finalCaseSummary$case == 1][[1]] else 0
  uncertainSubjects <- if (any(finalCaseSummary$case == 0)) finalCaseSummary$count[finalCaseSummary$case == 0][[1]] else 0
  negativeSubjects <- if (any(finalCaseSummary$case == -1)) finalCaseSummary$count[finalCaseSummary$case == -1][[1]] else 0

  summaryOverview <- data.frame(
    conditionName = conditionName,
    databaseId = databaseId,
    randomCohortDefinitionId = randomCohortDefinitionId,
    totalSubjects = totalSubjects,
    positiveSubjects = positiveSubjects,
    uncertainSubjects = uncertainSubjects,
    negativeSubjects = negativeSubjects,
    positiveProportion = if (totalSubjects == 0) 0 else positiveSubjects / totalSubjects,
    uncertainProportion = if (totalSubjects == 0) 0 else uncertainSubjects / totalSubjects,
    negativeProportion = if (totalSubjects == 0) 0 else negativeSubjects / totalSubjects,
    stringsAsFactors = FALSE
  )

  summaryObject <- list(
    summaryOverview = summaryOverview,
    finalCaseSummary = finalCaseSummary,
    rubricCaseSummary = rubricCaseSummary,
    topCriteriaHits = topCriteriaHits,
    ruleRowCounts = ruleRowCounts,
    artifactVersions = list(
      sumScores = sumFileVersion,
      finalScores = finalFileVersion,
      subjectCounts = subjectCountFileVersion,
      fullScoring = fullScoringFileVersion
    ),
    savedFiles = list(
      summaryPath = NULL,
      overviewPath = NULL
    )
  )

  if (isTRUE(saveSummary)) {
    summaryPath <- ithSaveVersioned(
      saveDirectory = cohortDir,
      objectToSave = summaryObject,
      fileName = paste0("scoringResultsSummary_Cohort", randomCohortDefinitionId, ".rds")
    )
    overviewPath <- ithSaveVersioned(
      saveDirectory = cohortDir,
      objectToSave = summaryOverview,
      fileName = paste0("scoringResultsOverview_Cohort", randomCohortDefinitionId, ".csv")
    )
    summaryObject$savedFiles$summaryPath <- summaryPath
    summaryObject$savedFiles$overviewPath <- overviewPath

    if (isTRUE(verbose)) {
      message("Saved scoring summary files: ", summaryPath, " ; ", overviewPath)
    }
  }

  summaryObject
}

ithScoreFullScoringFile <- function(fullScoringFile,
                                    excludeNegativeScores = FALSE,
                                    yesScoreOverride = NULL,
                                    noScoreOverride = NULL,
                                    saveAdjustedOutput = TRUE,
                                    outputDirectory = NULL,
                                    verbose = TRUE,
                                    adjustableRuleId = NULL,
                                    dxRuleScoreOverride = NULL,
                                    dxRuleScoreAdjustment = NULL,
                                    fullScoringData = NULL) {
  if (is.null(fullScoringFile) || !is.character(fullScoringFile) || length(fullScoringFile) != 1 || !nzchar(fullScoringFile)) {
    stop("fullScoringFile must be a non-empty file path")
  }

  resolvedFullScoringFile <- as.character(fullScoringFile[[1]])
  # fullScoringData is the already-loaded content of fullScoringFile, so the file is not read again.
  if (is.null(fullScoringData)) {
    resolvedDirectory <- dirname(resolvedFullScoringFile)
    resolvedBaseName <- basename(resolvedFullScoringFile)
    latestFullScoringVersion <- NULL
    if (dir.exists(resolvedDirectory)) {
      latestFullScoringVersion <- ithFindLatestVersion(resolvedDirectory, resolvedBaseName)
    }
    if (!is.null(latestFullScoringVersion)) {
      resolvedFullScoringFile <- file.path(resolvedDirectory, latestFullScoringVersion)
    }

    if (!file.exists(resolvedFullScoringFile)) {
      stop("fullScoringFile not found: ", resolvedFullScoringFile)
    }

    fullScoring <- readRDS(resolvedFullScoringFile)
  } else {
    fullScoring <- data.table::copy(fullScoringData)
  }
  fullScoring <- data.table::as.data.table(fullScoring)

  requiredColumns <- c("cohortDefinitionId", "subjectId", "cohortStartDate", "rubricId", "score")
  missingColumns <- setdiff(requiredColumns, names(fullScoring))
  if (length(missingColumns) > 0) {
    stop("fullScoringFile is missing required columns: ", paste(missingColumns, collapse = ", "))
  }

  inputRowCount <- nrow(fullScoring)
  if (inputRowCount == 0) {
    stop("fullScoringFile contains zero rows")
  }

  fullScoring$rubricId <- as.character(fullScoring$rubricId)
  fullScoring$score <- as.numeric(fullScoring$score)

  adjustableRuleId <- toupper(trimws(as.character(unlist(adjustableRuleId, use.names = FALSE))))
  if (length(adjustableRuleId) > 0L && (!is.null(dxRuleScoreOverride) || !is.null(dxRuleScoreAdjustment))) {
    if (!is.null(dxRuleScoreOverride) && !is.null(dxRuleScoreAdjustment)) {
      stop("Set either dxRuleScoreOverride or dxRuleScoreAdjustment, not both")
    }
    if (!("ruleId" %in% names(fullScoring))) {
      stop("fullScoringFile has no ruleId column, so adjustableRuleId scores cannot be changed")
    }
    ruleMatch <- toupper(trimws(as.character(fullScoring$ruleId))) %in% adjustableRuleId
    missingRuleIds <- setdiff(adjustableRuleId, toupper(trimws(as.character(fullScoring$ruleId))))
    if (length(missingRuleIds) > 0L) {
      ithMaybeMessage(verbose, "No fullScoringFile rows for adjustable rule(s): ", paste(missingRuleIds, collapse = ", "))
    }
    if (!is.null(dxRuleScoreOverride)) {
      fullScoring$score[ruleMatch] <- as.numeric(dxRuleScoreOverride)
    } else {
      fullScoring$score[ruleMatch] <- fullScoring$score[ruleMatch] + as.numeric(dxRuleScoreAdjustment)
    }
    if (any(fullScoring$score[ruleMatch] < 0, na.rm = TRUE)) {
      stop("Adjusted adjustableRuleId scores must not be negative")
    }
    ithMaybeMessage(
      verbose,
      "Adjusted ", sum(ruleMatch), " fullScoringFile row(s) for rule(s) ", paste(adjustableRuleId, collapse = ", "),
      if (!is.null(dxRuleScoreOverride)) paste0(" to ", dxRuleScoreOverride, " points") else paste0(" by ", dxRuleScoreAdjustment, " points")
    )
  }

  scoringRows <- fullScoring
  if (isTRUE(excludeNegativeScores)) {
    # The fixed AE1 zero-occurrence penalty is always applied, even when other negatives are excluded.
    scoringRows <- scoringRows[is.na(score) | score >= 0 | toupper(trimws(as.character(ruleId))) == "AE1"]
  }

  usedRowCount <- nrow(scoringRows)
  if (usedRowCount == 0) {
    stop("No scoring rows available after applying excludeNegativeScores filter")
  }

  sumScoresPre <- scoringRows[, .(total = sum(score, na.rm = TRUE)), by = .(cohortDefinitionId, subjectId, cohortStartDate, rubricId)]
  rubricIds <- sort(unique(as.character(sumScoresPre$rubricId)))

  resolveThresholdOverride <- function(overrideValues, rubricIds, label) {
    if (is.null(overrideValues)) {
      return(rep(NA_real_, length(rubricIds)))
    }

    if (!is.numeric(overrideValues)) {
      stop(label, " must be numeric when provided")
    }

    if (length(overrideValues) == 1) {
      return(rep(as.numeric(overrideValues[[1]]), length(rubricIds)))
    }

    overrideNames <- names(overrideValues)
    if (!is.null(overrideNames) && any(nzchar(overrideNames))) {
      overrideMap <- setNames(as.numeric(overrideValues), as.character(overrideNames))
      resolved <- overrideMap[rubricIds]
      missingRubricIds <- rubricIds[is.na(resolved)]
      if (length(missingRubricIds) > 0) {
        stop(
          label,
          " named overrides are missing rubricId values: ",
          paste(missingRubricIds, collapse = ", ")
        )
      }
      return(as.numeric(resolved))
    }

    if (length(overrideValues) == length(rubricIds)) {
      return(as.numeric(overrideValues))
    }

    stop(
      label,
      " must be one numeric value, a named numeric vector keyed by rubricId, ",
      "or an unnamed numeric vector with one value per rubricId"
    )
  }

  thresholdTable <- data.frame(
    rubricId = rubricIds,
    yesScore = resolveThresholdOverride(yesScoreOverride, rubricIds, "yesScoreOverride"),
    noScore = resolveThresholdOverride(noScoreOverride, rubricIds, "noScoreOverride"),
    stringsAsFactors = FALSE
  )

  if (any(is.na(thresholdTable$yesScore)) || any(is.na(thresholdTable$noScore))) {
    if (all(c("yesScore", "noScore") %in% names(fullScoring))) {
      fileThresholds <- unique(
        fullScoring[, .(
          rubricId = as.character(rubricId),
          yesScore = suppressWarnings(as.numeric(yesScore)),
          noScore = suppressWarnings(as.numeric(noScore))
        )]
      )

      thresholdCounts <- fileThresholds[, .N, by = .(rubricId)]
      conflictingRubricIds <- thresholdCounts$rubricId[thresholdCounts$N > 1]
      if (length(conflictingRubricIds) > 0) {
        stop(
          "fullScoringFile contains conflicting yesScore/noScore threshold rows for rubricId: ",
          paste(conflictingRubricIds, collapse = ", ")
        )
      }

      thresholdTable <- merge(
        thresholdTable,
        fileThresholds,
        by = "rubricId",
        all.x = TRUE,
        suffixes = c("", "FromFile")
      )

      thresholdTable$yesScore[is.na(thresholdTable$yesScore)] <- thresholdTable$yesScoreFromFile[is.na(thresholdTable$yesScore)]
      thresholdTable$noScore[is.na(thresholdTable$noScore)] <- thresholdTable$noScoreFromFile[is.na(thresholdTable$noScore)]
      thresholdTable$yesScoreFromFile <- NULL
      thresholdTable$noScoreFromFile <- NULL
    }
  }

  missingThresholdRubricIds <- thresholdTable$rubricId[is.na(thresholdTable$yesScore) | is.na(thresholdTable$noScore)]
  if (length(missingThresholdRubricIds) > 0) {
    stop(
      "Missing yes/no thresholds for rubricId: ",
      paste(missingThresholdRubricIds, collapse = ", "),
      ". Provide yesScoreOverride and noScoreOverride, or include yesScore/noScore columns in fullScoringFile."
    )
  }

  sumScores <- merge(sumScoresPre, thresholdTable, by = "rubricId", all.x = TRUE, sort = FALSE)
  sumScores$case <- -1L
  sumScores$case[sumScores$total >= sumScores$yesScore] <- 1L
  sumScores$case[sumScores$total > sumScores$noScore & sumScores$total < sumScores$yesScore] <- 0L

  finalScores <- sumScores[, .(total = sum(case, na.rm = TRUE)), by = .(cohortDefinitionId, subjectId, cohortStartDate)]
  finalScores$case <- 0L
  finalScores$case[finalScores$total > 0] <- 1L
  finalScores$case[finalScores$total < 0] <- -1L

  ithMaybeMessage(
    verbose,
    "Scored fullScoringFile rows=", inputRowCount,
    "; usedRows=", usedRowCount,
    "; rubricCount=", length(rubricIds),
    "; excludeNegativeScores=", isTRUE(excludeNegativeScores)
  )

  savedAdjustedSumPath <- NULL
  savedAdjustedFinalPath <- NULL
  if (isTRUE(saveAdjustedOutput)) {
    outputDirectoryResolved <- if (is.null(outputDirectory) || !nzchar(as.character(outputDirectory[[1]]))) {
      dirname(resolvedFullScoringFile)
    } else {
      as.character(outputDirectory[[1]])
    }

    if (!dir.exists(outputDirectoryResolved)) {
      dir.create(outputDirectoryResolved, recursive = TRUE, showWarnings = FALSE)
    }

    scoringBaseName <- basename(resolvedFullScoringFile)
    scoringBaseName <- sub("\\.rds$", "", scoringBaseName, ignore.case = TRUE)
    scoringBaseName <- sub("_V[0-9]+$", "", scoringBaseName)
    scoringSuffix <- sub("^fullScoringFile_", "", scoringBaseName)
    if (identical(scoringSuffix, scoringBaseName) || !nzchar(scoringSuffix)) {
      scoringSuffix <- scoringBaseName
    }

    savedAdjustedSumPath <- ithSaveVersioned(
      saveDirectory = outputDirectoryResolved,
      objectToSave = sumScores,
      fileName = paste0("sumCohortScoresAdjusted_", scoringSuffix, ".rds")
    )

    savedAdjustedFinalPath <- ithSaveVersioned(
      saveDirectory = outputDirectoryResolved,
      objectToSave = finalScores,
      fileName = paste0("finalScoresAdjusted_", scoringSuffix, ".rds")
    )
  }

  list(
    sumCohortScores = sumScores,
    finalScores = finalScores,
    thresholdTable = thresholdTable,
    savedFiles = list(
      sumCohortScoresAdjustedPath = savedAdjustedSumPath,
      finalScoresAdjustedPath = savedAdjustedFinalPath
    ),
    metadata = list(
      fullScoringFile = resolvedFullScoringFile,
      excludeNegativeScores = isTRUE(excludeNegativeScores),
      inputRowCount = inputRowCount,
      usedRowCount = usedRowCount,
      rubricCount = length(rubricIds),
      saveAdjustedOutput = isTRUE(saveAdjustedOutput)
    )
  )
}

# Only valid when the rules and criteria match the scoring run; otherwise rerun scoring.
ithRescoreFromRuleScores <- function(sumScores,
                                     cohortDir,
                                     conditionName,
                                     outputDirectory,
                                     randomCohortDefinitionId) {
  fullScoringName <- paste0("fullScoringFile_Cohort", randomCohortDefinitionId, ".rds")
  subjectCountName <- paste0("subjectCount_Cohort", randomCohortDefinitionId, ".csv")
  fullScoring <- ithLoadLatest(cohortDir, fullScoringName)
  subjectCounts <- ithLoadLatest(cohortDir, subjectCountName)
  if (is.null(fullScoring) || is.null(subjectCounts)) {
    stop("recomputeFromRuleScores requires ", fullScoringName, " and ", subjectCountName, " in: ", cohortDir)
  }

  fullScoring <- as.data.frame(fullScoring, stringsAsFactors = FALSE)
  missingColumns <- setdiff(c("subjectId", "cohortStartDate", "rubricId", "ruleId", "score"), names(fullScoring))
  if (length(missingColumns) > 0) {
    stop("Full scoring file is missing required columns: ", paste(missingColumns, collapse = ", "))
  }

  shortRubric <- ithLoadLatestShortRubric(conditionName, outputDirectory)
  ruleKey <- function(rubric, ruleId) paste(as.character(rubric), as.character(ruleId), sep = "||")
  currentKeys <- ruleKey(shortRubric$rubric, shortRubric$ruleId)
  scoredKeys <- ruleKey(subjectCounts$rubricId, subjectCounts$ruleId)

  if (anyDuplicated(currentKeys) > 0) {
    stop("Short rubric has duplicate rubric/ruleId values; rerun scoring instead of recomputeFromRuleScores")
  }

  addedRules <- setdiff(currentKeys, scoredKeys)
  removedRules <- setdiff(scoredKeys, currentKeys)
  if (length(addedRules) > 0 || length(removedRules) > 0) {
    stop(
      "Rules differ from the scoring run (added: ", paste(addedRules, collapse = ", "),
      "; removed: ", paste(removedRules, collapse = ", "),
      "). recomputeFromRuleScores only supports score and threshold changes; rerun scoring."
    )
  }

  scoredCriteria <- subjectCounts$criteria[match(currentKeys, scoredKeys)]
  changedCriteria <- currentKeys[which(trimws(as.character(shortRubric$criteria)) != trimws(as.character(scoredCriteria)))]
  if (length(changedCriteria) > 0) {
    stop("Criteria changed since the scoring run for: ", paste(changedCriteria, collapse = ", "), ". Rerun scoring.")
  }

  newScoreByKey <- stats::setNames(as.numeric(shortRubric$score), currentKeys)
  fullScoring$score <- unname(newScoreByKey[ruleKey(fullScoring$rubricId, fullScoring$ruleId)])
  if (anyNA(fullScoring$score)) {
    stop("Rule hits reference rules missing from the short rubric; rerun scoring")
  }
  newCountScores <- unname(newScoreByKey[scoredKeys])
  ruleScoresChanged <- !isTRUE(all.equal(as.numeric(subjectCounts$score), newCountScores))
  subjectCounts$score <- newCountScores

  sumScores <- data.table::as.data.table(sumScores)
  newTotal <- rep(0, nrow(sumScores))
  if (nrow(fullScoring) > 0) {
    hitKey <- paste(as.double(fullScoring$subjectId), as.character(as.Date(fullScoring$cohortStartDate)), as.character(fullScoring$rubricId), sep = "||")
    sumKey <- paste(as.double(sumScores$subjectId), as.character(as.Date(sumScores$cohortStartDate)), as.character(sumScores$rubricId), sep = "||")
    hitTotals <- rowsum(fullScoring$score, hitKey)

    unmatched <- setdiff(rownames(hitTotals), sumKey)
    if (length(unmatched) > 0) {
      stop(length(unmatched), " subject/rubric group(s) in the full scoring file are missing from sumCohortScores; rerun scoring.")
    }

    hitIndex <- match(sumKey, rownames(hitTotals))
    newTotal[!is.na(hitIndex)] <- hitTotals[hitIndex[!is.na(hitIndex)], 1]
  }
  sumScores$total <- newTotal

  list(
    sumScores = sumScores,
    fullScoring = fullScoring,
    subjectCounts = subjectCounts,
    ruleScoresChanged = ruleScoresChanged
  )
}

ithScoringUnchanged <- function(before, after) {
  needed <- c("total", "yesScore", "noScore", "case")
  keyColumns <- c("subjectId", "cohortStartDate", "rubricId")
  if (!all(c(needed, keyColumns) %in% names(before)) || nrow(before) != nrow(after)) {
    return(FALSE)
  }

  keyOf <- function(x) paste(as.double(x$subjectId), as.character(as.Date(x$cohortStartDate)), as.character(x$rubricId), sep = "||")
  index <- match(keyOf(after), keyOf(before))
  if (anyNA(index)) {
    return(FALSE)
  }

  all(vapply(needed, function(column) {
    isTRUE(all.equal(as.numeric(after[[column]]), as.numeric(before[[column]])[index]))
  }, logical(1)))
}

ithBackfillScoringCaseLabels <- function(conditionName,
                                         databaseId,
                                         randomCohortDefinitionId,
                                         outputDirectory,
                                         rubricPath = NULL,
                                         saveSummary = TRUE,
                                         verbose = TRUE,
                                         recomputeFromRuleScores = FALSE,
                                         skipIfUnchanged = FALSE) {
  randomCohortDefinitionId <- suppressWarnings(as.integer(
    if (is.null(randomCohortDefinitionId) || length(randomCohortDefinitionId) == 0) NA else randomCohortDefinitionId[[1]]
  ))
  if (length(randomCohortDefinitionId) == 0 || is.na(randomCohortDefinitionId)) {
    stop("randomCohortDefinitionId must be a valid integer")
  }

  cohortDir <- file.path(outputDirectory, "scoring", conditionName, databaseId)
  if (!dir.exists(cohortDir)) {
    stop("Scoring output directory not found: ", cohortDir)
  }

  sumFileName <- paste0("sumCohortScores_Cohort", randomCohortDefinitionId, ".rds")
  sumScores <- ithLoadLatest(cohortDir, sumFileName)
  if (is.null(sumScores)) {
    stop("Could not find scoring artifact: ", sumFileName)
  }

  resolveRubricObject <- function() {
    if (is.null(rubricPath) || !nzchar(as.character(rubricPath[[1]]))) {
      return(ithLoadLatest(file.path(outputDirectory, conditionName), "rubricList.rds"))
    }

    inputPath <- as.character(rubricPath[[1]])
    if (dir.exists(inputPath)) {
      latest <- ithFindLatestVersion(inputPath, "rubricList.rds")
      resolved <- if (!is.null(latest)) file.path(inputPath, latest) else file.path(inputPath, "rubricList.rds")
      if (!file.exists(resolved)) {
        stop("rubricPath directory does not contain rubricList.rds: ", inputPath)
      }
      return(readRDS(resolved))
    }

    parentDirectory <- dirname(inputPath)
    baseName <- basename(inputPath)
    if (dir.exists(parentDirectory)) {
      latest <- ithFindLatestVersion(parentDirectory, baseName)
      if (!is.null(latest)) {
        return(readRDS(file.path(parentDirectory, latest)))
      }
    }

    if (!file.exists(inputPath)) {
      stop("rubricPath does not exist: ", inputPath)
    }

    readRDS(inputPath)
  }

  rubricObject <- resolveRubricObject()
  if (is.null(rubricObject)) {
    stop("Could not load rubric object for condition: ", conditionName)
  }

  fullRubric <- if (is.list(rubricObject) && !is.null(rubricObject$rubricList)) rubricObject$rubricList else rubricObject
  if (!is.list(fullRubric) || length(fullRubric) == 0) {
    stop("Rubric object does not contain a valid rubric list")
  }

  getRubricIdentifier <- function(rubricObj) {
    if (!is.null(rubricObj$rubricId) && length(rubricObj$rubricId) > 0) {
      return(as.character(rubricObj$rubricId[[1]]))
    }
    if (!is.null(rubricObj$rubric_id) && length(rubricObj$rubric_id) > 0) {
      return(as.character(rubricObj$rubric_id[[1]]))
    }
    NA_character_
  }

  getDecisionThresholds <- function(rubricObj) {
    thresholds <- NULL
    if (!is.null(rubricObj$decisionThresholds)) {
      thresholds <- rubricObj$decisionThresholds
    } else if (!is.null(rubricObj$decision_thresholds)) {
      thresholds <- rubricObj$decision_thresholds
    }
    if (is.null(thresholds)) {
      return(list(yes = NA_real_, no = NA_real_))
    }
    yesScore <- if (!is.null(thresholds$yes) && length(thresholds$yes) > 0) as.numeric(thresholds$yes[[1]]) else NA_real_
    noScore <- if (!is.null(thresholds$no) && length(thresholds$no) > 0) as.numeric(thresholds$no[[1]]) else NA_real_
    list(yes = yesScore, no = noScore)
  }

  allScoring <- NULL
  for (rubricUp in seq_along(fullRubric)) {
    rubricIdValue <- getRubricIdentifier(fullRubric[[rubricUp]])
    thresholds <- getDecisionThresholds(fullRubric[[rubricUp]])
    if (is.na(rubricIdValue) || !nzchar(rubricIdValue)) {
      next
    }
    if (is.na(thresholds$yes) || is.na(thresholds$no)) {
      next
    }
    allScoring <- rbind(
      allScoring,
      data.frame(
        rubricId = rubricIdValue,
        yesScore = thresholds$yes,
        noScore = thresholds$no,
        stringsAsFactors = FALSE
      )
    )
  }

  if (is.null(allScoring) || nrow(allScoring) == 0) {
    stop("No valid rubric thresholds available; unable to backfill subject case labels")
  }

  allScoring <- unique(data.table::as.data.table(allScoring))
  sumScores <- data.table::as.data.table(sumScores)
  sumScores$rubricId <- as.character(sumScores$rubricId)
  allScoring$rubricId <- as.character(allScoring$rubricId)

  originalSumScores <- sumScores[, intersect(
    c("subjectId", "cohortStartDate", "rubricId", "total", "yesScore", "noScore", "case"),
    names(sumScores)
  ), with = FALSE]

  rescored <- NULL
  if (isTRUE(recomputeFromRuleScores)) {
    rescored <- ithRescoreFromRuleScores(
      sumScores = sumScores,
      cohortDir = cohortDir,
      conditionName = conditionName,
      outputDirectory = outputDirectory,
      randomCohortDefinitionId = randomCohortDefinitionId
    )
    sumScores <- rescored$sumScores
    ithMaybeMessage(verbose, "Recomputed rubric totals from saved rule hits using the latest short rubric scores.")
  }

  existingThresholdCols <- intersect(c("yesScore", "noScore"), names(sumScores))
  if (length(existingThresholdCols) > 0) {
    sumScores[, (existingThresholdCols) := NULL]
  }

  sumScores <- merge(sumScores, allScoring, by = "rubricId", all.x = TRUE, sort = FALSE)

  sumScores$case <- -1L
  sumScores$case[!is.na(sumScores$yesScore) & sumScores$total >= sumScores$yesScore] <- 1L
  sumScores$case[!is.na(sumScores$yesScore) & !is.na(sumScores$noScore) &
    sumScores$total > sumScores$noScore & sumScores$total < sumScores$yesScore] <- 0L

  finalScores <- sumScores[, .(total = sum(case, na.rm = TRUE)), by = .(cohortDefinitionId, subjectId, cohortStartDate)]
  finalScores$case <- 0L
  finalScores$case[finalScores$total > 0] <- 1L
  finalScores$case[finalScores$total < 0] <- -1L

  if (isTRUE(skipIfUnchanged) && !isTRUE(rescored$ruleScoresChanged) && ithScoringUnchanged(originalSumScores, sumScores)) {
    ithMaybeMessage(
      verbose,
      "No changes found for condition '", conditionName, "' database '", databaseId,
      "' randomCohortDefinitionId=", randomCohortDefinitionId, "; skipped saving new versions."
    )
    return(invisible(list(
      skipped = TRUE,
      sumScoresPath = NULL,
      finalScoresPath = NULL,
      fullScoringPath = NULL,
      subjectCountPath = NULL,
      summaryPath = NULL,
      overviewPath = NULL,
      positiveSubjects = sum(finalScores$case == 1, na.rm = TRUE),
      uncertainSubjects = sum(finalScores$case == 0, na.rm = TRUE),
      negativeSubjects = sum(finalScores$case == -1, na.rm = TRUE)
    )))
  }

  sumPath <- ithSaveVersioned(
    saveDirectory = cohortDir,
    objectToSave = sumScores,
    fileName = sumFileName
  )

  finalPath <- ithSaveVersioned(
    saveDirectory = cohortDir,
    objectToSave = finalScores,
    fileName = paste0("finalScores_Cohort", randomCohortDefinitionId, ".rds")
  )

  # Keep rule scores in these artifacts consistent with the recomputed totals.
  fullScoringPath <- NULL
  subjectCountPath <- NULL
  if (!is.null(rescored)) {
    fullScoringPath <- ithSaveVersioned(
      saveDirectory = cohortDir,
      objectToSave = rescored$fullScoring,
      fileName = paste0("fullScoringFile_Cohort", randomCohortDefinitionId, ".rds")
    )
    subjectCountPath <- ithSaveVersioned(
      saveDirectory = cohortDir,
      objectToSave = rescored$subjectCounts,
      fileName = paste0("subjectCount_Cohort", randomCohortDefinitionId, ".csv")
    )
  }

  summaryResult <- NULL
  if (isTRUE(saveSummary)) {
    summaryResult <- ithSummarizeScoringResults(
      conditionName = conditionName,
      databaseId = databaseId,
      randomCohortDefinitionId = randomCohortDefinitionId,
      outputDirectory = outputDirectory,
      topN = 10,
      saveSummary = TRUE,
      verbose = verbose
    )
  }

  positiveCount <- sum(finalScores$case == 1, na.rm = TRUE)
  uncertainCount <- sum(finalScores$case == 0, na.rm = TRUE)
  negativeCount <- sum(finalScores$case == -1, na.rm = TRUE)

  ithMaybeMessage(
    verbose,
    "Backfilled scoring case labels for condition '",
    conditionName,
    "' database '",
    databaseId,
    "' randomCohortDefinitionId=",
    randomCohortDefinitionId,
    ". Positives=",
    positiveCount,
    ", uncertain=",
    uncertainCount,
    ", negatives=",
    negativeCount,
    "."
  )

  invisible(list(
    skipped = FALSE,
    sumScoresPath = sumPath,
    finalScoresPath = finalPath,
    fullScoringPath = fullScoringPath,
    subjectCountPath = subjectCountPath,
    summaryPath = if (!is.null(summaryResult)) summaryResult$savedFiles$summaryPath else NULL,
    overviewPath = if (!is.null(summaryResult)) summaryResult$savedFiles$overviewPath else NULL,
    positiveSubjects = positiveCount,
    uncertainSubjects = uncertainCount,
    negativeSubjects = negativeCount
  ))
}

#' Find latest versioned file by base name
#'
#' @param saveDirectory Directory containing versioned files.
#' @param fileName Base file name pattern.
#'
#' @return Latest matching file name or `NULL`.
#' @examples
#' \dontrun{
#' latest <- findLatestFileVersion("P:/llm/rubrics/GERD", "rubricList.rds")
#' }
#' @export
findLatestFileVersion <- function(saveDirectory, fileName) {
  ithFindLatestVersion(saveDirectory, fileName)
}

#' Save object as a versioned file
#'
#' @param saveDirectory Directory to save into.
#' @param objectToSave Object to save.
#' @param fileName Base file name.
#' @param rowNumbers Deprecated compatibility argument.
#' @param replace Logical; replace latest version instead of creating new one.
#'
#' @return Full path to saved file.
#' @examples
#' \dontrun{
#' path <- saveVersionedFile("P:/llm/rubrics/GERD", list(a = 1), "temp.rds")
#' }
#' @export
saveVersionedFile <- function(saveDirectory, objectToSave, fileName, rowNumbers = FALSE, replace = FALSE) {
  ithSaveVersioned(saveDirectory, objectToSave, fileName, replace = replace)
}

ithGetConceptSetAliasCandidates <- function(conceptSetName) {
  candidates <- character()

  if (grepl("condition_codes", conceptSetName, fixed = TRUE)) {
    candidates <- c(candidates, gsub("condition_codes", "diagnosis_codes", conceptSetName, fixed = TRUE))
  }

  if (grepl("visits_visit_codes", conceptSetName, fixed = TRUE)) {
    candidates <- c(candidates, gsub("visits_visit_codes", "visit_codes", conceptSetName, fixed = TRUE))
  }

  unique(candidates[candidates != conceptSetName])
}

ithMigrateConceptSetLibraryNames <- function(outputDirectory,
                                                conceptLibraryFile = "rubricConceptSetLibrary.duckdb",
                                                dryRun = FALSE,
                                                verbose = TRUE) {
  csLibrary <- ithLoadLatestConceptSetLibrary(outputDirectory, conceptLibraryFile)

  if (length(csLibrary) < 2 || nrow(csLibrary[[1]]) == 0) {
    stop("Concept set library has no concept rows to migrate.")
  }

  conceptTable <- csLibrary[[1]]
  if (!("conceptSetName" %in% names(conceptTable)) || !("conceptSet" %in% names(conceptTable))) {
    stop("Concept set library is missing required columns: conceptSetName and conceptSet")
  }

  existingNames <- unique(tolower(as.character(conceptTable$conceptSetName)))

  proposals <- NULL
  for (rowUp in seq_len(nrow(conceptTable))) {
    sourceName <- as.character(conceptTable$conceptSetName[[rowUp]])
    aliasNames <- ithGetConceptSetAliasCandidates(sourceName)

    if (length(aliasNames) == 0) {
      next
    }

    for (aliasUp in aliasNames) {
      aliasRule <- if (grepl("diagnosis_codes", aliasUp, fixed = TRUE)) {
        "condition_to_diagnosis"
      } else {
        "visits_visit_to_visit"
      }

      proposals <- rbind(
        proposals,
        data.frame(
          sourceConceptSetName = sourceName,
          aliasConceptSetName = aliasUp,
          aliasRule = aliasRule,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  if (is.null(proposals) || nrow(proposals) == 0) {
    ithMaybeMessage(verbose, "No concept set alias proposals found for migration rules.")
    return(invisible(list(savedPath = NULL, created = data.frame(), proposed = data.frame(), dryRun = dryRun)))
  }

  proposals$aliasLower <- tolower(proposals$aliasConceptSetName)
  proposals <- proposals[!duplicated(proposals$aliasLower), c("sourceConceptSetName", "aliasConceptSetName", "aliasRule")]

  toCreate <- proposals[!(tolower(proposals$aliasConceptSetName) %in% existingNames), ]

  if (nrow(toCreate) == 0) {
    ithMaybeMessage(verbose, "All alias concept set names already exist; no migration changes needed.")
    return(invisible(list(savedPath = NULL, created = data.frame(), proposed = proposals, dryRun = dryRun)))
  }

  ithMaybeMessage(verbose, "Concept set alias migration candidates to create: ", nrow(toCreate))

  created <- NULL
  workingLibrary <- csLibrary

  for (rowUp in seq_len(nrow(toCreate))) {
    sourceName <- toCreate$sourceConceptSetName[[rowUp]]
    aliasName <- toCreate$aliasConceptSetName[[rowUp]]
    aliasRule <- toCreate$aliasRule[[rowUp]]
    cleanName <- gsub("_", " ", aliasName)

    ithMaybeMessage(verbose, "Creating alias concept set '", aliasName, "' from source '", sourceName, "' (", aliasRule, ").")

    sourceIdx <- which(tolower(as.character(workingLibrary[[1]]$conceptSetName)) == tolower(sourceName))
    if (length(sourceIdx) == 0) {
      ithMaybeMessage(verbose, "Skipping alias '", aliasName, "' because source concept set was not found in current working library.")
      next
    }

    sourceConceptJson <- workingLibrary[[1]]$conceptSet[[sourceIdx[[1]]]]

    if (!dryRun) {
      workingLibrary <- saveConceptSet(
        cleanName = cleanName,
        conceptSetName = aliasName,
        finalConceptSet = sourceConceptJson,
        csLibraryName = workingLibrary
      )
    }

    created <- rbind(
      created,
      data.frame(
        sourceConceptSetName = sourceName,
        aliasConceptSetName = aliasName,
        aliasRule = aliasRule,
        stringsAsFactors = FALSE
      )
    )
  }

  if (is.null(created) || nrow(created) == 0) {
    ithMaybeMessage(verbose, "No alias rows were created during migration.")
    return(invisible(list(savedPath = NULL, created = data.frame(), proposed = proposals, dryRun = dryRun)))
  }

  if (dryRun) {
    ithMaybeMessage(verbose, "Dry run completed. No files were written.")
    return(invisible(list(savedPath = NULL, created = created, proposed = proposals, dryRun = dryRun)))
  }

  resolvedLibraryPath <- ithResolveDuckdbLibraryPath(
    libraryPath = outputDirectory,
    defaultFileName = ithCanonicalConceptLibraryFileName(conceptLibraryFile),
    mustExist = TRUE
  )

  for (rowUp in seq_len(nrow(created))) {
    aliasName <- as.character(created$aliasConceptSetName[[rowUp]])
    conceptIdx <- which(tolower(as.character(workingLibrary[[1]]$conceptSetName)) == tolower(aliasName))
    embedIdx <- which(tolower(as.character(workingLibrary[[2]]$conceptSetName)) == tolower(aliasName))

    if (length(conceptIdx) == 0) {
      next
    }

    conceptSet <- workingLibrary[[1]]$conceptSet[[conceptIdx[[1]]]]
    cleanName <- as.character(workingLibrary[[1]]$cleanName[[conceptIdx[[1]]]])
    embedValue <- if (length(embedIdx) > 0) workingLibrary[[2]]$embed[[embedIdx[[1]]]] else NA_real_

    ithDuckdbUpsertConceptSet(
      libraryPath = resolvedLibraryPath,
      conceptSetName = aliasName,
      cleanName = cleanName,
      conceptSet = conceptSet,
      embedding = embedValue,
      runId = "migrateConceptSetLibraryAliases",
      userName = Sys.info()[["user"]],
      conditionName = NA_character_,
      cohortId = NA_integer_,
      sourceFile = outputDirectory
    )
  }

  savedPath <- resolvedLibraryPath

  ithMaybeMessage(verbose, "Concept set alias migration saved to: ", savedPath)
  invisible(list(savedPath = savedPath, created = created, proposed = proposals, dryRun = dryRun))
}

#' One-time migrate concept set library aliases
#'
#' Creates one-time alias concept sets in the shared concept set library by duplicating
#' existing JSON definitions and recomputing embeddings for the alias names.
#'
#' Applied alias rules:
#' - `condition_codes` -> `diagnosis_codes`
#' - `visits_visit_codes` -> `visit_codes`
#'
#' @param outputDirectory Directory containing versioned `rubricConceptSetLibrary` files.
#' @param conceptLibraryFile DuckDB concept set library file name.
#' @param dryRun Logical; when `TRUE`, report planned creations without writing changes.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Invisible list with `savedPath`, `created`, `proposed`, and `dryRun`.
#' @examples
#' \dontrun{
#' result <- migrateConceptSetLibraryAliases(
#'   outputDirectory = "P:/llm/rubrics",
#'   conceptLibraryFile = "rubricConceptSetLibrary.duckdb",
#'   dryRun = TRUE
#' )
#' }
#' @export
migrateConceptSetLibraryAliases <- function(outputDirectory,
                                            conceptLibraryFile = "rubricConceptSetLibrary.duckdb",
                                            dryRun = FALSE,
                                            verbose = TRUE) {
  ithMigrateConceptSetLibraryNames(
    outputDirectory = outputDirectory,
    conceptLibraryFile = conceptLibraryFile,
    dryRun = dryRun,
    verbose = verbose
  )
}

#' Summarize scoring results from saved artifacts
#'
#' Loads the latest versioned scoring outputs for a condition/database/cohort run
#' and returns compact summary tables for case distribution and rule-level activity.
#'
#' @param conditionName Condition/rubric name used under `outputDirectory/scoring`.
#' @param databaseId Database identifier used under `outputDirectory/scoring`.
#' @param randomCohortDefinitionId Random cohort definition ID used in scoring artifact file names.
#' @param outputDirectory Root output directory for rubric/scoring artifacts.
#' @param topN Number of top rows to keep for criteria hit summary.
#' @param saveSummary Logical; when `TRUE`, save versioned summary artifacts to the scoring folder.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return List with `summaryOverview`, `finalCaseSummary`, `rubricCaseSummary`,
#' `topCriteriaHits`, `ruleRowCounts`, and `artifactVersions`.
#' @examples
#' \dontrun{
#' result <- summarizeScoringResults(
#'   conditionName = "Gastroesophageal reflux disease",
#'   databaseId = "CCAE",
#'   randomCohortDefinitionId = 14412,
#'   outputDirectory = "P:/llm/rubricsNew",
#'   topN = 10,
#'   saveSummary = TRUE
#' )
#' }
#' @export
summarizeScoringResults <- function(conditionName,
                                    databaseId,
                                    randomCohortDefinitionId,
                                    outputDirectory,
                                    topN = 10,
                                    saveSummary = TRUE,
                                    verbose = TRUE) {
  ithSummarizeScoringResults(
    conditionName = conditionName,
    databaseId = databaseId,
    randomCohortDefinitionId = randomCohortDefinitionId,
    outputDirectory = outputDirectory,
    topN = topN,
    saveSummary = saveSummary,
    verbose = verbose
  )
}
