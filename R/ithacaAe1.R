# AE1 is a fixed "zero occurrence penalty" alternative-explanation rule added to every rubric. It
# fires (subtracting points) when the subject has no code from the condition's relevant diagnostic
# context (related/differential diagnoses, plus the condition's own diagnosis set) within -30 to +30
# days of index.

ithAe1DefaultPenalty <- 3
ithAe1WindowDays <- 30L
ithAe1RuleId <- "AE1"

# NULL, NA, blank, or "NULL" gives `default`; otherwise a positive number is required.
ithAe1ParsePenalty <- function(value, default = ithAe1DefaultPenalty, rowLabel = "zeroOccurrencePenalty") {
  raw <- if (is.null(value) || length(value) == 0L) NA else value[[1]]
  text <- trimws(as.character(raw))
  if (is.na(raw) || !nzchar(text) || tolower(text) %in% c("null", "na")) {
    return(default)
  }
  penalty <- suppressWarnings(as.numeric(text))
  if (is.na(penalty) || !is.finite(penalty) || penalty <= 0) {
    stop("zeroOccurrencePenalty must be a positive number (points subtracted); got '", text, "': ", rowLabel, call. = FALSE)
  }
  penalty
}

ithAe1Slug <- function(conditionName) {
  gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", gsub("['\u2019]", "", tolower(trimws(as.character(conditionName[[1]]))))))
}

ithAe1ConceptSetName <- function(conditionName) {
  paste0(ithAe1Slug(conditionName), "_RELEVANT_DIAGNOSIS_CODES")
}

# The condition's own diagnosis set (the same one ER1 uses).
ithAe1ConditionConceptSetName <- function(conditionName) {
  paste0(ithAe1Slug(conditionName), "_DIAGNOSIS_CODES")
}

# Concept sets the AE1 SQL needs: the related/differential set plus the condition's own set.
ithAe1NeededConceptSets <- function(conditionName) {
  c(ithAe1ConceptSetName(conditionName), ithAe1ConditionConceptSetName(conditionName))
}

ithAe1Criteria <- function(conditionName) {
  paste0(
    "No ", trimws(as.character(conditionName[[1]])),
    " diagnosis or related/differential diagnosis code within -", ithAe1WindowDays, " to +", ithAe1WindowDays, " days of index"
  )
}

ithAe1Sql <- function(conceptSetNames) {
  paste0(
    "WITH cohort_subjects AS (\n",
    "  SELECT subject_id, cohort_start_date, cohort_definition_id\n",
    "  FROM @cohortSchema.@cohortTable\n",
    "  WHERE cohort_definition_id = @cohortId\n",
    ")\n",
    "SELECT cs.subject_id, cs.cohort_start_date, cs.cohort_definition_id\n",
    "FROM cohort_subjects cs\n",
    "WHERE NOT EXISTS (\n",
    "  SELECT 1\n",
    "  FROM @databaseSchema.condition_occurrence co\n",
    "  WHERE co.person_id = cs.subject_id\n",
    "    AND co.condition_concept_id IN (", paste0("@", conceptSetNames, collapse = ", "), ")\n",
    "    AND co.condition_start_date >= DATEADD(day, -", ithAe1WindowDays, ", cs.cohort_start_date)\n",
    "    AND co.condition_start_date <= DATEADD(day, ", ithAe1WindowDays, ", cs.cohort_start_date)\n",
    ")"
  )
}

ithAe1Rationale <- function(penalty) {
  paste0(
    "Fixed: a subject with no diagnosis of the condition or a related/differential diagnosis around index loses ",
    format(penalty), " point(s)."
  )
}

# The fixed AE1 as a row of the rubric item's alternativeExplanations table.
ithAe1RuleRow <- function(conditionName, penalty) {
  data.frame(
    ruleId = ithAe1RuleId,
    rule_type = "alternative_explanations",
    criteria = ithAe1Criteria(conditionName),
    start_day = -ithAe1WindowDays,
    end_day = ithAe1WindowDays,
    presenceOrAbsence = "absence",
    occurrencesNeeded = 1L,
    minimumTimeAfterFirstOccurrenceDays = NA_integer_,
    maximumTimeAfterFirstOccurrenceDays = NA_integer_,
    points = -penalty,
    rationaleForPoints = ithAe1Rationale(penalty),
    cap_max_decision = NA_character_,
    rationale = ithAe1Rationale(penalty),
    conceptSets_needed = paste(ithAe1NeededConceptSets(conditionName), collapse = ","),
    stringsAsFactors = FALSE
  )
}

# TRUE for rows that are the fixed AE1: rule ID AE1 using the relevant-diagnosis set (this condition's
# when conditionName is given, otherwise any condition's).
ithAe1IsFixed <- function(ruleIds, conceptSets, conditionName = NULL) {
  matchesSet <- function(x) {
    sets <- tolower(ithParseConceptSets(x))
    if (is.null(conditionName)) {
      any(grepl("_relevant_diagnosis_codes$", sets))
    } else {
      tolower(ithAe1ConceptSetName(conditionName)) %in% sets
    }
  }
  toupper(trimws(as.character(ruleIds))) == ithAe1RuleId &
    vapply(as.character(conceptSets), matchesSet, logical(1), USE.NAMES = FALSE)
}

# Shifts alternative-explanation IDs by one (AE1 -> AE2, ...) in a rule table or short rubric id vector.
ithAe1ShiftIds <- function(ids) {
  isAe <- grepl("^AE[0-9]+$", toupper(trimws(as.character(ids))))
  shifted <- as.character(ids)
  shifted[isAe] <- paste0("AE", as.integer(sub("^AE", "", toupper(trimws(shifted[isAe])))) + 1L)
  shifted
}

# Puts the fixed AE1 first in an alternativeExplanations table and renumbers the others from AE2.
# An existing fixed AE1 is replaced (so its points follow `penalty`); other rules keep their order.
ithAe1ApplyToRuleTable <- function(rules, conditionName, penalty) {
  fixedRow <- ithAe1RuleRow(conditionName, penalty)
  if (!is.data.frame(rules) || nrow(rules) == 0L) {
    return(fixedRow)
  }
  isFixed <- if (all(c("ruleId", "conceptSets_needed") %in% names(rules))) {
    ithAe1IsFixed(rules$ruleId, rules$conceptSets_needed, conditionName)
  } else {
    rep(FALSE, nrow(rules))
  }
  others <- rules[!isFixed, , drop = FALSE]
  if (nrow(others) > 0L) {
    others$ruleId <- paste0("AE", seq_len(nrow(others)) + 1L)
  }
  result <- ithBindRows(if (nrow(others) > 0L) list(fixedRow, others) else list(fixedRow))
  rownames(result) <- NULL
  result
}

ithAe1DefaultSearch <- function(term) {
  ithVectorSearchStandard(term, domains = "Condition", limit = 5)
}

ithAe1CandidateTable <- function(term, searchFunction) {
  found <- tryCatch(searchFunction(term), error = function(e) NULL)
  if (!is.data.frame(found) || nrow(found) == 0L || !("conceptId" %in% names(found))) {
    return(NULL)
  }
  pick <- function(column) {
    if (column %in% names(found)) as.character(found[[column]]) else rep("", nrow(found))
  }
  candidates <- data.frame(
    searchTerm = term,
    conceptId = suppressWarnings(as.integer(found$conceptId)),
    conceptName = pick("conceptName"),
    domainId = pick("domainId"),
    vocabularyId = pick("vocabularyId"),
    conceptClassId = pick("conceptClassId"),
    standardConcept = pick("standardConcept"),
    stringsAsFactors = FALSE
  )
  candidates <- candidates[!is.na(candidates$conceptId) & !duplicated(candidates$conceptId), , drop = FALSE]
  if (nrow(candidates) == 0L) NULL else candidates
}

ithAe1RelatedTermsType <- function() {
  ellmer::type_object(
    relatedTerms = ellmer::type_array(ellmer::type_string()),
    rationale = ellmer::type_string()
  )
}

ithAe1SelectionType <- function() {
  ellmer::type_object(
    relatedConceptIds = ellmer::type_array(ellmer::type_integer()),
    rationale = ellmer::type_string()
  )
}

ithAe1RelatedTermsPrompt <- function(conditionName, additionalInformation = "") {
  info <- if (is.null(additionalInformation) || length(additionalInformation) == 0L || is.na(additionalInformation[[1]]) ||
    !nzchar(trimws(additionalInformation[[1]]))) {
    "<none supplied>"
  } else {
    as.character(additionalInformation[[1]])
  }
  paste0(
    "A phenotype algorithm is scoring whether a person truly has: ", conditionName, ".\n",
    "Additional information: ", info, "\n\n",
    "A subject with NO diagnosis code for the condition and NO diagnosis code for any closely related condition around the index date is unlikely to be a true case.\n",
    "List 3 to 5 diagnosis terms (as they would appear as OMOP condition concept names) that a clinician could legitimately record in place of, or alongside, this condition:\n",
    "- If the condition is a diagnosis, give its main differential diagnoses and rule-out diagnoses.\n",
    "- If the condition is a symptom or finding, give the diagnoses that commonly explain or are associated with it.\n",
    "Do not include the condition itself, broad catch-all terms (for example 'disease' or 'disorder' alone), or terms that are not diagnoses.\n",
    "Return the terms in relatedTerms and a one-sentence rationale."
  )
}

ithAe1SelectionPrompt <- function(conditionName, relatedTerms, candidates) {
  table <- jsonlite::toJSON(
    candidates[, c("searchTerm", "conceptId", "conceptName", "domainId", "vocabularyId", "conceptClassId")],
    auto_unbox = TRUE, pretty = TRUE
  )
  paste0(
    "Condition of interest: ", conditionName, " (it has its own concept set, so do not include it here)\n",
    "Related or differential diagnosis terms: ", paste(relatedTerms, collapse = "; "), "\n\n",
    "Below are OMOP concept candidates found by searching each related term. ",
    "Each chosen concept will be used together with all of its descendants.\n\n",
    "Choose relatedConceptIds: up to 2 candidates per related term that correctly represent that term. ",
    "Leave out a term when no candidate fits, and leave out any candidate that is the condition of interest itself or a subtype of it.\n",
    "Use only conceptId values from the table. Do not pick a concept that is clinically different from the term searched.\n",
    "Return the IDs and a short rationale.\n\n",
    "Candidates:\n", table
  )
}

ithAe1ToIntegerVector <- function(x) {
  values <- suppressWarnings(as.integer(unlist(x, use.names = FALSE)))
  unique(values[!is.na(values)])
}

# Builds the relevant-diagnosis concept set (concept IDs plus descendants) for one condition.
# Two LLM calls: related terms, then confirmation of which Hecate candidates represent each term.
# searchFunction(term) must return a data frame with conceptId, conceptName, domainId, vocabularyId,
# conceptClassId and standardConcept (the default queries Hecate).
ithBuildAe1ConceptSet <- function(conditionName,
                                  llmClient,
                                  additionalInformation = "",
                                  searchFunction = ithAe1DefaultSearch,
                                  verbose = TRUE) {
  conditionName <- trimws(as.character(conditionName[[1]]))
  ithMaybeMessage(verbose, "Building ", ithAe1ConceptSetName(conditionName), " from Hecate.")

  termsResponse <- ithQueryLlm(
    llmClient = llmClient,
    prompt = ithAe1RelatedTermsPrompt(conditionName, additionalInformation),
    output = "list",
    ellmerTypeObject = ithAe1RelatedTermsType(),
    silent = !isTRUE(verbose)
  )
  relatedTerms <- unique(trimws(as.character(unlist(termsResponse$relatedTerms, use.names = FALSE))))
  relatedTerms <- relatedTerms[nzchar(relatedTerms) & tolower(relatedTerms) != tolower(conditionName)]
  relatedTerms <- utils::head(relatedTerms, 5L)

  candidateTables <- list()
  for (term in relatedTerms) {
    found <- ithAe1CandidateTable(term, searchFunction)
    if (!is.null(found)) {
      candidateTables[[length(candidateTables) + 1L]] <- found
    }
  }
  ithMaybeMessage(verbose, "  Related terms proposed: ", if (length(relatedTerms) > 0L) paste(relatedTerms, collapse = "; ") else "<none>")

  # With nothing usable the set holds a placeholder concept that matches no one, so AE1 relies on the
  # condition's own concept set alone.
  placeholderResult <- function(reason, rationale = "", candidates = NULL) {
    warning(
      "AE1 for '", conditionName, "': ", reason, " Using an empty placeholder relevant set; ",
      "AE1 will rely on the condition's own concept set only.",
      call. = FALSE, immediate. = TRUE
    )
    list(
      conceptSet = list(items = list(list(
        concept = list(
          CONCEPT_ID = 36433327L, CONCEPT_NAME = "NO_CONCEPT_PLACEHOLDER", DOMAIN_ID = "Observation",
          VOCABULARY_ID = "None", CONCEPT_CLASS_ID = "Undefined", STANDARD_CONCEPT = "", INVALID_REASON = ""
        ),
        isExcluded = FALSE,
        includeDescendants = FALSE,
        includeMapped = FALSE
      ))),
      conceptIds = integer(0),
      relatedTerms = relatedTerms,
      candidates = candidates,
      chosen = NULL,
      placeholder = TRUE,
      rationale = rationale
    )
  }

  if (length(candidateTables) == 0L) {
    return(placeholderResult("Hecate returned no concepts for the related terms."))
  }
  candidates <- do.call(rbind, candidateTables)
  candidates <- candidates[!duplicated(candidates$conceptId), , drop = FALSE]
  ithMaybeMessage(
    verbose, "  Hecate candidates: ",
    paste0(candidates$conceptName, " (", candidates$conceptId, ")", collapse = "; ")
  )

  selection <- ithQueryLlm(
    llmClient = llmClient,
    prompt = ithAe1SelectionPrompt(conditionName, relatedTerms, candidates),
    output = "list",
    ellmerTypeObject = ithAe1SelectionType(),
    silent = !isTRUE(verbose)
  )
  returnedIds <- ithAe1ToIntegerVector(selection$relatedConceptIds)
  relatedIds <- returnedIds[returnedIds %in% candidates$conceptId]
  llmRationale <- as.character(ithCoalesce(selection$rationale, ""))[[1]]
  if (length(relatedIds) == 0L) {
    return(placeholderResult(
      paste0(
        "the LLM confirmed no related diagnosis concept (returned IDs: ",
        if (length(returnedIds) > 0L) paste(returnedIds, collapse = ", ") else "none",
        "; LLM rationale: ", llmRationale, ")."
      ),
      llmRationale,
      candidates
    ))
  }
  allIds <- unique(relatedIds)
  chosen <- candidates[match(allIds, candidates$conceptId), , drop = FALSE]

  items <- lapply(seq_len(nrow(chosen)), function(i) {
    list(
      concept = list(
        CONCEPT_ID = chosen$conceptId[[i]],
        CONCEPT_NAME = chosen$conceptName[[i]],
        DOMAIN_ID = chosen$domainId[[i]],
        VOCABULARY_ID = chosen$vocabularyId[[i]],
        CONCEPT_CLASS_ID = chosen$conceptClassId[[i]],
        STANDARD_CONCEPT = chosen$standardConcept[[i]],
        INVALID_REASON = ""
      ),
      isExcluded = FALSE,
      includeDescendants = TRUE,
      includeMapped = FALSE
    )
  })

  list(
    conceptSet = list(items = items),
    conceptIds = allIds,
    relatedTerms = relatedTerms,
    candidates = candidates,
    chosen = chosen,
    placeholder = FALSE,
    rationale = llmRationale
  )
}

# Writes the search terms sent to Hecate, the candidates it returned, and the concept set expression
# Ithaca built, so a relevant-diagnosis set can be reviewed outside the library.
ithWriteAe1Artifacts <- function(built, artifactDirectory) {
  dir.create(artifactDirectory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(searchTerm = as.character(built$relatedTerms), stringsAsFactors = FALSE),
    file.path(artifactDirectory, "hecateSearchTerms.csv"), row.names = FALSE
  )
  candidates <- if (is.data.frame(built$candidates)) built$candidates else data.frame()
  if (is.data.frame(built$chosen) && nrow(candidates) > 0L) {
    candidates$selected <- candidates$conceptId %in% built$chosen$conceptId
  }
  utils::write.csv(candidates, file.path(artifactDirectory, "hecateCandidates.csv"), row.names = FALSE)
  writeLines(
    as.character(jsonlite::toJSON(built$conceptSet, auto_unbox = TRUE, pretty = TRUE, null = "null")),
    file.path(artifactDirectory, "conceptSetExpression.json")
  )
  invisible(artifactDirectory)
}

#' Export existing relevant-diagnosis concept sets from the library
#'
#' Writes `conceptSetExpression.json` and `hecateCandidates.csv` (the expression's items) for every
#' `<condition>_RELEVANT_DIAGNOSIS_CODES` concept set in the library, each into its own folder under
#' the config's `conceptSetFolder` (default `<outputDirectory>/conceptSetBuild`).
#'
#' @param configPath Path to the Ithaca config.
#' @param conceptSetFolder Folder to write into; `NULL` uses the config.
#' @param libraryVersion Library version to read; `NULL` uses the config's `conceptLibraryVersion`.
#'
#' @return Invisibly, a data frame of exported concept set names and folders.
#' @examples
#' \dontrun{
#' exportRelevantDiagnosisConceptSets()
#' }
#' @export
exportRelevantDiagnosisConceptSets <- function(configPath = "config.yaml",
                                               conceptSetFolder = NULL,
                                               libraryVersion = NULL) {
  config <- ithReadIthacaConditionConfig(configPath)
  outputDirectory <- as.character(config$outputDirectory[[1]])
  if (is.null(conceptSetFolder)) {
    conceptSetFolder <- if (is.null(config$conceptSetFolder) || !nzchar(as.character(config$conceptSetFolder[[1]]))) {
      file.path(outputDirectory, "conceptSetBuild")
    } else {
      as.character(config$conceptSetFolder[[1]])
    }
  }
  conceptTable <- ithLoadConceptSetLibraryForScoring(
    outputDirectory = outputDirectory,
    seedLibraryPath = config$seedLibraryPath,
    libraryVersion = if (is.null(libraryVersion)) config$conceptLibraryVersion else libraryVersion
  )[[1]]

  rows <- which(grepl("_relevant_diagnosis_codes$", tolower(as.character(conceptTable$conceptSetName))))
  exported <- data.frame(conceptSetName = character(), folder = character(), stringsAsFactors = FALSE)
  for (row in rows) {
    name <- tolower(as.character(conceptTable$conceptSetName[[row]]))
    conceptSet <- ithPrepareConceptSetForResolution(conceptTable$conceptSet[[row]])
    if (is.null(conceptSet)) {
      warning("Skipping ", name, ": concept set could not be parsed.", call. = FALSE)
      next
    }
    folder <- file.path(conceptSetFolder, name)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    writeLines(
      as.character(jsonlite::toJSON(conceptSet, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA)),
      file.path(folder, "conceptSetExpression.json")
    )
    field <- function(item, column) {
      value <- item$concept[[column]]
      if (is.null(value) || length(value) == 0L) NA_character_ else as.character(value[[1]])
    }
    candidates <- data.frame(
      conceptId = vapply(conceptSet$items, field, character(1), column = "CONCEPT_ID"),
      conceptName = vapply(conceptSet$items, field, character(1), column = "CONCEPT_NAME"),
      domainId = vapply(conceptSet$items, field, character(1), column = "DOMAIN_ID"),
      vocabularyId = vapply(conceptSet$items, field, character(1), column = "VOCABULARY_ID"),
      conceptClassId = vapply(conceptSet$items, field, character(1), column = "CONCEPT_CLASS_ID"),
      standardConcept = vapply(conceptSet$items, field, character(1), column = "STANDARD_CONCEPT"),
      stringsAsFactors = FALSE
    )
    utils::write.csv(candidates, file.path(folder, "hecateCandidates.csv"), row.names = FALSE)
    exported[nrow(exported) + 1L, ] <- c(name, folder)
  }
  message("Exported ", nrow(exported), " relevant-diagnosis concept set(s) to: ", conceptSetFolder)
  invisible(exported)
}

# Adds the AE1 concept set to the library unless a set with that name already exists. Each upsert
# is a new library version, so it can be reverted. Returns list(status, name, built).
# A non-NULL artifactDirectory receives the build artifacts (see ithWriteAe1Artifacts).
ithEnsureAe1ConceptSetInLibrary <- function(conditionName,
                                            libraryPath,
                                            llmClient,
                                            additionalInformation = "",
                                            searchFunction = ithAe1DefaultSearch,
                                            cohortId = NA_integer_,
                                            runId = NA_character_,
                                            artifactDirectory = NULL,
                                            verbose = TRUE) {
  name <- tolower(ithAe1ConceptSetName(conditionName))
  if (!file.exists(libraryPath)) {
    ithDuckdbInitializeLibrary(libraryPath = libraryPath, verbose = verbose)
  }
  latestVersion <- ithDuckdbGetLatestLibraryVersion(libraryPath)
  if (!is.null(latestVersion)) {
    library <- ithDuckdbGetConceptSetLibrary(libraryPath = libraryPath, libraryVersion = latestVersion)
    if (nrow(library[[1]]) > 0L && name %in% tolower(as.character(library[[1]]$conceptSetName))) {
      return(list(status = "existing", name = name, built = NULL))
    }
  }

  built <- ithBuildAe1ConceptSet(conditionName, llmClient, additionalInformation, searchFunction, verbose)
  if (!is.null(artifactDirectory)) {
    tryCatch(
      ithWriteAe1Artifacts(built, artifactDirectory),
      error = function(e) warning("Could not write ", name, " artifacts to ", artifactDirectory, ": ", conditionMessage(e), call. = FALSE)
    )
  }
  cleanName <- gsub("_", " ", name)
  embedding <- tryCatch(
    ithGetEmbeddingVectorsInternal(tolower(cleanName), reportTime = FALSE, silent = TRUE)[[1]],
    error = function(e) NULL
  )
  if (is.null(embedding) || !is.numeric(embedding) || length(embedding) <= 1L || any(!is.finite(embedding))) {
    embedding <- NA_real_
  }
  ithDuckdbUpsertConceptSet(
    libraryPath = libraryPath,
    conceptSetName = name,
    cleanName = cleanName,
    conceptSet = built$conceptSet,
    embedding = embedding,
    runId = runId,
    userName = Sys.info()[["user"]],
    conditionName = conditionName,
    cohortId = cohortId,
    sourceFile = "ithEnsureAe1ConceptSetInLibrary"
  )
  list(status = "created", name = name, built = built)
}

# Adds the fixed AE1 to a short rubric and rubricList: existing AE IDs shift up by one and the
# fixed row is inserted before the first AE row (else before the first ER row, else at the end).
# When the fixed AE1 is already there only its score is brought in line with `penalty`.
ithAe1ApplyToRubric <- function(shortRubric, rubricList, conditionName, penalty) {
  neededSets <- ithAe1NeededConceptSets(conditionName)
  ruleIds <- toupper(trimws(as.character(shortRubric$ruleId)))
  isAeType <- as.character(shortRubric$type) == "alternativeExplanations"
  isFixed <- isAeType & ithAe1IsFixed(shortRubric$ruleId, shortRubric$neededConceptSets, conditionName)
  rubricIds <- unique(as.character(shortRubric$rubric))
  missingRubrics <- rubricIds[!(rubricIds %in% as.character(shortRubric$rubric[isFixed]))]
  changed <- character()

  if (any(isFixed)) {
    stale <- isFixed & !(suppressWarnings(as.numeric(shortRubric$score)) %in% -penalty)
    if (any(stale)) {
      shortRubric$score[stale] <- -penalty
      shortRubric$rationale[stale] <- ithAe1Rationale(penalty)
      changed <- "penaltyUpdated"
    }
    # An AE1 saved before the condition's own set was added to the SQL gets the current sets and SQL.
    outdated <- isFixed & vapply(
      as.character(shortRubric$neededConceptSets),
      function(x) !all(tolower(neededSets) %in% tolower(ithParseConceptSets(x))),
      logical(1), USE.NAMES = FALSE
    )
    if (any(outdated)) {
      shortRubric$neededConceptSets[outdated] <- paste(neededSets, collapse = ",")
      shortRubric$sqlStatement[outdated] <- ithAe1Sql(neededSets)
      changed <- c(changed, "penaltyUpdated")
    }
  }

  for (rubricId in missingRubrics) {
    inRubric <- as.character(shortRubric$rubric) == rubricId
    shiftRows <- inRubric & isAeType & grepl("^AE[0-9]+$", ruleIds)
    shortRubric$ruleId[shiftRows] <- ithAe1ShiftIds(shortRubric$ruleId[shiftRows])
    template <- shortRubric[which(inRubric)[[1]], , drop = FALSE]
    newRow <- template
    newRow$ruleId <- ithAe1RuleId
    newRow$type <- "alternativeExplanations"
    newRow$score <- -penalty
    newRow$criteria <- ithAe1Criteria(conditionName)
    newRow$rationale <- ithAe1Rationale(penalty)
    newRow$neededConceptSets <- paste(neededSets, collapse = ",")
    newRow$sqlStatement <- ithAe1Sql(neededSets)
    newRow$okToUse <- TRUE
    anchor <- which(inRubric & isAeType)
    if (length(anchor) == 0L) {
      anchor <- which(inRubric & as.character(shortRubric$type) == "evidenceRules")
    }
    position <- if (length(anchor) > 0L) anchor[[1]] else nrow(shortRubric) + 1L
    before <- if (position > 1L) shortRubric[seq_len(position - 1L), , drop = FALSE] else shortRubric[0, , drop = FALSE]
    after <- if (position <= nrow(shortRubric)) shortRubric[position:nrow(shortRubric), , drop = FALSE] else shortRubric[0, , drop = FALSE]
    shortRubric <- rbind(before, newRow, after)
    # Positions moved; recompute the flags for the next rubric.
    ruleIds <- toupper(trimws(as.character(shortRubric$ruleId)))
    isAeType <- as.character(shortRubric$type) == "alternativeExplanations"
    changed <- c(changed, "added")
  }
  rownames(shortRubric) <- NULL

  for (itemUp in seq_along(rubricList)) {
    item <- rubricList[[itemUp]]
    if (is.null(item)) next
    item$alternativeExplanations <- ithAe1ApplyToRuleTable(item$alternativeExplanations, conditionName, penalty)
    rubricList[[itemUp]] <- item
  }

  list(shortRubric = shortRubric, rubricList = rubricList, changed = unique(changed))
}

#' Add the fixed AE1 zero-occurrence penalty rule to existing rubrics
#'
#' AE1 subtracts points from a subject who has no diagnosis code for the condition (its
#' `<condition>_DIAGNOSIS_CODES` set) or for 3-5 related/differential diagnoses within -30 to +30
#' days of index. For each condition this:
#' \enumerate{
#'   \item builds the condition's `<condition>_RELEVANT_DIAGNOSIS_CODES` concept set, which excludes the
#'     condition itself (an LLM proposes related diagnoses, Hecate finds the concept IDs, the LLM
#'     confirms them, and each is stored with its descendants) and adds it to the concept set library,
#'     a new library version;
#'   \item inserts AE1 into every rubric in the latest short rubric and `rubricList.rds` and shifts
#'     the existing alternative explanations up by one (AE1 becomes AE2, and so on);
#'   \item adds the concept set to the latest `matchedConceptSets.rds`.
#' }
#' The short rubric, concise rubric CSV, `rubricList.rds` and matched concept sets are saved as new
#' versions and the library as a new version, so everything can be reverted by using the earlier
#' versions. A condition that already has the fixed AE1 only has its penalty updated when it
#' differs. Scoring results created before the change must be rerun because AE IDs shift.
#'
#' @param conditionNames Optional character vector of condition names (`rubricName` or
#'   `testCondition`, case-insensitive). When `NULL`, every condition in `conditionList` (or the
#'   config) is processed.
#' @param conditionList Optional list of condition specifications (each with `testCondition` and,
#'   optionally, `zeroOccurrencePenalty`, `additionalInformation`, `randomCohortDefinitionId`) or a
#'   character vector of condition names. When `NULL`, the config's conditionSpecs (including
#'   `conditionSpecsCsvPath`) are used.
#' @param configPath Path to the YAML configuration file; provides `outputDirectory`,
#'   `seedLibraryPath` and the default `zeroOccurrencePenalty`.
#' @param llmClient LLM client used to propose and confirm related diagnoses.
#' @param dryRun Logical; when `TRUE`, report what would change without saving or building anything.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return Data frame with one row per condition: `conditionName`, `status`
#'   (`added`, `penaltyUpdated`, `alreadyPresent`, `wouldAdd`, `noShortRubric`, or `failed`),
#'   `conceptSet`, `conceptSetStatus` and `detail`.
#' @examples
#' \dontrun{
#' addZeroOccurrencePenaltyRule(dryRun = TRUE)
#' addZeroOccurrencePenaltyRule(conditionNames = "Confusion")
#' addZeroOccurrencePenaltyRule(conditionList = list(list(testCondition = "COPD", zeroOccurrencePenalty = 4)))
#' }
#' @export
addZeroOccurrencePenaltyRule <- function(conditionNames = NULL,
                                         conditionList = NULL,
                                         configPath = "config.yaml",
                                         llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                                         dryRun = FALSE,
                                         verbose = TRUE) {
  ithCheckConditionNamesNotConfig(conditionNames)
  config <- ithReadIthacaConditionConfig(configPath)
  if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
    stop("outputDirectory is not defined in config: ", configPath)
  }
  outputDirectory <- as.character(config$outputDirectory[[1]])
  defaultPenalty <- ithAe1ParsePenalty(config$zeroOccurrencePenalty)

  specs <- if (is.null(conditionList)) {
    config$conditionSpecs
  } else if (is.character(conditionList)) {
    lapply(conditionList, function(name) list(testCondition = name))
  } else {
    conditionList
  }
  specName <- function(spec) as.character(ithCoalesce(spec$testCondition, spec$rubricName)[[1]])
  if (!is.null(conditionNames)) {
    requested <- tolower(trimws(conditionNames))
    unknown <- conditionNames[!(requested %in% tolower(vapply(specs, function(s) c(specName(s)), character(1))))]
    if (length(unknown) > 0L) {
      stop("Requested conditionNames are not in the condition list: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    specs <- Filter(function(spec) tolower(specName(spec)) %in% requested, specs)
  }

  libraryLocation <- ithResolveConceptLibraryLocation(
    libraryPath = ithCoalesce(config$seedLibraryPath, outputDirectory),
    defaultFileName = "rubricConceptSetLibrary.duckdb"
  )
  runId <- paste0("addZeroOccurrencePenaltyRule_", format(Sys.time(), "%Y%m%d%H%M%S"))

  results <- lapply(specs, function(spec) {
    conditionName <- specName(spec)
    result <- function(status, conceptSet = NA_character_, conceptSetStatus = NA_character_, detail = "") {
      data.frame(
        conditionName = conditionName, status = status, conceptSet = conceptSet,
        conceptSetStatus = conceptSetStatus, detail = detail, stringsAsFactors = FALSE
      )
    }
    tryCatch({
      penalty <- ithAe1ParsePenalty(spec$zeroOccurrencePenalty, default = defaultPenalty, rowLabel = conditionName)
      conditionDir <- file.path(outputDirectory, conditionName)
      shortRubric <- tryCatch(ithLoadLatestShortRubric(conditionName, outputDirectory), error = function(e) NULL)
      if (is.null(shortRubric)) {
        return(result("noShortRubric", detail = "no short rubric found"))
      }
      rubricObject <- ithLoadLatest(conditionDir, "rubricList.rds")
      if (is.null(rubricObject)) {
        return(result("failed", detail = "no rubricList.rds found"))
      }
      applied <- ithAe1ApplyToRubric(shortRubric, rubricObject$rubricList, conditionName, penalty)
      conceptSetName <- tolower(ithAe1ConceptSetName(conditionName))
      if (length(applied$changed) == 0L) {
        return(result("alreadyPresent", conceptSetName))
      }
      if (isTRUE(dryRun)) {
        return(result("wouldAdd", conceptSetName, detail = paste(applied$changed, collapse = ", ")))
      }

      # Build the concept set first so a failure leaves the rubric files untouched.
      cohortId <- suppressWarnings(as.integer(ithCoalesce(spec$randomCohortDefinitionId, NA)[[1]]))
      ensured <- ithEnsureAe1ConceptSetInLibrary(
        conditionName = conditionName,
        libraryPath = libraryLocation$resolvedPath,
        llmClient = llmClient,
        additionalInformation = ithCoalesce(spec$additionalInformation, ""),
        cohortId = cohortId,
        runId = runId,
        artifactDirectory = file.path(
          if (is.null(config$conceptSetFolder) || !nzchar(as.character(config$conceptSetFolder[[1]]))) conditionDir else as.character(config$conceptSetFolder[[1]]),
          conceptSetName
        ),
        verbose = verbose
      )

      ithSaveShortRubric(applied$shortRubric, conditionName, outputDirectory, yesCombinationReport = FALSE)
      rubricObject$rubricList <- applied$rubricList
      rubricObject$neededConcepts <- ithBuildNeededConceptsFromShortRubric(applied$shortRubric)
      ithSaveVersioned(saveDirectory = conditionDir, objectToSave = rubricObject, fileName = "rubricList.rds")

      matched <- ithLoadLatest(conditionDir, "matchedConceptSets.rds")
      if (!is.null(matched) && !(conceptSetName %in% tolower(as.character(matched$csName)))) {
        newMatch <- data.frame(csName = conceptSetName, libCsName = conceptSetName, coSim = 1, stringsAsFactors = FALSE)
        ithSaveVersioned(
          saveDirectory = conditionDir,
          objectToSave = ithBindRows(list(matched, newMatch)),
          fileName = "matchedConceptSets.rds"
        )
      }
      status <- if ("added" %in% applied$changed) "added" else "penaltyUpdated"
      ithMaybeMessage(verbose, conditionName, ": ", status, " (concept set ", ensured$status, ").")
      result(status, conceptSetName, ensured$status, paste(applied$changed, collapse = ", "))
    }, error = function(e) {
      message(conditionName, ": ", conditionMessage(e))
      result("failed", detail = conditionMessage(e))
    })
  })

  out <- do.call(rbind, results)
  if (any(out$status %in% c("added", "penaltyUpdated"))) {
    message("Rerun scoring for the updated conditions: AE rule IDs shifted and AE1 is new.")
  }
  out
}
