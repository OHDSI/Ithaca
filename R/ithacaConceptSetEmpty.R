# Handling of concept sets whose build returns no concepts.
#
# A set that comes back empty is not a failure of the batch: it is either replaced by an existing
# set under a simplified name (levothyroxine_brand_medication_codes -> levothyroxine_medication_codes)
# or recorded as "known empty" (placeholder concept 36433327 plus a knownEmpty.json file) so reruns
# do not rebuild it and rules that use it are flagged instead of silently scoring zero.

ithPlaceholderConceptId <- 36433327

# Qualifier words that may be dropped from a concept-set name without changing what it means much.
ithEmptyConceptSetQualifiers <- c(
  "brand", "brands", "generic", "generics", "specific", "related", "general", "other", "various", "common"
)

# TRUE when the concept set is only the placeholder concept, i.e. it was recorded as known empty.
ithIsKnownEmptyConceptSet <- function(conceptSet) {
  prepared <- tryCatch(ithPrepareConceptSetForResolution(conceptSet), error = function(e) NULL)
  if (!is.list(prepared) || length(prepared$items) != 1L) {
    return(FALSE)
  }
  conceptId <- tryCatch(prepared$items[[1]]$concept$CONCEPT_ID, error = function(e) NULL)
  conceptId <- suppressWarnings(as.numeric(conceptId))
  length(conceptId) == 1L && !is.na(conceptId) && conceptId == ithPlaceholderConceptId
}

# State of one concept set in the library table (csLibrary[[1]]), looked up by exact name:
# "missing", "readable", "knownEmpty" or "unreadable".
ithLibraryConceptSetState <- function(conceptSetTable, conceptSetName) {
  if (is.null(conceptSetTable) || nrow(conceptSetTable) == 0L || !("conceptSetName" %in% names(conceptSetTable))) {
    return("missing")
  }
  rowIdx <- which(tolower(as.character(conceptSetTable$conceptSetName)) == tolower(conceptSetName))
  if (length(rowIdx) == 0L) {
    return("missing")
  }
  conceptSet <- conceptSetTable$conceptSet[[rowIdx[[1]]]]
  if (ithIsKnownEmptyConceptSet(conceptSet)) {
    return("knownEmpty")
  }
  prepared <- tryCatch(ithPrepareConceptSetForResolution(conceptSet), error = function(e) NULL)
  if (is.list(prepared) && length(prepared$items) > 0L) "readable" else "unreadable"
}

# Candidate names made by dropping one qualifier word (see ithEmptyConceptSetQualifiers) from the
# descriptive part of the name; the first word and the controlled suffix are kept. Returns
# character(0) when the name has no droppable qualifier.
ithSimplifiedConceptSetNames <- function(conceptSetName, qualifiers = ithEmptyConceptSetQualifiers) {
  tokens <- strsplit(tolower(as.character(conceptSetName)), "_", fixed = TRUE)[[1]]
  n <- length(tokens)
  if (n < 4L) {
    return(character())
  }
  suffix <- tokens[(n - 1L):n]
  if (!(toupper(paste(suffix, collapse = "_")) %in% ithControlledConceptSetSuffixes)) {
    return(character())
  }
  headIdx <- seq_len(n - 2L)
  dropIdx <- rev(headIdx[headIdx > 1L & tokens[headIdx] %in% tolower(qualifiers)])
  vapply(
    dropIdx,
    function(i) paste(c(tokens[setdiff(headIdx, i)], suffix), collapse = "_"),
    character(1)
  )
}

ithWriteKnownEmptyRecord <- function(directory, conceptSetName, outcome, reason, attempts, runId) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      conceptSetName = conceptSetName,
      outcome = outcome,
      reason = reason,
      attempts = as.list(as.character(attempts)),
      runId = runId,
      recordedAt = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    path = file.path(directory, "knownEmpty.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  invisible(file.path(directory, "knownEmpty.json"))
}

ithConceptSetSummaryRow <- function(conceptSetName,
                                    status,
                                    outcome = "",
                                    reason = "",
                                    aliasTarget = "",
                                    seconds = NA_real_) {
  data.frame(
    conceptSetName = as.character(conceptSetName),
    status = as.character(status),
    outcome = as.character(outcome),
    reason = substr(gsub("[\r\n]+", " ", as.character(reason)), 1, 300),
    aliasTarget = as.character(aliasTarget),
    seconds = as.numeric(seconds),
    stringsAsFactors = FALSE
  )
}

# Names among matches$csName whose matched library set is a known-empty record.
ithKnownEmptyMatchedNames <- function(matches, conceptSetTable) {
  if (is.null(matches) || nrow(matches) == 0L) {
    return(character())
  }
  isEmpty <- vapply(
    as.character(matches$libCsName),
    function(libName) identical(ithLibraryConceptSetState(conceptSetTable, libName), "knownEmpty"),
    logical(1)
  )
  unique(as.character(matches$csName)[isEmpty])
}

# Flags (okToUse = FALSE) the rules whose SQL uses a known-empty concept set and un-flags rules
# flagged earlier that no longer do. The flagged rules are listed in
# <condition>_emptyConceptSetRules_V#.csv in the condition folder. SQL and concept sets are not
# touched. Skipped with a warning when the concise rubric has unapplied edits.
ithFlagRulesForEmptyConceptSets <- function(conditionName,
                                            outputDirectory,
                                            emptyNames,
                                            llmClient,
                                            verbose = TRUE) {
  conditionDir <- file.path(outputDirectory, conditionName)
  shortRubric <- tryCatch(ithLoadLatestShortRubric(conditionName, outputDirectory), error = function(e) NULL)
  if (is.null(shortRubric) || nrow(shortRubric) == 0L) {
    return(invisible(NULL))
  }
  sidecarName <- paste0(conditionName, "_emptyConceptSetRules.csv")
  previous <- ithLoadLatest(conditionDir, sidecarName)
  emptyNorm <- ithNormalizeConceptSetName(emptyNames)

  hits <- lapply(seq_len(nrow(shortRubric)), function(i) {
    used <- ithScoringSqlConceptSets(shortRubric$sqlStatement[[i]])
    used[ithNormalizeConceptSetName(used) %in% emptyNorm]
  })
  affected <- vapply(hits, length, integer(1)) > 0L
  ruleKey <- function(rubric, ruleId) paste(trimws(as.character(rubric)), trimws(as.character(ruleId)), sep = "||")
  shortKey <- ruleKey(shortRubric$rubric, shortRubric$ruleId)

  current <- data.frame(
    condition = rep(conditionName, sum(affected)),
    rubric = as.character(shortRubric$rubric[affected]),
    ruleId = as.character(shortRubric$ruleId[affected]),
    conceptSets = vapply(hits[affected], paste, character(1), collapse = ","),
    stringsAsFactors = FALSE
  )
  previousKeys <- if (is.null(previous) || nrow(previous) == 0L) character() else ruleKey(previous$rubric, previous$ruleId)
  currentKeys <- ruleKey(current$rubric, current$ruleId)
  unchanged <- setequal(previousKeys, currentKeys) &&
    (is.null(previous) || nrow(previous) == 0L || identical(sort(as.character(previous$conceptSets)), sort(current$conceptSets)))
  if (unchanged && (nrow(current) == 0L || !is.null(previous))) {
    # Same rules as the last run: the flags are already in place.
    if (nrow(current) == 0L) {
      return(invisible(NULL))
    }
  }

  shortOkToUse <- if ("okToUse" %in% names(shortRubric)) as.logical(shortRubric$okToUse) else rep(TRUE, nrow(shortRubric))
  shortOkToUse[is.na(shortOkToUse)] <- TRUE
  toFlag <- affected & shortOkToUse
  toRestore <- (shortKey %in% previousKeys) & !affected & !shortOkToUse

  if (any(toFlag) || any(toRestore)) {
    concise <- ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
    canApply <- !is.null(concise) && !isTRUE(tryCatch(ithDiffConciseRubric(concise, shortRubric)$differs, error = function(e) TRUE))
    if (!canApply) {
      warning(
        conditionName, ": rules use empty concept sets but the concise rubric has unapplied edits, so okToUse was not ",
        "changed. Run applyConciseRubricEdits and rerun concept-set creation to flag them.",
        call. = FALSE, immediate. = TRUE
      )
    } else {
      conciseKey <- ruleKey(concise$rubric, concise$ruleId)
      if (!("okToUse" %in% names(concise))) {
        concise$okToUse <- TRUE
      }
      concise$okToUse <- as.logical(concise$okToUse)
      concise$okToUse[conciseKey %in% shortKey[toFlag]] <- FALSE
      concise$okToUse[conciseKey %in% shortKey[toRestore]] <- TRUE
      editedPath <- tempfile(fileext = ".csv")
      on.exit(unlink(editedPath), add = TRUE)
      utils::write.csv(concise, editedPath, row.names = FALSE)
      tryCatch(
        ithApplyConciseRubricEdits(
          conditionName = conditionName,
          outputDirectory = outputDirectory,
          editedConciseRubricPath = editedPath,
          llmClient = llmClient,
          updateSummary = FALSE,
          verbose = FALSE,
          refreshConceptSets = FALSE,
          repairConceptSetMismatches = FALSE
        ),
        error = function(e) {
          warning(conditionName, ": could not update okToUse for rules with empty concept sets: ", conditionMessage(e),
                  call. = FALSE, immediate. = TRUE)
        }
      )
      ithMaybeMessage(
        verbose, conditionName, ": flagged ", sum(toFlag), " rule(s) okToUse = FALSE for empty concept sets",
        if (any(toRestore)) paste0("; restored ", sum(toRestore), " rule(s)") else "", "."
      )
    }
  }

  if (!unchanged) {
    ithSaveVersioned(saveDirectory = conditionDir, objectToSave = current, fileName = sidecarName)
  }
  invisible(current)
}

# Warns, without stopping, about rules that use concept sets recorded as empty.
ithWarnRulesWithEmptyConceptSets <- function(conditionName, conditionDir) {
  recorded <- ithLoadLatest(conditionDir, paste0(conditionName, "_emptyConceptSetRules.csv"))
  if (is.null(recorded) || nrow(recorded) == 0L) {
    return(invisible(FALSE))
  }
  warning(
    conditionName, ": ", nrow(recorded), " rule(s) use concept sets recorded as empty and cannot match any record: ",
    paste0(recorded$rubric, " ", recorded$ruleId, " (", recorded$conceptSets, ")", collapse = "; "),
    ". They are flagged okToUse = FALSE in the rubric.",
    call. = FALSE, immediate. = TRUE
  )
  invisible(TRUE)
}
