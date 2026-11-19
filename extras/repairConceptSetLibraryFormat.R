prepareConceptSetForResolutionLocal <- function(conceptSetJson) {
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
    itemCount <- max(
      length(parsed$isExcluded),
      length(parsed$includeDescendants),
      length(parsed$includeMapped)
    )

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
    looksLikeBareItems <- is.null(names(parsed)) &&
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

validatePreparedConceptSetLocal <- function(prepared) {
  if (!is.list(prepared)) {
    return("prepared_not_list")
  }

  if (is.null(prepared$items)) {
    return("missing_items")
  }

  if (!is.list(prepared$items) || length(prepared$items) == 0) {
    return("empty_or_nonlist_items")
  }

  bad <- 0L
  for (it in prepared$items) {
    if (!is.list(it)) {
      bad <- bad + 1L
      next
    }

    needed <- c("concept", "isExcluded", "includeDescendants", "includeMapped")
    if (!all(needed %in% names(it))) {
      bad <- bad + 1L
      next
    }

    concept <- it$concept
    conceptOk <- is.list(concept) || is.data.frame(concept)
    if (!conceptOk) {
      bad <- bad + 1L
    }
  }

  if (bad > 0) {
    return(paste0("bad_item_shapes:", bad))
  }

  "ok"
}

repairConceptSetLibraryFormat <- function(libraryPath,
                                          connectionDetails = NULL,
                                          cdmDatabaseSchema = NULL,
                                          llmClientReasoning = ithGetLlmClientFromConfig(list(), "reasoning"),
                                          llmClientNonReasoning = ithGetLlmClientFromConfig(list(), "nonReasoning"),
                                          clinicalContext = "",
                                          outputDirectory = NULL,
                                          dryRun = TRUE,
                                          verbose = TRUE) {
  if (!file.exists(libraryPath)) {
    stop("Library file not found: ", libraryPath)
  }

  libraryObject <- readRDS(libraryPath)
  if (!is.list(libraryObject) || length(libraryObject) < 2) {
    stop("Library object is not in expected two-table format")
  }

  conceptTable <- libraryObject[[1]]
  if (!is.data.frame(conceptTable) || !all(c("conceptSetName", "cleanName", "conceptSet") %in% names(conceptTable))) {
    stop("Concept table is missing required columns")
  }

  reason <- character(nrow(conceptTable))
  for (rowUp in seq_len(nrow(conceptTable))) {
    prepared <- tryCatch(
      prepareConceptSetForResolutionLocal(conceptTable$conceptSet[[rowUp]]),
      error = function(e) structure(list(.err = e$message), class = "prep_error")
    )

    if (inherits(prepared, "prep_error")) {
      reason[[rowUp]] <- paste0("prepare_error:", prepared$.err)
    } else {
      reason[[rowUp]] <- validatePreparedConceptSetLocal(prepared)
    }
  }

  status <- ifelse(reason == "ok", "ok", "bad")
  audit <- data.frame(
    conceptSetName = as.character(conceptTable$conceptSetName),
    cleanName = as.character(conceptTable$cleanName),
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )

  badRows <- audit[audit$status == "bad", , drop = FALSE]
  if (isTRUE(verbose)) {
    message("Total concept sets: ", nrow(audit))
    message("Readable concept sets: ", sum(audit$status == "ok"))
    message("Malformed concept sets: ", nrow(badRows))
  }

  if (isTRUE(dryRun) || nrow(badRows) == 0) {
    return(list(
      audit = audit,
      badRows = badRows,
      repairedLibraryPath = NULL,
      repairedCount = 0L
    ))
  }

  if (is.null(connectionDetails) || is.null(cdmDatabaseSchema)) {
    stop("connectionDetails and cdmDatabaseSchema are required when dryRun = FALSE")
  }

  if (!requireNamespace("RubricConcepts", quietly = TRUE)) {
    stop("Package RubricConcepts is required for regeneration")
  }

  regenerateConceptSetSafe <- function(conceptSetTarget) {
    tryCatch(
      RubricConcepts::createConceptSet(
        conceptSetTarget = conceptSetTarget,
        llmClientReasoning = llmClientReasoning,
        llmClientNonReasoning = llmClientNonReasoning,
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        clinicalContext = clinicalContext,
        clinicalDefinition = "",
        outputDirectory = tempfile(pattern = paste0("cs_", gsub("[^A-Za-z0-9_]", "_", conceptSetTarget), "_")),
        bucketSize = 100
      ),
      error = function(e) {
        if (grepl("object 'llmClient' not found", e$message, fixed = TRUE)) {
          if (isTRUE(verbose)) {
            message("Detected legacy RubricConcepts signature; retrying with llmClient for concept set target: ", conceptSetTarget)
          }
          return(RubricConcepts::createConceptSet(
            conceptSetTarget = conceptSetTarget,
            llmClient = llmClientReasoning,
            connectionDetails = connectionDetails,
            cdmDatabaseSchema = cdmDatabaseSchema,
            clinicalContext = clinicalContext,
            clinicalDefinition = "",
            outputDirectory = tempfile(pattern = paste0("cs_", gsub("[^A-Za-z0-9_]", "_", conceptSetTarget), "_")),
            bucketSize = 100
          ))
        }

        stop(e)
      }
    )
  }

  buildZeroSubjectConceptSet <- function() {
    list(
      items = list(
        list(
          concept = list(
            CONCEPT_ID = 36433327,
            CONCEPT_NAME = "NO_CONCEPT_PLACEHOLDER",
            DOMAIN_ID = "Observation",
            VOCABULARY_ID = "None",
            CONCEPT_CLASS_ID = "Undefined",
            STANDARD_CONCEPT = "",
            INVALID_REASON = ""
          ),
          isExcluded = FALSE,
          includeDescendants = FALSE,
          includeMapped = FALSE
        )
      )
    )
  }

  extractRegeneratedConceptSet <- function(regenerated) {
    candidates <- list()

    if (is.list(regenerated)) {
      if (!is.null(regenerated$finalConceptSet)) {
        candidates <- c(candidates, list(regenerated$finalConceptSet))
      }
      if (!is.null(regenerated$conceptSet)) {
        candidates <- c(candidates, list(regenerated$conceptSet))
      }
      if (length(regenerated) >= 2) {
        candidates <- c(candidates, list(regenerated[[2]]))
      }
      if (length(regenerated) >= 1) {
        candidates <- c(candidates, list(regenerated[[1]]))
      }
    } else {
      candidates <- list(regenerated)
    }

    if (length(candidates) == 0) {
      return(list(ok = FALSE, conceptSet = NULL, reason = "no_candidates"))
    }

    for (candidate in candidates) {
      prepared <- tryCatch(
        prepareConceptSetForResolutionLocal(candidate),
        error = function(e) NULL
      )
      validity <- validatePreparedConceptSetLocal(prepared)
      if (identical(validity, "ok")) {
        return(list(ok = TRUE, conceptSet = candidate, reason = "ok"))
      }
    }

    list(ok = FALSE, conceptSet = NULL, reason = "no_readable_candidate")
  }

  computeAudit <- function(conceptTableInput) {
    reasonLocal <- character(nrow(conceptTableInput))
    for (rowUp in seq_len(nrow(conceptTableInput))) {
      prepared <- tryCatch(
        prepareConceptSetForResolutionLocal(conceptTableInput$conceptSet[[rowUp]]),
        error = function(e) structure(list(.err = e$message), class = "prep_error")
      )

      if (inherits(prepared, "prep_error")) {
        reasonLocal[[rowUp]] <- paste0("prepare_error:", prepared$.err)
      } else {
        reasonLocal[[rowUp]] <- validatePreparedConceptSetLocal(prepared)
      }
    }

    statusLocal <- ifelse(reasonLocal == "ok", "ok", "bad")
    data.frame(
      conceptSetName = as.character(conceptTableInput$conceptSetName),
      cleanName = as.character(conceptTableInput$cleanName),
      status = statusLocal,
      reason = reasonLocal,
      stringsAsFactors = FALSE
    )
  }

  repairedCount <- 0L
  skippedCount <- 0L
  repairLog <- data.frame(
    conceptSetName = character(),
    action = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )

  for (rowUp in seq_len(nrow(badRows))) {
    conceptSetName <- badRows$conceptSetName[[rowUp]]
    tableIdx <- which(tolower(as.character(conceptTable$conceptSetName)) == tolower(conceptSetName))
    if (length(tableIdx) == 0) {
      skippedCount <- skippedCount + 1L
      repairLog <- rbind(
        repairLog,
        data.frame(
          conceptSetName = conceptSetName,
          action = "skipped",
          detail = "not_found_in_table",
          stringsAsFactors = FALSE
        )
      )
      next
    }

    if (isTRUE(verbose)) {
      message("Regenerating malformed concept set: ", conceptSetName)
    }

    regenerated <- tryCatch(
      regenerateConceptSetSafe(gsub("_", " ", conceptSetName)),
      error = function(e) structure(list(.err = e$message), class = "regen_error")
    )

    if (inherits(regenerated, "regen_error")) {
      skippedCount <- skippedCount + 1L
      repairLog <- rbind(
        repairLog,
        data.frame(
          conceptSetName = conceptSetName,
          action = "skipped",
          detail = paste0("regenerate_error:", regenerated$.err),
          stringsAsFactors = FALSE
        )
      )
      if (isTRUE(verbose)) {
        message("Skipping concept set due to regeneration error: ", conceptSetName)
      }
      next
    }

    extracted <- extractRegeneratedConceptSet(regenerated)
    if (!isTRUE(extracted$ok)) {
      placeholderConceptSet <- buildZeroSubjectConceptSet()
      conceptTable$conceptSet[[tableIdx[[1]]]] <- placeholderConceptSet
      repairedCount <- repairedCount + 1L
      repairLog <- rbind(
        repairLog,
        data.frame(
          conceptSetName = conceptSetName,
          action = "repaired",
          detail = paste0("replaced_with_zero_subject_placeholder:", extracted$reason),
          stringsAsFactors = FALSE
        )
      )
      if (isTRUE(verbose)) {
        message("Regenerated output empty/unreadable; replaced with zero-subject placeholder concept set: ", conceptSetName)
      }
      next
    }

    conceptTable$conceptSet[[tableIdx[[1]]]] <- extracted$conceptSet
    repairedCount <- repairedCount + 1L
    repairLog <- rbind(
      repairLog,
      data.frame(
        conceptSetName = conceptSetName,
        action = "repaired",
        detail = "ok",
        stringsAsFactors = FALSE
      )
    )
  }

  libraryObject[[1]] <- conceptTable
  auditPost <- computeAudit(conceptTable)
  badRowsPost <- auditPost[auditPost$status == "bad", , drop = FALSE]

  if (is.null(outputDirectory)) {
    outputDirectory <- dirname(libraryPath)
  }
  dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)

  baseName <- basename(libraryPath)
  repairedName <- sub("\\.rds$", "", baseName)
  repairedName <- paste0(repairedName, "_repaired.rds")
  repairedPath <- file.path(outputDirectory, repairedName)
  saveRDS(libraryObject, repairedPath)

  auditPath <- file.path(outputDirectory, paste0(sub("\\.rds$", "", baseName), "_format_audit_pre.csv"))
  utils::write.csv(audit, auditPath, row.names = FALSE)
  auditPostPath <- file.path(outputDirectory, paste0(sub("\\.rds$", "", baseName), "_format_audit_post.csv"))
  utils::write.csv(auditPost, auditPostPath, row.names = FALSE)
  repairLogPath <- file.path(outputDirectory, paste0(sub("\\.rds$", "", baseName), "_repair_log.csv"))
  utils::write.csv(repairLog, repairLogPath, row.names = FALSE)

  if (isTRUE(verbose)) {
    message("Repair complete. Repaired: ", repairedCount, "; skipped: ", skippedCount, "; remaining malformed: ", nrow(badRowsPost))
    message("Saved repaired library: ", repairedPath)
    message("Saved pre-repair audit: ", auditPath)
    message("Saved post-repair audit: ", auditPostPath)
    message("Saved repair log: ", repairLogPath)
  }

  list(
    audit = audit,
    auditPost = auditPost,
    badRows = badRows,
    badRowsPost = badRowsPost,
    repairedLibraryPath = repairedPath,
    repairedCount = repairedCount,
    skippedCount = skippedCount,
    auditPath = auditPath,
    auditPostPath = auditPostPath,
    repairLog = repairLog,
    repairLogPath = repairLogPath
  )
}
