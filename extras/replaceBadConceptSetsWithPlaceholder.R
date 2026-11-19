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

buildPlaceholderConceptSet <- function(placeholderConceptId = 36433327) {
  list(
    items = list(
      list(
        concept = list(
          CONCEPT_ID = as.integer(placeholderConceptId),
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

replaceBadConceptSetsWithPlaceholder <- function(libraryPath,
                                                 outputPath = NULL,
                                                 placeholderConceptId = 36433327,
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
  auditPre <- data.frame(
    conceptSetName = as.character(conceptTable$conceptSetName),
    cleanName = as.character(conceptTable$cleanName),
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )

  badIdx <- which(auditPre$status == "bad")
  if (isTRUE(verbose)) {
    message("Malformed concept sets identified: ", length(badIdx))
  }

  placeholder <- buildPlaceholderConceptSet(placeholderConceptId = placeholderConceptId)
  if (length(badIdx) > 0) {
    for (rowUp in badIdx) {
      conceptTable$conceptSet[[rowUp]] <- placeholder
    }
  }

  libraryObject[[1]] <- conceptTable

  reasonPost <- character(nrow(conceptTable))
  for (rowUp in seq_len(nrow(conceptTable))) {
    prepared <- tryCatch(
      prepareConceptSetForResolutionLocal(conceptTable$conceptSet[[rowUp]]),
      error = function(e) structure(list(.err = e$message), class = "prep_error")
    )

    if (inherits(prepared, "prep_error")) {
      reasonPost[[rowUp]] <- paste0("prepare_error:", prepared$.err)
    } else {
      reasonPost[[rowUp]] <- validatePreparedConceptSetLocal(prepared)
    }
  }

  statusPost <- ifelse(reasonPost == "ok", "ok", "bad")
  auditPost <- data.frame(
    conceptSetName = as.character(conceptTable$conceptSetName),
    cleanName = as.character(conceptTable$cleanName),
    status = statusPost,
    reason = reasonPost,
    stringsAsFactors = FALSE
  )

  if (is.null(outputPath)) {
    base <- sub("\\.rds$", "", basename(libraryPath))
    outputPath <- file.path(dirname(libraryPath), paste0(base, "_placeholder_fixed.rds"))
  }

  saveRDS(libraryObject, outputPath)

  baseOut <- sub("\\.rds$", "", outputPath)
  auditPrePath <- paste0(baseOut, "_audit_pre.csv")
  auditPostPath <- paste0(baseOut, "_audit_post.csv")
  replacedPath <- paste0(baseOut, "_replaced_names.csv")

  utils::write.csv(auditPre, auditPrePath, row.names = FALSE)
  utils::write.csv(auditPost, auditPostPath, row.names = FALSE)
  utils::write.csv(auditPre[auditPre$status == "bad", c("conceptSetName", "reason"), drop = FALSE], replacedPath, row.names = FALSE)

  if (isTRUE(verbose)) {
    message("Saved fixed library: ", outputPath)
    message("Saved pre-audit: ", auditPrePath)
    message("Saved post-audit: ", auditPostPath)
    message("Saved replaced names: ", replacedPath)
    message("Remaining malformed after replacement: ", sum(auditPost$status == "bad"))
  }

  list(
    outputPath = outputPath,
    replacedCount = length(badIdx),
    remainingBadCount = sum(auditPost$status == "bad"),
    auditPrePath = auditPrePath,
    auditPostPath = auditPostPath,
    replacedPath = replacedPath,
    auditPre = auditPre,
    auditPost = auditPost
  )
}

