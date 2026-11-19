# Builds one concept set with RubricConcepts. Returns list(conceptSet, isPlaceholder, message, outcome);
# outcome is ok, error (RubricConcepts raised), empty (a result with no concepts) or unreadable.
# The clinical context is deliberately not passed to the builder: it made some sets specific to
# the condition that requested them. Failures yield the placeholder concept set (36433327).
# Placeholder concept set (conceptId 36433327) stored for sets that could not be built or are empty.
ithPlaceholderConceptSet <- function() {
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

ithCreateConceptSetSafe <- function(conceptSetTarget,
                                    csDirectory,
                                    database,
                                    llmClientReasoning,
                                    llmClientNonReasoning,
                                    clinicalDefinition = "",
                                    bucketSize = 100,
                                    verbose = TRUE) {
  buildPlaceholderConceptSet <- ithPlaceholderConceptSet

  sawEmptyResult <- FALSE
  extractReadableConceptSet <- function(createResult) {
    candidates <- list()
    if (is.list(createResult)) {
      if (!is.null(createResult$finalConceptSet)) {
        candidates <- c(candidates, list(createResult$finalConceptSet))
      }
      if (!is.null(createResult$conceptSet)) {
        candidates <- c(candidates, list(createResult$conceptSet))
      }
      if (length(createResult) >= 2) {
        candidates <- c(candidates, list(createResult[[2]]))
      }
      if (length(createResult) >= 1) {
        candidates <- c(candidates, list(createResult[[1]]))
      }
    } else {
      candidates <- list(createResult)
    }

    for (candidate in candidates) {
      prepared <- tryCatch(ithPrepareConceptSetForResolution(candidate), error = function(e) NULL)
      if (is.list(prepared) && !is.null(prepared$items) && length(prepared$items) > 0) {
        return(candidate)
      }
      if ((is.list(prepared) && !is.null(prepared$items)) || length(candidate) == 0L || (is.data.frame(candidate) && nrow(candidate) == 0L)) {
        sawEmptyResult <<- TRUE
      }
    }

    NULL
  }

  rawResult <- NULL
  failureMessage <- NULL
  for (attempt in 1:2) {
    rawResult <- tryCatch(
      Phenelope::createConceptSet(
        name = conceptSetTarget,
        seedConceptIds = NULL,
        clinicalDefinition = clinicalDefinition,
        llmClient = llmClientReasoning,
        connectionDetails = database$connectionDetails,
        vocabDatabaseSchema = database$cdmDatabaseSchema,
        conceptAdjudicator = Phenelope::DefaultConceptAdjudicator$new(batchSize = 100, nForQuickScreen = 1000),
        conceptRecommender = Phenelope::HecateConceptRecommender$new(minCount = 0),
        batchSize = bucketSize,
        cacheFolder = csDirectory
      ),
      error = function(e) {
        failureMessage <<- e$message
        NULL
      }
    )
    if (!is.null(rawResult)) {
      break
    }
    if (attempt == 1) {
      ithMaybeMessage(verbose, "Phenelope failed for '", conceptSetTarget, "'; retrying once.")
    }
  }

  if (is.null(rawResult)) {
    ithMaybeMessage(
      verbose,
      "Phenelope failed while creating concept set '",
      conceptSetTarget,
      "'. Error: ",
      if (is.null(failureMessage)) "unknown error" else failureMessage,
      ". Using placeholder conceptId 36433327."
    )
    return(list(conceptSet = buildPlaceholderConceptSet(), isPlaceholder = TRUE, message = "Phenelope failed while creating concept set", outcome = "error"))
  }

  # rawResult <- tryCatch(
  #   do.call(RubricConcepts::createConceptSet, createArgs),
  #   error = function(e) e
  # )

  extracted <- extractReadableConceptSet(rawResult)
  if (is.character(extracted) && length(extracted) == 1L) {
    extracted <- tryCatch(jsonlite::fromJSON(extracted, simplifyVector = FALSE), error = function(e) NULL)
  }
  if (!is.null(extracted)) {
    return(list(conceptSet = extracted, isPlaceholder = FALSE, message = "", outcome = "ok"))
  }

  ithMaybeMessage(verbose, "Phenelope returned an empty/unreadable concept set for '", conceptSetTarget, "'. Replacing with placeholder conceptId 36433327.")
  list(
    conceptSet = buildPlaceholderConceptSet(),
    isPlaceholder = TRUE,
    message = "Phenelope returned an empty or unreadable concept set",
    outcome = if (sawEmptyResult) "empty" else "unreadable"
  )
}

ithConceptSetBuildResultPath <- function(conceptSetFolder, conceptSetName) {
  file.path(conceptSetFolder, conceptSetName, "result.rds")
}

ithConceptSetComplexityScore <- function(conceptSetName) {
  tokens <- unlist(strsplit(tolower(gsub("[^a-z0-9]+", "_", conceptSetName)), "_", fixed = TRUE))
  tokens <- tokens[nzchar(tokens)]
  suffix <- tail(tokens, 2L)
  categoryScore <- if ("medication" %in% suffix) {
    6
  } else if ("condition" %in% suffix || "diagnosis" %in% suffix) {
    5
  } else if ("procedure" %in% suffix) {
    4
  } else if ("measurement" %in% suffix) {
    3
  } else if ("visit" %in% suffix) {
    2
  } else {
    1
  }

  complexity <- categoryScore + min(3, max(0, length(tokens) - 3L) * 0.5)
  if (any(tokens %in% c("trauma", "injury", "syndrome", "complication", "manifestation", "acute"))) {
    complexity <- complexity + 2
  }
  if (any(tokens %in% c("oral", "intravenous", "iv", "inhaled", "topical", "injectable", "route"))) {
    complexity <- complexity + 2
  }
  if (any(tokens %in% c("potency", "potent", "strength", "dose", "dosage", "formulation"))) {
    complexity <- complexity + 2
  }
  if (any(tokens %in% c("corticosteroid", "corticosteroids", "antibiotic", "antibiotics", "anticoagulant", "immunosuppressant"))) {
    complexity <- complexity + 2
  }

  as.numeric(complexity)
}

ithConceptSetTimingPath <- function(conceptSetFolder) {
  file.path(conceptSetFolder, "conceptSetBuildTimes.csv")
}

ithReadConceptSetBuildTimes <- function(conceptSetFolder) {
  path <- ithConceptSetTimingPath(conceptSetFolder)
  if (!file.exists(path)) {
    return(data.frame())
  }
  times <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (!all(c("conceptSetName", "seconds") %in% names(times))) {
    return(data.frame())
  }
  times$seconds <- suppressWarnings(as.numeric(times$seconds))
  times <- times[!is.na(times$seconds) & is.finite(times$seconds) & times$seconds > 0, , drop = FALSE]
  times[!duplicated(times$conceptSetName, fromLast = TRUE), , drop = FALSE]
}

ithConceptSetDispatchPlan <- function(conceptSetNames, conceptSetFolder) {
  history <- ithReadConceptSetBuildTimes(conceptSetFolder)
  found <- match(conceptSetNames, history$conceptSetName)
  historical <- !is.na(found)
  estimates <- vapply(seq_along(conceptSetNames), function(index) {
    if (historical[[index]]) history$seconds[[found[[index]]]] else 10 * ithConceptSetComplexityScore(conceptSetNames[[index]])
  }, numeric(1))
  order <- order(-estimates, !historical, seq_along(conceptSetNames))
  list(
    order = order,
    estimates = data.frame(
      conceptSetName = conceptSetNames,
      estimatedSeconds = estimates,
      estimateSource = ifelse(historical, "history", "heuristic"),
      stringsAsFactors = FALSE
    )
  )
}

ithConceptSetDispatchOrder <- function(conceptSetNames, conceptSetFolder) {
  ithConceptSetDispatchPlan(conceptSetNames, conceptSetFolder)$order
}

ithSaveConceptSetBuildTimes <- function(conceptSetFolder, results) {
  completed <- results[results$status == "ok" & is.finite(results$seconds) & results$seconds > 0, , drop = FALSE]
  if (nrow(completed) == 0L) {
    return(invisible(NULL))
  }
  previous <- ithReadConceptSetBuildTimes(conceptSetFolder)
  columns <- c("conceptSetName", "seconds", "outcome", "recordedAt")
  current <- completed[, intersect(c("conceptSetName", "seconds", "outcome"), names(completed)), drop = FALSE]
  current$recordedAt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (nrow(previous) > 0L) {
    previous$recordedAt <- if ("recordedAt" %in% names(previous)) previous$recordedAt else NA_character_
    previous$outcome <- if ("outcome" %in% names(previous)) previous$outcome else "ok"
    previous <- previous[, columns, drop = FALSE]
    current <- rbind(previous, current[, columns, drop = FALSE])
  } else {
    current <- current[, columns, drop = FALSE]
  }
  current <- current[!duplicated(current$conceptSetName, fromLast = TRUE), , drop = FALSE]
  temporaryPath <- tempfile("conceptSetBuildTimes_", tmpdir = conceptSetFolder, fileext = ".csv")
  utils::write.csv(current, temporaryPath, row.names = FALSE)
  targetPath <- ithConceptSetTimingPath(conceptSetFolder)
  if (file.exists(targetPath)) unlink(targetPath)
  file.rename(temporaryPath, targetPath)
  invisible(targetPath)
}

# Runs on a worker process. A finished set is saved to <conceptSetFolder>/<name>/result.rds (written
# only on success), and a rerun reuses it. Each set runs in its own working directory because
# RubricConcepts writes ./lastPrompt/prompt.txt relative to the working directory.
ithBuildConceptSetOnWorker <- function(conceptSetName, settings) {
  startedAt <- Sys.time()
  buildDirectory <- file.path(settings$conceptSetFolder, conceptSetName)
  dir.create(buildDirectory, recursive = TRUE, showWarnings = FALSE)
  resultPath <- ithConceptSetBuildResultPath(settings$conceptSetFolder, conceptSetName)
  errorPath <- file.path(buildDirectory, "error.txt")

  finish <- function(status, message = "", outcome = if (identical(status, "error")) "error" else "ok") {
    data.frame(
      conceptSetName = conceptSetName,
      status = status,
      outcome = outcome,
      seconds = as.numeric(difftime(Sys.time(), startedAt, units = "secs")),
      message = substr(gsub("[\r\n]+", " ", message), 1, 300),
      stringsAsFactors = FALSE
    )
  }

  if (file.exists(resultPath)) {
    return(finish("reused"))
  }

  previousDirectory <- setwd(buildDirectory)
  on.exit(setwd(previousDirectory), add = TRUE)
  unlink(errorPath)

  failureOutcome <- "error"
  tryCatch({
    built <- if (!is.null(settings$builder)) {
      settings$builder(conceptSetTarget = gsub("_", " ", conceptSetName), csDirectory = buildDirectory)
    } else {
      state <- get0("ithConceptSetWorkerState", envir = globalenv(), inherits = FALSE)
      if (is.null(state)) {
        state <- list(
          llmClientReasoning = ithGetLlmClientFromConfig(settings$llmConfig, "reasoning"),
          llmClientNonReasoning = ithGetLlmClientFromConfig(settings$llmConfig, "nonReasoning")
        )
        assign("ithConceptSetWorkerState", state, envir = globalenv())
      }
      ithCreateConceptSetSafe(
        conceptSetTarget = gsub("_", " ", conceptSetName),
        csDirectory = buildDirectory,
        database = settings$database,
        llmClientReasoning = state$llmClientReasoning,
        llmClientNonReasoning = state$llmClientNonReasoning,
        bucketSize = settings$bucketSize,
        verbose = FALSE
      )
    }

    if (isTRUE(built$isPlaceholder)) {
      failureOutcome <- if (is.null(built$outcome)) "error" else as.character(built$outcome[[1]])
      stop(built$message)
    }

    temporaryPath <- paste0(resultPath, ".tmp")
    saveRDS(built$conceptSet, temporaryPath)
    file.rename(temporaryPath, resultPath)
    finish("ok")
  }, error = function(e) {
    writeLines(paste0("[", failureOutcome, "] ", conditionMessage(e)), errorPath)
    finish("error", conditionMessage(e), failureOutcome)
  })
}

# Loads Ithaca on a worker: from source when the main session loaded it with pkgload, otherwise
# from the installed library.
ithLoadIthacaOnWorker <- function(packagePath, isDevelopment, workerOptions = list()) {
  if (length(workerOptions) > 0L) {
    options(workerOptions)
  }
  if (isTRUE(isDevelopment)) {
    pkgload::load_all(packagePath, quiet = TRUE)
  } else {
    loadNamespace("Ithaca")
  }
  invisible(NULL)
}

# Builds concept sets in parallel on a PSOCK cluster and returns one row per set with its status.
# Nothing is written to the concept set library here; the caller does that after all sets finish.
# `builder` replaces the RubricConcepts call (used by tests); it must be self-contained because it is
# sent to the workers.
ithBuildConceptSetsParallel <- function(conceptSetNames,
                                        conceptSetFolder,
                                        workers = 4L,
                                        database = NULL,
                                        llmConfig = list(),
                                        bucketSize = 100,
                                        builder = NULL,
                                        verbose = TRUE) {
  conceptSetNames <- unique(as.character(conceptSetNames))
  emptyResult <- data.frame(
    conceptSetName = character(), status = character(), outcome = character(), seconds = numeric(), message = character(),
    stringsAsFactors = FALSE
  )
  if (length(conceptSetNames) == 0L) {
    return(emptyResult)
  }

  dir.create(conceptSetFolder, recursive = TRUE, showWarnings = FALSE)
  workers <- max(1L, min(as.integer(workers), length(conceptSetNames)))

  settings <- list(
    conceptSetFolder = conceptSetFolder,
    database = database,
    llmConfig = llmConfig,
    bucketSize = bucketSize,
    builder = builder
  )

  ithMaybeMessage(verbose, "Building ", length(conceptSetNames), " concept set(s) with ", workers, " worker(s) into: ", conceptSetFolder)
  dispatchPlan <- ithConceptSetDispatchPlan(conceptSetNames, conceptSetFolder)
  if (verbose) {
    ithMaybeMessage(
      verbose,
      "Concept-set dispatch order: ",
      paste(conceptSetNames[dispatchPlan$order], collapse = ", ")
    )
  }
  cluster <- ithStartWorkerCluster(workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  total <- length(conceptSetNames)
  results <- vector("list", total)
  # Load-balanced dispatch as in parallel::clusterApplyLB, so progress prints as each set finishes.
  dispatch <- function(node, index) {
    parallel:::sendCall(
      cluster[[node]], Ithaca:::ithBuildConceptSetOnWorker,
      list(conceptSetNames[[index]], settings), tag = index
    )
    ithMaybeMessage(
      verbose, sprintf("[%s] started   %d/%d %s", format(Sys.time(), "%H:%M:%S"), index, total, conceptSetNames[[index]])
    )
  }

  nextPosition <- 1L
  for (node in seq_len(workers)) {
    index <- dispatchPlan$order[[nextPosition]]
    dispatch(node, index)
    nextPosition <- nextPosition + 1L
  }
  for (done in seq_len(total)) {
    reply <- parallel:::recvOneResult(cluster)
    value <- reply$value
    if (!is.data.frame(value)) {
      value <- data.frame(
        conceptSetName = conceptSetNames[[reply$tag]], status = "error", outcome = "error", seconds = NA_real_,
        message = substr(paste(as.character(value), collapse = " "), 1, 300), stringsAsFactors = FALSE
      )
    }
    results[[reply$tag]] <- value
    ithMaybeMessage(
      verbose,
      sprintf(
        "[%s] finished  %d/%d %s: %s%s%s", format(Sys.time(), "%H:%M:%S"), done, total,
        conceptSetNames[[reply$tag]], value$status,
        if (is.finite(value$seconds)) sprintf(" in %.0fs", value$seconds) else "",
        if (identical(value$status, "error")) paste0(" - ", substr(value$message, 1, 200)) else ""
      )
    )
    if (nextPosition <= total) {
      index <- dispatchPlan$order[[nextPosition]]
      dispatch(reply$node, index)
      nextPosition <- nextPosition + 1L
    }
  }

  results <- do.call(rbind, results)
  ithSaveConceptSetBuildTimes(conceptSetFolder, results)
  results
}
