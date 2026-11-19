# Feasibility probe: build several concept sets in parallel with RubricConcepts::createConceptSet.
# Nothing is written to the concept-set library; each set is saved under
# <outputDirectory>/parallelProbe/<run>/<conceptSetName>/ and a timing table is returned.
# No clinicalContext is sent, so the sets are built from the name alone.
#
# Example (live; uses your LLM and Databricks credentials):
# devtools::load_all("E:/Ithaca")
# source("extras/parallelConceptSetProbe.R")
# probe <- probeParallelConceptSets()          # 10 default names, 4 workers
# probe$results
# probe$summary

# Names taken from existing concise rubrics: 3 conditions, 4 drugs, 3 procedures.
probeDefaultConceptSetNames <- c(
  "acute_myocardial_infarction_diagnosis_codes",
  "ulcerative_colitis_diagnosis_codes",
  "sepsis_DIAGNOSIS_CODES",
  "azathioprine_medication_codes",
  "rituximab_MEDICATION_CODES",
  "tacrolimus_MEDICATION_CODES",
  "bortezomib_medication_codes",
  "cystoscopy_procedure_codes",
  "plasmapheresis_PROCEDURE_CODES",
  "bone_marrow_biopsy_procedure_codes"
)

# Runs on a worker. Each worker builds its LLM clients and database connection details once and
# reuses them for every concept set it is given.
probeBuildOneConceptSet <- function(conceptSetName, settings) {
  startedAt <- Sys.time()
  conceptCount <- NA_integer_
  status <- "ok"
  message <- ""
  attemptMessages <- character()
  tryCatch({
    if (is.null(settings$creator)) {
      state <- get0("probeWorkerState", envir = globalenv(), inherits = FALSE)
      if (is.null(state)) {
        config <- suppressMessages(Ithaca:::ithReadIthacaWorkflowConfig(settings$configPath))
        databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = globalenv())
        databaseId <- as.character(Ithaca:::ithCoalesce(config$conceptSetDatabaseId, names(databases)[[1]])[[1]])
        state <- list(
          database = databases[[databaseId]],
          llmClientReasoning = Ithaca:::ithGetLlmClientFromConfig(config, "reasoning"),
          llmClientNonReasoning = Ithaca:::ithGetLlmClientFromConfig(config, "nonReasoning")
        )
        assign("probeWorkerState", state, envir = globalenv())
        # RubricConcepts:::getDrugClass reads a global `llmClient` instead of receiving it as an argument.
        assign("llmClient", state$llmClientNonReasoning, envir = globalenv())
      }
      creator <- function(conceptSetTarget, outputDirectory) {
        RubricConcepts::createConceptSet(
          conceptSetTarget = conceptSetTarget,
          llmClientReasoning = state$llmClientReasoning,
          llmClientNonReasoning = state$llmClientNonReasoning,
          connectionDetails = state$database$connectionDetails,
          cdmDatabaseSchema = state$database$cdmDatabaseSchema,
          outputDirectory = outputDirectory,
          bucketSize = 100
        )
      }
    } else {
      creator <- settings$creator
    }

    outputDirectory <- file.path(settings$runDirectory, conceptSetName)
    dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
    # RubricConcepts writes ./lastPrompt/prompt.txt relative to the working directory, so isolate it per set.
    previousDirectory <- setwd(outputDirectory)
    on.exit(setwd(previousDirectory), add = TRUE)
    created <- withCallingHandlers(
      creator(conceptSetTarget = gsub("_", " ", conceptSetName), outputDirectory = outputDirectory),
      message = function(m) {
        text <- conditionMessage(m)
        if (grepl("failed|limit", text, ignore.case = TRUE)) {
          attemptMessages <<- c(attemptMessages, trimws(text))
        }
      }
    )

    candidates <- if (is.list(created)) c(list(created), list(created$finalConceptSet), list(created$conceptSet), as.list(created)) else list(created)
    for (candidate in candidates) {
      items <- tryCatch(candidate$items, error = function(e) NULL)
      if (is.list(items) && length(items) > 0L) {
        conceptCount <- length(items)
        break
      }
    }
    if (is.na(conceptCount)) {
      status <- "empty"
    }
  }, error = function(e) {
    status <<- "error"
    firstAttempt <- if (length(attemptMessages) > 0L) paste0(attemptMessages[[1]], " | ") else ""
    message <<- paste0(firstAttempt, conditionMessage(e))
    errorDirectory <- file.path(settings$runDirectory, conceptSetName)
    dir.create(errorDirectory, recursive = TRUE, showWarnings = FALSE)
    writeLines(c(conditionMessage(e), "", format(conditionCall(e)), "", attemptMessages),
               file.path(errorDirectory, "error.txt"))
  })

  endedAt <- Sys.time()
  data.frame(
    conceptSetName = conceptSetName,
    workerPid = Sys.getpid(),
    status = status,
    conceptCount = conceptCount,
    startSeconds = as.numeric(difftime(startedAt, settings$runStart, units = "secs")),
    endSeconds = as.numeric(difftime(endedAt, settings$runStart, units = "secs")),
    seconds = as.numeric(difftime(endedAt, startedAt, units = "secs")),
    message = substr(gsub("[\r\n]+", " ", message), 1, 300),
    stringsAsFactors = FALSE
  )
}

#' Probe parallel concept-set creation
#'
#' @param conceptSetNames Concept set names to build (default: 10 names from existing rubrics).
#' @param workers Number of parallel worker sessions.
#' @param configPath Path to config.yaml (used for the output folder, database and LLM settings).
#' @param packagePath Ithaca source folder the workers load, so they run the current code.
#' @param creator Optional replacement for RubricConcepts::createConceptSet, a function of
#'   (conceptSetTarget, outputDirectory). Used to test the parallel mechanics without LLM calls.
#' @return A list with `results` (one row per concept set: worker, status, seconds, start/end
#'   offsets), `summary` (elapsed time, summed task time, and speed-up) and `runDirectory`.
probeParallelConceptSets <- function(conceptSetNames = probeDefaultConceptSetNames,
                                     workers = 4L,
                                     configPath = "config.yaml",
                                     packagePath = "E:/Ithaca",
                                     creator = NULL) {
  stopifnot(is.character(conceptSetNames), length(conceptSetNames) > 0L, !anyDuplicated(conceptSetNames))
  workers <- max(1L, min(as.integer(workers), length(conceptSetNames)))

  rawConfig <- yaml::read_yaml(configPath)
  outputRoot <- Ithaca:::ithCoalesce(rawConfig$general$outputDirectory, rawConfig$defaults$contextual$outputDirectory)
  if (is.null(outputRoot)) stop("outputDirectory not found in ", configPath)
  runDirectory <- file.path(as.character(outputRoot[[1]]), "parallelProbe", format(Sys.time(), "%Y%m%d%H%M%S"))
  dir.create(runDirectory, recursive = TRUE, showWarnings = FALSE)

  settings <- list(
    configPath = normalizePath(configPath, winslash = "/"),
    runDirectory = runDirectory,
    runStart = Sys.time(),
    creator = creator
  )

  cluster <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterCall(cluster, function(path) {
    pkgload::load_all(path, quiet = TRUE)
    invisible(NULL)
  }, packagePath)

  message("Building ", length(conceptSetNames), " concept sets with ", workers, " workers...")
  wallStart <- Sys.time()
  # Manual load-balanced dispatch (as clusterApplyLB does) so progress prints as each set finishes.
  total <- length(conceptSetNames)
  results <- vector("list", total)
  dispatch <- function(node, i) {
    parallel:::sendCall(cluster[[node]], probeBuildOneConceptSet,
                        list(conceptSetNames[[i]], settings), tag = i)
    message(sprintf("[%s] started   %d/%d %s (worker %d)", format(Sys.time(), "%H:%M:%S"),
                    i, total, conceptSetNames[[i]], node))
  }
  nextIndex <- 1L
  for (node in seq_len(min(workers, total))) {
    dispatch(node, nextIndex)
    nextIndex <- nextIndex + 1L
  }
  for (done in seq_len(total)) {
    reply <- parallel:::recvOneResult(cluster)
    value <- reply$value
    if (inherits(value, "try-error") || !is.data.frame(value)) {
      value <- data.frame(conceptSetName = conceptSetNames[[reply$tag]], workerPid = NA_integer_,
                          status = "error", conceptCount = NA_integer_, seconds = NA_real_,
                          startSeconds = NA_real_, endSeconds = NA_real_,
                          message = paste(as.character(value), collapse = " "))
    }
    results[[reply$tag]] <- value
    message(sprintf("[%s] finished  %d/%d %s: %s%s%s", format(Sys.time(), "%H:%M:%S"), done, total,
                    conceptSetNames[[reply$tag]], value$status,
                    if (is.finite(value$seconds)) sprintf(" in %.0fs", value$seconds) else "",
                    if (identical(value$status, "error")) paste0(" - ", substr(value$message, 1, 200)) else ""))
    if (nextIndex <= total) {
      dispatch(reply$node, nextIndex)
      nextIndex <- nextIndex + 1L
    }
  }
  elapsed <- as.numeric(difftime(Sys.time(), wallStart, units = "secs"))

  results <- do.call(rbind, results)
  summary <- data.frame(
    conceptSets = nrow(results),
    workers = workers,
    ok = sum(results$status == "ok"),
    failed = sum(results$status != "ok"),
    elapsedSeconds = round(elapsed, 1),
    summedTaskSeconds = round(sum(results$seconds), 1),
    estimatedSpeedup = round(sum(results$seconds) / elapsed, 2)
  )
  message("Done in ", summary$elapsedSeconds, "s (tasks summed to ", summary$summedTaskSeconds, "s; speed-up ", summary$estimatedSpeedup, "x).")
  list(results = results, summary = summary, runDirectory = runDirectory)
}
