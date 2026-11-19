# Config-driven phenotype algorithm rubric reports (formerly the body of
# testPhenotypeAlgorithmRubricReportMultipleDatabases.R).

ithConditionSpecName <- function(conditionSpec) {
  as.character(ithCoalesce(conditionSpec$rubricName, conditionSpec$testCondition)[[1]])
}

ithPhenotypeReportCohortIds <- function(conditionSpec) {
  phenotypeCohortIds <- suppressWarnings(as.integer(unlist(conditionSpec$phenotypeCohortId)))
  phenotypeCohortIds <- phenotypeCohortIds[!is.na(phenotypeCohortIds)]
  if (length(phenotypeCohortIds) == 0L) {
    stop("phenotypeCohortId is required for condition: ", ithConditionSpecName(conditionSpec))
  }
  unique(phenotypeCohortIds)
}

# The phenotypeReportScoring CSV rows, or list(NULL) when there is no CSV (settings come from config).
ithPhenotypeReportScoringRuns <- function(config) {
  scoringRuns <- ithReadPhenotypeReportScoringCsv(config)
  if (is.null(scoringRuns)) list(NULL) else scoringRuns
}

ithPhenotypeReportFileStem <- function(text) {
  gsub("[^A-Za-z0-9_-]", "_", text)
}

# Saves one condition/analysis report file pair (rds + csv) under scoring/<conditionName>.
ithSavePhenotypeConditionReport <- function(rows, outputDirectory, conditionName, testCondition, analysisName, verbose = TRUE) {
  scoringDirectory <- file.path(outputDirectory, "scoring", conditionName)
  reportStem <- paste0(
    ithPhenotypeReportFileStem(testCondition), "_",
    ithPhenotypeReportFileStem(analysisName), "_phenotypeAlgorithmRubricReports"
  )
  rownames(rows) <- NULL
  rdsPath <- ithSaveVersioned(saveDirectory = scoringDirectory, objectToSave = rows, fileName = paste0(reportStem, ".rds"))
  csvPath <- ithSaveVersioned(saveDirectory = scoringDirectory, objectToSave = rows, fileName = paste0(reportStem, ".csv"))
  ithMaybeMessage(verbose, "Saved phenotype reports for ", conditionName, " to: ", rdsPath, " ; ", csvPath)
  invisible(c(rds = rdsPath, csv = csvPath))
}

# Saves the combined report across all conditions, named after the default analysisName.
ithSavePhenotypeCombinedReport <- function(rows, config, verbose = TRUE) {
  defaultAnalysisName <- trimws(as.character(ithCoalesce(
    config$phenotypeReportScoring$default$analysisName,
    "rubric_validation"
  )[[1]]))
  baseName <- paste0(ithPhenotypeReportFileStem(defaultAnalysisName), "_phenotypeAlgorithmRubricReports")
  directory <- file.path(as.character(config$outputDirectory[[1]]), "scoring")
  rownames(rows) <- NULL
  rdsPath <- ithSaveVersioned(saveDirectory = directory, objectToSave = rows, fileName = paste0(baseName, ".rds"))
  csvPath <- ithSaveVersioned(saveDirectory = directory, objectToSave = rows, fileName = paste0(baseName, ".csv"))
  ithMaybeMessage(verbose, "Saved combined phenotype reports to: ", rdsPath, " ; ", csvPath)
  invisible(c(rds = rdsPath, csv = csvPath))
}

# Saves the per-condition/analysis report files and the combined report from `reportRows`.
ithSavePhenotypeReportAggregates <- function(reportRows, config, verbose = TRUE) {
  if (is.null(reportRows) || nrow(reportRows) == 0L) {
    return(invisible(NULL))
  }
  outputDirectory <- as.character(config$outputDirectory[[1]])
  testConditions <- if ("testCondition" %in% names(reportRows)) reportRows$testCondition else reportRows$conditionName
  groupKey <- paste(reportRows$conditionName, testConditions, reportRows$analysisName, sep = "\r")
  for (key in unique(groupKey)) {
    inGroup <- groupKey == key
    first <- which(inGroup)[[1]]
    ithSavePhenotypeConditionReport(
      reportRows[inGroup, , drop = FALSE], outputDirectory,
      conditionName = reportRows$conditionName[[first]],
      testCondition = testConditions[[first]],
      analysisName = reportRows$analysisName[[first]],
      verbose = verbose
    )
  }
  ithSavePhenotypeCombinedReport(reportRows, config, verbose = verbose)
}

#' Run phenotype algorithm rubric reports from YAML config
#'
#' For every phenotypeReportScoring run (each row of `phenotypeReportScoringCsvPath`, or the
#' config settings when there is no CSV), condition, database and `phenotypeCohortId`, compares
#' the phenotype cohort with the scored evaluation cohort using
#' [testPhenotypeAlgorithmRubricReport()]. The latest `sumCohortScores` file is the evaluation
#' cohort; when scoring rules are set, the latest `fullScoringFile` is re-scored once per
#' condition and database. A phenotype cohort that is empty or fails gets one report row with
#' the identifying columns filled and every metric `NA`; `reportStatus` is `ok`, `empty`, or
#' `error: <message>`.
#'
#' @param configPath Path to YAML configuration file.
#' @param conditionNames Optional condition names (`rubricName` or `testCondition`) to run.
#' @param databaseIds Optional database IDs to run.
#' @param analysisNames Optional analysis names to run.
#' @param units Optional data frame with columns `analysisName`, `conditionName`, `databaseId`
#'   and `phenotypeCohortId`; when given, only those combinations run.
#' @param generatePhenotypeCohorts Logical; when `TRUE`, generate the needed phenotype cohorts
#'   (incrementally) with CohortGenerator before reporting.
#' @param saveReports Logical; when `TRUE`, save the per-condition and combined report files.
#' @param phenotypePopulationCache Optional environment shared across calls so each phenotype
#'   cohort is downloaded once per database.
#' @param envir Environment where legacy `databaseSpecNames` objects are looked up by name.
#'
#' @return Data frame with one report row per case-zero handling scenario and phenotype cohort.
#' @examples
#' \dontrun{
#' reports <- testPhenotypeAlgorithmRubricReportFromConfig("config.yaml")
#' }
#' @export
testPhenotypeAlgorithmRubricReportFromConfig <- function(configPath = "config.yaml",
                                                         conditionNames = NULL,
                                                         databaseIds = NULL,
                                                         analysisNames = NULL,
                                                         units = NULL,
                                                         generatePhenotypeCohorts = TRUE,
                                                         saveReports = TRUE,
                                                         phenotypePopulationCache = NULL,
                                                         envir = .GlobalEnv) {
  config <- ithReadIthacaWorkflowConfig(configPath)
  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)
  conditionSpecs <- config$conditionSpecs
  verbose <- isTRUE(ithCoalesce(config$verbose, TRUE))

  if (length(databases) == 0L) {
    stop("config.yaml must define at least one database under general.cdm")
  }
  if (is.null(conditionSpecs) || length(conditionSpecs) == 0L) {
    stop("No condition specifications were loaded from conditionSpecsCsvPath")
  }
  if (!is.null(databaseIds)) {
    missingIds <- setdiff(databaseIds, names(databases))
    if (length(missingIds) > 0L) {
      stop("Requested databaseIds are not configured: ", paste(missingIds, collapse = ", "))
    }
    databases <- databases[databaseIds]
  }

  outputDirectory <- as.character(config$outputDirectory[[1]])
  configuredConditionNames <- vapply(conditionSpecs, ithConditionSpecName, character(1))
  unknownScoringConditions <- setdiff(names(config$phenotypeReportScoring$conditions), configuredConditionNames)
  if (length(unknownScoringConditions) > 0L) {
    stop(
      "phenotypeReportScoring.conditions has names that match no condition in the CSV: ",
      paste(unknownScoringConditions, collapse = ", ")
    )
  }
  if (!is.null(conditionNames)) {
    keep <- configuredConditionNames %in% conditionNames
    if (!any(keep)) {
      stop("No conditionSpecs match conditionNames")
    }
    conditionSpecs <- conditionSpecs[keep]
  }
  phenotypeCohortIdsByCondition <- lapply(conditionSpecs, ithPhenotypeReportCohortIds)

  if (!is.null(units)) {
    units <- data.frame(
      analysisName = as.character(units$analysisName),
      conditionName = as.character(units$conditionName),
      databaseId = as.character(units$databaseId),
      phenotypeCohortId = as.integer(units$phenotypeCohortId),
      stringsAsFactors = FALSE
    )
  }
  wanted <- function(analysisName, conditionName, databaseId, cohortIds) {
    if (!is.null(analysisNames) && !(analysisName %in% analysisNames)) {
      return(integer(0))
    }
    if (is.null(units)) {
      return(cohortIds)
    }
    selected <- units$phenotypeCohortId[
      units$analysisName == analysisName & units$conditionName == conditionName & units$databaseId == databaseId
    ]
    cohortIds[cohortIds %in% selected]
  }

  webapiBaseUrl <- as.character(config$webapiBaseUrl[[1]])
  if (length(webapiBaseUrl) == 0L || is.na(webapiBaseUrl) || !nzchar(webapiBaseUrl)) {
    stop("webapiBaseUrl is required to generate phenotype cohorts")
  }

  if (isTRUE(generatePhenotypeCohorts)) {
    jobDatabases <- list()
    jobCohortIds <- list()
    for (database in databases) {
      databaseId <- as.character(database$databaseId[[1]])
      cohortIds <- if (is.null(units)) {
        unlist(phenotypeCohortIdsByCondition, use.names = FALSE)
      } else {
        units$phenotypeCohortId[units$databaseId == databaseId]
      }
      cohortIds <- unique(cohortIds)
      if (length(cohortIds) == 0L) next
      jobDatabases[[length(jobDatabases) + 1L]] <- database
      jobCohortIds[[length(jobCohortIds) + 1L]] <- cohortIds
    }
    if (length(jobDatabases) > 0L) {
      definitionSet <- ithGetWebapiCohortDefinitionSet(
        cohortIds = unique(unlist(jobCohortIds, use.names = FALSE)),
        webapiBaseUrl = webapiBaseUrl,
        verbose = verbose
      )
      jobs <- lapply(seq_along(jobDatabases), function(jobUp) {
        list(
          database = jobDatabases[[jobUp]],
          cohortDefinitionSet = definitionSet[definitionSet$cohortId %in% jobCohortIds[[jobUp]], , drop = FALSE],
          cohortTable = NULL
        )
      })
      ithGenerateCohortJobs(
        jobs,
        workers = ithConfigParallelWorkers(config, "cohortGeneration"),
        stopOnError = TRUE,
        incremental = TRUE,
        createTables = TRUE,
        verbose = verbose
      )
    }
  }

  if (is.null(phenotypePopulationCache)) {
    # Scoring runs that differ only in scoring settings reuse the same downloaded phenotype cohorts.
    phenotypePopulationCache <- new.env(parent = emptyenv())
  }
  reportResults <- list()

  for (scoringRun in ithPhenotypeReportScoringRuns(config)) {
    for (conditionIndex in seq_along(conditionSpecs)) {
      conditionSpec <- conditionSpecs[[conditionIndex]]
      conditionName <- ithConditionSpecName(conditionSpec)
      phenotype <- as.character(conditionSpec$testCondition[[1]])
      randomCohortDefinitionId <- suppressWarnings(as.integer(conditionSpec$randomCohortDefinitionId[[1]]))
      scoringRules <- ithResolvePhenotypeReportScoring(
        config,
        conditionName,
        adjustableRuleId = conditionSpec$adjustableRuleId,
        adjustableRuleScore = conditionSpec$adjustableRuleScore,
        overrides = scoringRun
      )
      neededByDatabase <- lapply(databases, function(database) {
        wanted(scoringRules$analysisName, conditionName, as.character(database$databaseId[[1]]),
               phenotypeCohortIdsByCondition[[conditionIndex]])
      })
      if (all(lengths(neededByDatabase) == 0L)) next

      ithCheckAdjustableRules(
        conditionDir = file.path(outputDirectory, conditionName),
        conditionName = conditionName,
        adjustableRuleId = scoringRules$adjustableRuleId,
        dxRuleScoreOverride = scoringRules$dxRuleScoreOverride,
        dxRuleScoreAdjustment = scoringRules$dxRuleScoreAdjustment
      )
      if (length(randomCohortDefinitionId) == 0L || is.na(randomCohortDefinitionId)) {
        stop("randomCohortDefinitionId is required for condition: ", conditionName)
      }

      conditionResults <- list()
      for (databaseUp in seq_along(databases)) {
        phenotypeCohortIds <- neededByDatabase[[databaseUp]]
        if (length(phenotypeCohortIds) == 0L) next
        rows <- ithRunPhenotypeReportConditionDatabase(
          database = databases[[databaseUp]],
          conditionName = conditionName,
          phenotype = phenotype,
          randomCohortDefinitionId = randomCohortDefinitionId,
          phenotypeCohortIds = phenotypeCohortIds,
          scoringRules = scoringRules,
          outputDirectory = outputDirectory,
          phenotypePopulationCache = phenotypePopulationCache,
          verbose = verbose
        )
        conditionResults <- c(conditionResults, rows)
      }

      if (length(conditionResults) > 0L) {
        conditionRows <- ithBindRows(conditionResults)
        # Saved as soon as the condition finishes so its report is available while later conditions run.
        if (isTRUE(saveReports)) {
          ithSavePhenotypeConditionReport(
            conditionRows, outputDirectory, conditionName, phenotype, scoringRules$analysisName, verbose = verbose
          )
        }
        reportResults[[length(reportResults) + 1L]] <- conditionRows
      }
    }
  }

  results <- if (length(reportResults) > 0L) ithBindRows(reportResults) else data.frame()
  if (nrow(results) > 0L && isTRUE(saveReports)) {
    ithSavePhenotypeCombinedReport(results, config, verbose = verbose)
  }
  results
}

# One condition/database: loads (and optionally re-scores) the evaluation cohort once, then
# reports each phenotype cohort. Returns a list of report data frames.
ithRunPhenotypeReportConditionDatabase <- function(database,
                                                   conditionName,
                                                   phenotype,
                                                   randomCohortDefinitionId,
                                                   phenotypeCohortIds,
                                                   scoringRules,
                                                   outputDirectory,
                                                   phenotypePopulationCache,
                                                   verbose = TRUE) {
  databaseId <- as.character(database$databaseId[[1]])
  cohortDirectory <- file.path(outputDirectory, "scoring", conditionName, databaseId)
  latestSumCohortScores <- ithFindLatestVersion(
    cohortDirectory,
    paste0("sumCohortScores_Cohort", randomCohortDefinitionId, ".rds")
  )
  if (is.null(latestSumCohortScores)) {
    stop(
      "No versioned sumCohortScores file found for condition '", conditionName,
      "', database '", databaseId, "' in: ", cohortDirectory
    )
  }
  evaluationCohortFile <- file.path(cohortDirectory, latestSumCohortScores)

  fullScoringFile <- NULL
  fullScoringData <- NULL
  latestFullScoringFile <- ithFindLatestVersion(
    cohortDirectory,
    paste0("fullScoringFile_Cohort", randomCohortDefinitionId, ".rds")
  )
  if (isTRUE(scoringRules$specified) || isTRUE(scoringRules$createSubjectSample)) {
    if (is.null(latestFullScoringFile)) {
      stop(
        "No versioned fullScoringFile found for condition '", conditionName,
        "', database '", databaseId, "' in: ", cohortDirectory
      )
    }
    fullScoringFile <- file.path(cohortDirectory, latestFullScoringFile)
    # Loaded once here and reused for every phenotype cohort of this condition/database.
    ithMaybeMessage(verbose, "Loading full scoring file: ", fullScoringFile)
    fullScoringData <- readRDS(fullScoringFile)
  }
  # Re-scoring only happens when an adjustment is configured; createSubjectSample alone just reads the file.
  adjustedScoringFile <- if (isTRUE(scoringRules$specified)) fullScoringFile else NULL

  ithMaybeMessage(
    verbose,
    "Running phenotype report for ", conditionName, " / ", databaseId, " using ", evaluationCohortFile,
    if (!is.null(adjustedScoringFile)) paste0(" with adjusted scoring from ", adjustedScoringFile) else ""
  )

  evaluation <- readEvaluationCohortFile(evaluationCohortFile)
  adjustedSumCohortScoresFile <- NA_character_
  if (!is.null(adjustedScoringFile)) {
    ithMaybeMessage(
      verbose,
      "Re-scoring evaluation subjects once with adjusted scoring rules for all ",
      length(phenotypeCohortIds), " phenotype cohort(s)..."
    )
    adjustedFiles <- saveAdjustedScoringFiles(
      evaluationCohortFile = evaluation,
      fullScoringFile = fullScoringFile,
      saveDirectory = cohortDirectory,
      randomCohortDefinitionId = randomCohortDefinitionId,
      excludeNegativeScores = scoringRules$excludeNegativeScores,
      yesScoreOverride = scoringRules$yesScoreOverride,
      noScoreOverride = scoringRules$noScoreOverride,
      yesScoreAdjustment = scoringRules$yesScoreAdjustment,
      noScoreAdjustment = scoringRules$noScoreAdjustment,
      adjustableRuleId = scoringRules$adjustableRuleId,
      dxRuleScoreOverride = scoringRules$dxRuleScoreOverride,
      dxRuleScoreAdjustment = scoringRules$dxRuleScoreAdjustment,
      fullScoringData = fullScoringData
    )
    evaluation <- adjustedFiles$evaluation
    adjustedSumCohortScoresFile <- adjustedFiles$sumScoresPath
    ithMaybeMessage(
      verbose,
      if (adjustedFiles$saved) "Saved adjusted scoring: " else "Adjusted scoring unchanged: ",
      adjustedFiles$sumScoresPath
    )
  }

  lapply(phenotypeCohortIds, function(phenotypeCohortId) {
    reportRunDateTime <- format(Sys.time(), "%Y%m%d%H%M%S")
    reportStatus <- "ok"
    # A failing or empty phenotype cohort must not stop the run: its report row keeps the
    # identifying columns and leaves every metric empty (NA).
    report <- tryCatch(
      testPhenotypeAlgorithmRubricReport(
        phenotype = phenotype,
        analysisName = scoringRules$analysisName,
        runDateTime = reportRunDateTime,
        connectionDetails = database$connectionDetails,
        evaluationCohortFile = evaluation,
        phenotypeCohortId = phenotypeCohortId,
        cdmDatabaseSchema = database$cdmDatabaseSchema,
        databaseId = databaseId,
        cohortDatabaseSchema = database$cohortDatabaseSchema,
        cohortTable = ithDefaultCohortTable(database),
        outputFolder = cohortDirectory,
        yesScoreOverride = scoringRules$yesScoreOverride,
        noScoreOverride = scoringRules$noScoreOverride,
        yesScoreAdjustment = scoringRules$yesScoreAdjustment,
        noScoreAdjustment = scoringRules$noScoreAdjustment,
        adjustableRuleId = scoringRules$adjustableRuleId,
        dxRuleScoreOverride = scoringRules$dxRuleScoreOverride,
        dxRuleScoreAdjustment = scoringRules$dxRuleScoreAdjustment,
        splayPrior = scoringRules$splayPrior,
        splayPost = scoringRules$splayPost,
        sampleSubjectCount = scoringRules$sampleSubjectCount,
        frequencyPlotFpFnScores = scoringRules$frequencyPlotFpFnScores,
        excludeNegativeScores = scoringRules$excludeNegativeScores,
        evaluationAdjusted = !is.null(adjustedScoringFile),
        randomCohortDefinitionId = randomCohortDefinitionId,
        phenotypePopulationCache = phenotypePopulationCache,
        generateTestingPhenotypeFirst = FALSE,
        webapiBaseUrl = NULL
      ),
      error = function(e) {
        reportStatus <<- paste0("error: ", conditionMessage(e))
        warning(
          conditionName, " / ", databaseId, " / cohort ", phenotypeCohortId, ": report failed (",
          conditionMessage(e), "); continuing with empty results.",
          call. = FALSE, immediate. = TRUE
        )
        data.frame()
      }
    )
    if (!is.data.frame(report) || nrow(report) == 0L) {
      if (identical(reportStatus, "ok")) {
        reportStatus <- "empty"
      }
      ithMaybeMessage(
        verbose,
        conditionName, " / ", databaseId, " / cohort ", phenotypeCohortId,
        ": no analyzable results; recording an empty report row."
      )
      report <- data.frame(
        phenotype = phenotype,
        databaseId = databaseId,
        cohortId = as.integer(phenotypeCohortId),
        analysisName = scoringRules$analysisName,
        runDateTime = reportRunDateTime,
        stringsAsFactors = FALSE
      )
    }

    report$reportStatus <- reportStatus
    report$conditionName <- conditionName
    report$testCondition <- phenotype
    report$randomCohortDefinitionId <- randomCohortDefinitionId
    report$evaluationCohortFile <- evaluationCohortFile
    report$fullScoringFile <- if (is.null(adjustedScoringFile)) NA_character_ else adjustedScoringFile
    report$adjustedSumCohortScoresFile <- adjustedSumCohortScoresFile

    supportFileStem <- paste0(
      ithPhenotypeReportFileStem(phenotype), "_",
      ithPhenotypeReportFileStem(scoringRules$analysisName), "_cohort_",
      as.integer(phenotypeCohortId)
    )
    subjectSampleBaseName <- paste0(supportFileStem, "_subject_samples.csv")
    latestSubjectSample <- ithFindLatestVersion(cohortDirectory, subjectSampleBaseName)
    subjectSampleFile <- NA_character_
    if (is.null(latestSubjectSample)) {
      if (identical(reportStatus, "ok")) {
        warning(
          "Subject sample report was not created: ", file.path(cohortDirectory, subjectSampleBaseName),
          call. = FALSE, immediate. = TRUE
        )
      }
    } else {
      subjectSampleFile <- file.path(cohortDirectory, latestSubjectSample)
    }
    report$subjectSampleFile <- subjectSampleFile

    if (isTRUE(scoringRules$createSubjectSample) && identical(reportStatus, "ok") && !is.na(subjectSampleFile)) {
      criteriaRows <- tryCatch({
        conciseRubricVersion <- ithFindLatestVersion(
          file.path(outputDirectory, conditionName),
          paste0(conditionName, "_conciseRubric.csv")
        )
        if (is.null(conciseRubricVersion)) {
          stop("No concise rubric found for condition: ", conditionName)
        }
        nrow(createSubjectSampleScoringReport(
          subjectSampleFile = subjectSampleFile,
          fullScoringFile = fullScoringData,
          conciseRubricFile = file.path(outputDirectory, conditionName, conciseRubricVersion),
          outputFile = ithNextVersionedOutputPath(
            cohortDirectory,
            paste0(supportFileStem, "_scoringCriteriaReport.csv")
          ),
          classLabels = scoringRules$classLabels,
          cohortId = phenotypeCohortId,
          adjustableRuleId = scoringRules$adjustableRuleId,
          dxRuleScoreOverride = scoringRules$dxRuleScoreOverride,
          dxRuleScoreAdjustment = scoringRules$dxRuleScoreAdjustment
        ))
      }, error = function(e) {
        warning(
          conditionName, " / ", databaseId, " / cohort ", phenotypeCohortId,
          ": scoring criteria report failed (", conditionMessage(e), ").",
          call. = FALSE, immediate. = TRUE
        )
        NA_integer_
      })
      report$subjectScoringCriteriaReportRows <- criteriaRows
    }
    report
  })
}
