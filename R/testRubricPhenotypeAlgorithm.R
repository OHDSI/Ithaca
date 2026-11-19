readEvaluationRows <- function(evaluationCohortFile) {
  stopifnot(is.character(evaluationCohortFile), length(evaluationCohortFile) == 1, nzchar(evaluationCohortFile))
  if (!file.exists(evaluationCohortFile)) {
    stop(paste0("Evaluation cohort file does not exist: ", evaluationCohortFile))
  }

  lower <- tolower(evaluationCohortFile)
  if (grepl("\\.rds$", lower)) {
    readRDS(evaluationCohortFile)
  } else if (grepl("\\.csv$", lower)) {
    utils::read.csv(evaluationCohortFile, stringsAsFactors = FALSE)
  } else if (grepl("\\.tsv$", lower) || grepl("\\.txt$", lower)) {
    utils::read.delim(evaluationCohortFile, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported evaluation cohort file type. Use .csv, .tsv/.txt, or .rds")
  }
}

# Accepts a file path or an already-loaded evaluation data frame.
readEvaluationCohortFile <- function(evaluationCohortFile) {
  evaluation <- if (is.data.frame(evaluationCohortFile)) {
    evaluationCohortFile
  } else {
    readEvaluationRows(evaluationCohortFile)
  }

  evaluation <- as.data.frame(evaluation, stringsAsFactors = FALSE)

  requiredBase <- c("subjectId", "cohortStartDate", "total")
  missingBase <- setdiff(requiredBase, names(evaluation))
  if (length(missingBase) > 0) {
    stop(
      paste0(
        "Evaluation cohort file is missing required columns: ",
        paste(missingBase, collapse = ", "),
        "."
      )
    )
  }

  if (!("cohortEndDate" %in% names(evaluation))) {
    evaluation$cohortEndDate <- evaluation$cohortStartDate
  }

  if (!("yesScore" %in% names(evaluation))) {
    evaluation$yesScore <- NA_real_
  }

  if (!("noScore" %in% names(evaluation))) {
    evaluation$noScore <- NA_real_
  }

  if (!("case" %in% names(evaluation))) {
    evaluation$case <- NA_integer_
  }

  evaluation$subjectId <- as.numeric(evaluation$subjectId)
  evaluation$cohortStartDate <- as.Date(evaluation$cohortStartDate)
  evaluation$cohortEndDate <- as.Date(evaluation$cohortEndDate)
  evaluation$yesScore <- as.numeric(evaluation$yesScore)
  evaluation$noScore <- as.numeric(evaluation$noScore)
  evaluation$total <- as.numeric(evaluation$total)
  evaluation$case <- as.integer(evaluation$case)

  evaluation <- evaluation[!is.na(evaluation$subjectId) & !is.na(evaluation$cohortStartDate), , drop = FALSE]
  evaluation
}

computeCaseFromScores <- function(evaluation,
                                     yesScoreOverride = NULL,
                                     noScoreOverride = NULL) {
  if (is.null(yesScoreOverride) && any(is.na(evaluation$yesScore))) {
    stop(
      "Evaluation cohort file does not contain yesScore for all rows. ",
      "Provide yesScoreOverride (and noScoreOverride), or use an evaluation file that includes yesScore/noScore ",
      "(for example sumCohortScores_* rather than finalScores_*)."
    )
  }

  if (is.null(noScoreOverride) && any(is.na(evaluation$noScore))) {
    stop(
      "Evaluation cohort file does not contain noScore for all rows. ",
      "Provide noScoreOverride (and yesScoreOverride), or use an evaluation file that includes yesScore/noScore ",
      "(for example sumCohortScores_* rather than finalScores_*)."
    )
  }

  yesScoreUsed <- if (is.null(yesScoreOverride)) evaluation$yesScore else rep(as.numeric(yesScoreOverride), nrow(evaluation))
  noScoreUsed <- if (is.null(noScoreOverride)) evaluation$noScore else rep(as.numeric(noScoreOverride), nrow(evaluation))

  if (any(yesScoreUsed <= noScoreUsed, na.rm = TRUE)) {
    stop("yesScoreUsed must be greater than noScoreUsed for all rows")
  }

  derivedCase <- rep(0L, nrow(evaluation))
  derivedCase[evaluation$total >= yesScoreUsed] <- 1L
  derivedCase[evaluation$total <= noScoreUsed] <- -1L

  evaluation$yesScoreUsed <- yesScoreUsed
  evaluation$noScoreUsed <- noScoreUsed
  evaluation$case <- derivedCase
  evaluation
}

# Zero-hit subjects are absent from fullScoringFile, so the subject list comes from the evaluation file.
applyAdjustedScoringToEvaluation <- function(evaluation,
                                             fullScoringFile,
                                             excludeNegativeScores = FALSE,
                                             yesScoreOverride = NULL,
                                             noScoreOverride = NULL,
                                             yesScoreAdjustment = NULL,
                                             noScoreAdjustment = NULL,
                                             adjustableRuleId = NULL,
                                             dxRuleScoreOverride = NULL,
                                             dxRuleScoreAdjustment = NULL,
                                             fullScoringData = NULL) {
  stopifnot(is.character(fullScoringFile), length(fullScoringFile) == 1, nzchar(fullScoringFile))
  if (is.null(fullScoringData) && !file.exists(fullScoringFile)) {
    stop(paste0("Full scoring file does not exist: ", fullScoringFile))
  }

  evaluationRubricIds <- character()
  if ("rubricId" %in% names(evaluation)) {
    evaluationRubricIds <- unique(as.character(evaluation$rubricId[!is.na(evaluation$rubricId)]))
  }

  resolveThreshold <- function(override, adjustment, column, overrideLabel, adjustmentLabel) {
    if (!is.null(override) && !is.null(adjustment)) {
      stop("Provide either ", overrideLabel, " or ", adjustmentLabel, ", not both")
    }

    if (!is.null(override)) {
      if (!is.numeric(override) || length(override) != 1 || is.na(override)) {
        stop(overrideLabel, " must be a single numeric value (adjusted scoring supports one rubric)")
      }
      return(as.numeric(override))
    }

    fileValues <- unique(evaluation[[column]][!is.na(evaluation[[column]])])
    if (length(fileValues) != 1) {
      stop(overrideLabel, " was not supplied and the evaluation file does not contain exactly one ", column, " value")
    }

    if (!is.null(adjustment)) {
      if (!is.numeric(adjustment) || length(adjustment) != 1 || is.na(adjustment)) {
        stop(adjustmentLabel, " must be a single numeric value")
      }
      return(as.numeric(fileValues) + as.numeric(adjustment))
    }

    as.numeric(fileValues)
  }

  yesScore <- resolveThreshold(yesScoreOverride, yesScoreAdjustment, "yesScore", "yesScoreOverride", "yesScoreAdjustment")
  noScore <- resolveThreshold(noScoreOverride, noScoreAdjustment, "noScore", "noScoreOverride", "noScoreAdjustment")
  if (yesScore <= noScore) {
    stop("Resolved yes threshold (", yesScore, ") must be greater than the resolved no threshold (", noScore, ")")
  }

  scored <- ithScoreFullScoringFile(
    fullScoringFile = fullScoringFile,
    excludeNegativeScores = excludeNegativeScores,
    yesScoreOverride = yesScore,
    noScoreOverride = noScore,
    saveAdjustedOutput = FALSE,
    verbose = FALSE,
    adjustableRuleId = adjustableRuleId,
    dxRuleScoreOverride = dxRuleScoreOverride,
    dxRuleScoreAdjustment = dxRuleScoreAdjustment,
    fullScoringData = fullScoringData
  )

  adjusted <- as.data.frame(scored$sumCohortScores, stringsAsFactors = FALSE)
  rubricIds <- union(evaluationRubricIds, unique(as.character(adjusted$rubricId)))
  if (length(rubricIds) > 1) {
    stop(
      "Adjusted scoring supports a single rubric; found rubricId values: ",
      paste(rubricIds, collapse = ", ")
    )
  }

  adjusted <- data.frame(
    subjectId = as.numeric(adjusted$subjectId),
    cohortStartDate = as.Date(adjusted$cohortStartDate),
    adjustedTotal = as.numeric(adjusted$total),
    stringsAsFactors = FALSE
  )

  evaluation <- merge(
    evaluation,
    adjusted,
    by = c("subjectId", "cohortStartDate"),
    all.x = TRUE,
    sort = FALSE
  )

  noScoringRows <- is.na(evaluation$adjustedTotal)
  if (nrow(adjusted) > 0 && nrow(evaluation) > 0 && all(noScoringRows)) {
    stop(
      "No evaluation subjects matched the full scoring file on subjectId and cohortStartDate. ",
      "Check that both files cover the same cohort and that the bit64 package is loaded (integer64 subject IDs)."
    )
  }
  message(paste0(
    "Adjusted scoring: ", sum(noScoringRows), " of ", nrow(evaluation),
    " evaluation rows have no scoring rows and were assigned total 0."
  ))

  evaluation$total <- ifelse(noScoringRows, 0, evaluation$adjustedTotal)
  evaluation$adjustedTotal <- NULL
  evaluation$yesScore <- yesScore
  evaluation$noScore <- noScore
  evaluation
}

# Saves what the adjusted scoring evaluates; a rerun with identical results writes nothing new.
# Also returns the re-scored evaluation so callers can reuse it for every phenotype cohort.
saveAdjustedScoringFiles <- function(evaluationCohortFile,
                                     fullScoringFile,
                                     saveDirectory,
                                     randomCohortDefinitionId,
                                     excludeNegativeScores = FALSE,
                                     yesScoreOverride = NULL,
                                     noScoreOverride = NULL,
                                     yesScoreAdjustment = NULL,
                                     noScoreAdjustment = NULL,
                                     adjustableRuleId = NULL,
                                     dxRuleScoreOverride = NULL,
                                     dxRuleScoreAdjustment = NULL,
                                     fullScoringData = NULL) {
  evaluation <- applyAdjustedScoringToEvaluation(
    evaluation = readEvaluationCohortFile(evaluationCohortFile),
    fullScoringFile = fullScoringFile,
    excludeNegativeScores = excludeNegativeScores,
    yesScoreOverride = yesScoreOverride,
    noScoreOverride = noScoreOverride,
    yesScoreAdjustment = yesScoreAdjustment,
    noScoreAdjustment = noScoreAdjustment,
    adjustableRuleId = adjustableRuleId,
    dxRuleScoreOverride = dxRuleScoreOverride,
    dxRuleScoreAdjustment = dxRuleScoreAdjustment,
    fullScoringData = fullScoringData
  )
  adjustedEvaluation <- evaluation
  evaluation <- computeCaseFromScores(evaluation)

  sumColumns <- intersect(
    c("cohortDefinitionId", "subjectId", "cohortStartDate", "rubricId", "total", "yesScore", "noScore", "case"),
    names(evaluation)
  )
  sumScores <- evaluation[, sumColumns, drop = FALSE]

  byColumns <- intersect(c("cohortDefinitionId", "subjectId", "cohortStartDate"), names(sumScores))
  finalScores <- data.table::as.data.table(sumScores)[, .(total = sum(case, na.rm = TRUE)), by = byColumns]
  finalScores$case <- 0L
  finalScores$case[finalScores$total > 0] <- 1L
  finalScores$case[finalScores$total < 0] <- -1L

  sumName <- paste0("sumCohortScoresAdjusted_Cohort", randomCohortDefinitionId, ".rds")
  finalName <- paste0("finalScoresAdjusted_Cohort", randomCohortDefinitionId, ".rds")

  latestSum <- ithLoadLatest(saveDirectory, sumName)
  if (!is.null(latestSum) && ithScoringUnchanged(as.data.frame(latestSum), sumScores)) {
    return(list(
      saved = FALSE,
      sumScoresPath = file.path(saveDirectory, ithFindLatestVersion(saveDirectory, sumName)),
      finalScoresPath = file.path(saveDirectory, ithFindLatestVersion(saveDirectory, finalName)),
      evaluation = adjustedEvaluation
    ))
  }

  list(
    saved = TRUE,
    sumScoresPath = ithSaveVersioned(saveDirectory, sumScores, sumName),
    finalScoresPath = ithSaveVersioned(saveDirectory, finalScores, finalName),
    evaluation = adjustedEvaluation
  )
}

getWebapiCohortName <- function(webapiBaseUrl, cohortId) {
  if (is.null(webapiBaseUrl) || !is.character(webapiBaseUrl) || !nzchar(webapiBaseUrl)) {
    return(NA_character_)
  }

  cohortDefinition <- tryCatch(
    ROhdsiWebApi::getCohortDefinition(
      cohortId = as.integer(cohortId),
      baseUrl = webapiBaseUrl
    ),
    error = function(e) NULL
  )

  if (is.null(cohortDefinition)) {
    return(NA_character_)
  }

  definitionName <- NULL
  if (!is.null(cohortDefinition$name)) {
    definitionName <- cohortDefinition$name
  } else if (!is.null(cohortDefinition$cohortName)) {
    definitionName <- cohortDefinition$cohortName
  }
  if (is.null(definitionName) || length(definitionName) == 0 || is.na(definitionName[[1]]) || !nzchar(as.character(definitionName[[1]]))) {
    return(NA_character_)
  }

  as.character(definitionName[[1]])
}

getPhenotypePopulation <- function(connectionDetails,
                                     cdmDatabaseSchema,
                                     cohortDatabaseSchema,
                                     cohortTable,
                                     phenotypeCohortId,
                                     requireObservationPeriod = TRUE,
                                     randomCohortDefinitionId = NULL,
                                     cache = NULL) {
  # The population does not depend on the scoring settings, so a run that scores the same
  # cohort several times can pass an environment and download it once.
  cacheKey <- NULL
  if (is.environment(cache)) {
    cacheKey <- paste(
      c(
        paste(unlist(connectionDetails[c("dbms", "server", "connectionString")], use.names = FALSE), collapse = "|"),
        cdmDatabaseSchema, cohortDatabaseSchema, cohortTable, phenotypeCohortId,
        requireObservationPeriod, if (is.null(randomCohortDefinitionId)) "none" else randomCohortDefinitionId
      ),
      collapse = "\r"
    )
    if (exists(cacheKey, envir = cache, inherits = FALSE)) {
      return(get(cacheKey, envir = cache, inherits = FALSE))
    }
  }
  population <- getPhenotypePopulationUncached(
    connectionDetails, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable,
    phenotypeCohortId, requireObservationPeriod, randomCohortDefinitionId
  )
  if (!is.null(cacheKey)) {
    assign(cacheKey, population, envir = cache)
  }
  population
}

getPhenotypePopulationUncached <- function(connectionDetails,
                                           cdmDatabaseSchema,
                                           cohortDatabaseSchema,
                                           cohortTable,
                                           phenotypeCohortId,
                                           requireObservationPeriod = TRUE,
                                           randomCohortDefinitionId = NULL) {
  if (requireObservationPeriod) {
    sql <- "
SELECT DISTINCT
  c.subject_id,
  c.cohort_start_date AS pheno_cohort_start_date
FROM @cohort_database_schema.@cohortTable c
JOIN @cdm_database_schema.observation_period op
  ON c.subject_id = op.person_id
  AND c.cohort_start_date >= op.observation_period_start_date
  AND c.cohort_start_date <= op.observation_period_end_date
WHERE c.cohort_definition_id = @cohort_id;
"
  } else {
    sql <- "
SELECT DISTINCT
  c.subject_id,
  c.cohort_start_date AS pheno_cohort_start_date
FROM @cohort_database_schema.@cohortTable c
WHERE c.cohort_definition_id = @cohort_id;
"
  }

  # Only subjects in the random cohort can reach the metrics, so filter in the database to avoid downloading the rest.
  if (!is.null(randomCohortDefinitionId)) {
    sql <- sub(
      "WHERE c.cohort_definition_id = @cohort_id;",
      paste0(
        "WHERE c.cohort_definition_id = @cohort_id\n  AND EXISTS (SELECT 1 FROM @cohort_database_schema.@cohortTable r\n",
        "    WHERE r.cohort_definition_id = @random_cohort_id AND r.subject_id = c.subject_id);"
      ),
      sql,
      fixed = TRUE
    )
  }
  randomCohortId <- if (is.null(randomCohortDefinitionId)) NULL else list(random_cohort_id = as.integer(randomCohortDefinitionId))

  if (requireObservationPeriod) {
    sql <- do.call(SqlRender::render, c(
      list(
        sql = sql,
        cohort_database_schema = cohortDatabaseSchema,
        cdm_database_schema = cdmDatabaseSchema,
        cohortTable = cohortTable,
        cohort_id = as.integer(phenotypeCohortId)
      ),
      randomCohortId
    ))
  } else {
    sql <- do.call(SqlRender::render, c(
      list(
        sql = sql,
        cohort_database_schema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        cohort_id = as.integer(phenotypeCohortId)
      ),
      randomCohortId
    ))
  }
  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  DatabaseConnector::querySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE
  )
}

generateTestingPhenotypeCohort <- function(connectionDetails,
                                              webapiBaseUrl,
                                              phenotypeCohortId,
                                              cdmDatabaseSchema,
                                              cohortDatabaseSchema,
                                              cohortTable,
                                              phenotypeCohortName = NULL,
                                              incremental = TRUE) {
  sourceDefinition <- ROhdsiWebApi::getCohortDefinition(
    cohortId = as.integer(phenotypeCohortId),
    baseUrl = webapiBaseUrl
  )

  sourceSql <- ROhdsiWebApi::getCohortSql(
    cohortDefinition = sourceDefinition,
    baseUrl = webapiBaseUrl,
    generateStats = FALSE
  )

  if (is.list(sourceSql) && !is.null(sourceSql$sql)) {
    sourceSql <- sourceSql$sql
  }
  sourceSql <- as.character(sourceSql)

  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    incremental = TRUE
  )

  definitionSet <- data.frame(
    cohortId = as.integer(phenotypeCohortId),
    cohortName = if (is.null(phenotypeCohortName) || !nzchar(phenotypeCohortName)) {
      paste0("cohort_", as.integer(phenotypeCohortId))
    } else {
      phenotypeCohortName
    },
    sql = sourceSql,
    stringsAsFactors = FALSE
  )

  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = definitionSet,
    stopOnError = TRUE,
    incremental = incremental
  )

  ithReportCohortCounts(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cohortIds = phenotypeCohortId
  )
}

computeCi <- function(x, n) {
  if (is.na(n) || n <= 0) {
    return(c(estimate = NA_real_, ci95Lb = NA_real_, ci95Ub = NA_real_))
  }
  bt <- stats::binom.test(as.integer(x), as.integer(n), conf.level = 0.95)
  c(estimate = as.numeric(bt$estimate), ci95Lb = bt$conf.int[1], ci95Ub = bt$conf.int[2])
}

computeMetricsFromCounts <- function(truePositives, trueNegatives, falsePositives, falseNegatives) {
  sens <- computeCi(truePositives, truePositives + falseNegatives)
  ppv <- computeCi(truePositives, truePositives + falsePositives)
  spec <- computeCi(trueNegatives, trueNegatives + falsePositives)
  npv <- computeCi(trueNegatives, trueNegatives + falseNegatives)

  sensVal <- sens["estimate"]
  ppvVal <- ppv["estimate"]
  f1 <- if (is.na(sensVal) || is.na(ppvVal) || sensVal == 0 || ppvVal == 0) {
    NA_real_
  } else {
    2 * (sensVal * ppvVal) / (sensVal + ppvVal)
  }

  totalSubjects <- truePositives + trueNegatives + falsePositives + falseNegatives
  estimatedCases <- truePositives + falseNegatives
  estimatedPrevalence <- if (totalSubjects == 0) NA_real_ else estimatedCases / totalSubjects

  data.frame(
    truePositives = as.integer(truePositives),
    trueNegatives = as.integer(trueNegatives),
    falsePositives = as.integer(falsePositives),
    falseNegatives = as.integer(falseNegatives),
    totalSubjects = as.integer(totalSubjects),
    estimatedCases = as.integer(estimatedCases),
    estimatedPrevalence = estimatedPrevalence,
    sensitivity = sens["estimate"],
    sensitivityCi95Lb = sens["ci95Lb"],
    sensitivityCi95Ub = sens["ci95Ub"],
    ppv = ppv["estimate"],
    ppvCi95Lb = ppv["ci95Lb"],
    ppvCi95Ub = ppv["ci95Ub"],
    specificity = spec["estimate"],
    specificityCi95Lb = spec["ci95Lb"],
    specificityCi95Ub = spec["ci95Ub"],
    npv = npv["estimate"],
    npvCi95Lb = npv["ci95Lb"],
    npvCi95Ub = npv["ci95Ub"],
    f1Score = f1,
    stringsAsFactors = FALSE
  )
}

scoreRubricVsCohort <- function(fullTable, caseZeroAsCase) {
  actualCase <- fullTable$case == 1L
  actualCase[fullTable$case == 0L] <- isTRUE(caseZeroAsCase)

  inPhenotype <- fullTable$inPhenotype

  tp <- sum(inPhenotype & actualCase, na.rm = TRUE)
  tn <- sum(!inPhenotype & !actualCase, na.rm = TRUE)
  fp <- sum(inPhenotype & !actualCase, na.rm = TRUE)
  fn <- sum(!inPhenotype & actualCase, na.rm = TRUE)

  metrics <- computeMetricsFromCounts(tp, tn, fp, fn)
  metrics$caseZeroHandling <- if (caseZeroAsCase) "caseZeroAsCase" else "caseZeroAsNonCase"
  metrics
}

buildSubjectClassification <- function(fullTable, caseZeroAsCase) {
  actualCase <- fullTable$case == 1L
  actualCase[fullTable$case == 0L] <- isTRUE(caseZeroAsCase)

  predictedCase <- fullTable$inPhenotype
  classLabel <- ifelse(
    predictedCase & actualCase,
    "TP",
    ifelse(
      predictedCase & !actualCase,
      "FP",
      ifelse(!predictedCase & !actualCase, "TN", "FN")
    )
  )

  data.frame(
    caseZeroHandling = if (caseZeroAsCase) "caseZeroAsCase" else "caseZeroAsNonCase",
    classLabel = classLabel,
    evaluationSubjectId = ifelse(classLabel %in% c("TP", "TN", "FN"), fullTable$subjectId, NA_real_),
    testingSubjectId = ifelse(classLabel %in% c("TP", "FP"), fullTable$subjectId, NA_real_),
    evaluationTotalScore = fullTable$total,
    evaluationYesScoreUsed = fullTable$yesScoreUsed,
    evaluationNoScoreUsed = fullTable$noScoreUsed,
    evaluationCase = fullTable$case,
    cohortStartDate = as.Date(fullTable$cohortStartDate),
    phenoCohortStartDate = as.Date(fullTable$phenoCohortStartDate),
    stringsAsFactors = FALSE
  )
}

# Use the repository's version suffix for support artifacts while retaining a stable base name.
ithNextVersionedOutputPath <- function(outputFolder, fileName) {
  ext <- tools::file_ext(fileName)
  stem <- tools::file_path_sans_ext(basename(fileName))
  latest <- ithFindLatestVersion(outputFolder, fileName)
  nextVersion <- if (is.null(latest)) {
    1L
  } else {
    as.integer(sub(paste0("^", ithEscapeRegex(stem), "_V([0-9]+)\\.", ithEscapeRegex(ext), "$"), "\\1", latest)) + 1L
  }
  file.path(outputFolder, paste0(stem, "_V", nextVersion, ".", ext))
}

writeRubricSupportingReports <- function(outputFolder,
                                            phenotype,
                                            analysisName,
                                            runDateTime,
                                            phenotypeCohortId,
                                            sampleSubjectCount,
                                            fullTable,
                                         dateMismatchRows,
                                         caseZeroHandlingOptions = c(FALSE, TRUE)) {
  if (is.null(outputFolder) || !nzchar(outputFolder)) {
    return(invisible(NULL))
  }

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  }

  safePhenotype <- gsub("[^A-Za-z0-9_-]", "_", phenotype)
  safeAnalysis <- gsub("[^A-Za-z0-9_-]", "_", analysisName)
  fileStem <- paste0(safePhenotype, "_", safeAnalysis, "_cohort_", as.integer(phenotypeCohortId))

  caseZeroHandlingOptions <- unique(as.logical(caseZeroHandlingOptions))
  caseZeroHandlingOptions <- caseZeroHandlingOptions[!is.na(caseZeroHandlingOptions)]
  if (length(caseZeroHandlingOptions) == 0) {
    caseZeroHandlingOptions <- FALSE
  }

  classTables <- lapply(caseZeroHandlingOptions, function(caseZeroAsCase) {
    buildSubjectClassification(fullTable, caseZeroAsCase = caseZeroAsCase)
  })
  classTable <- do.call(rbind, classTables)

  caseHandlingLabels <- ifelse(caseZeroHandlingOptions, "caseZeroAsCase", "caseZeroAsNonCase")
  sampledRows <- lapply(caseHandlingLabels, function(caseHandling) {
    lapply(c("TP", "FP", "TN", "FN"), function(className) {
      subsetRows <- classTable[
        classTable$caseZeroHandling == caseHandling & classTable$classLabel == className,
        ,
        drop = FALSE
      ]
      if (nrow(subsetRows) > sampleSubjectCount) {
        subsetRows <- subsetRows[seq_len(sampleSubjectCount), , drop = FALSE]
      }
      subsetRows
    })
  })
  sampledRows <- do.call(rbind, unlist(sampledRows, recursive = FALSE))
  sampledRows <- data.frame(
    cohortId = rep(as.integer(phenotypeCohortId), nrow(sampledRows)),
    sampledRows,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  confusionReportPath <- ithNextVersionedOutputPath(outputFolder, paste0(fileStem, "_subject_samples.csv"))
  utils::write.csv(sampledRows, confusionReportPath, row.names = FALSE)

  dateMismatchRows <- data.frame(
    cohortId = rep(as.integer(phenotypeCohortId), nrow(dateMismatchRows)),
    dateMismatchRows,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dateMismatchReportPath <- ithNextVersionedOutputPath(outputFolder, paste0(fileStem, "_date_mismatch_subjects.csv"))
  utils::write.csv(dateMismatchRows, dateMismatchReportPath, row.names = FALSE)

  message(paste0("Wrote subject sample report: ", confusionReportPath))
  message(paste0("Wrote date mismatch report: ", dateMismatchReportPath))

  invisible(list(
    confusionReportPath = confusionReportPath,
    dateMismatchReportPath = dateMismatchReportPath
  ))
}

writeFpFnFrequencyPlots <- function(outputFolder,
                                        phenotype,
                                        analysisName,
                                        runDateTime,
                                        phenotypeCohortId,
                                    fullTable,
                                    caseZeroHandlingOptions = c(FALSE, TRUE)) {
  if (is.null(outputFolder) || !nzchar(outputFolder)) {
    message("Skipping FP/FN frequency plots because outputFolder is not provided.")
    return(invisible(NULL))
  }

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE)
  }

  safePhenotype <- gsub("[^A-Za-z0-9_-]", "_", phenotype)
  safeAnalysis <- gsub("[^A-Za-z0-9_-]", "_", analysisName)
  safeCohortId <- as.integer(phenotypeCohortId)
  fileStem <- paste0(safePhenotype, "_", safeAnalysis, "_cohort_", safeCohortId)

  buildDiscreteFrequencyTable <- function(scores, classLabel, caseHandlingLabel) {
    scores <- scores[!is.na(scores) & scores >= 0]
    if (length(scores) == 0) {
      return(data.frame(
        caseZeroHandling = character(),
        classLabel = character(),
        score = integer(),
        count = integer(),
        stringsAsFactors = FALSE
      ))
    }

    nonInteger <- abs(scores - round(scores, 0)) > 1e-8
    if (any(nonInteger)) {
      stop(paste0(
        "Non-integer evaluation scores detected while plotting discrete bars. Example value: ",
        as.character(scores[which(nonInteger)[1]])
      ))
    }

    scoreInts <- as.integer(round(scores, 0))
    scoreTab <- table(scoreInts)

    data.frame(
      caseZeroHandling = caseHandlingLabel,
      classLabel = classLabel,
      score = as.integer(names(scoreTab)),
      count = as.integer(scoreTab),
      stringsAsFactors = FALSE
    )
  }

  plotDiscreteScoreCounts <- function(scores, titleText, barColor) {
    scores <- scores[!is.na(scores) & scores >= 0]

    if (length(scores) == 0) {
      graphics::plot.new()
      graphics::title(main = titleText)
      graphics::text(0.5, 0.5, "No scores >= 0")
      return(invisible(NULL))
    }

    nonInteger <- abs(scores - round(scores, 0)) > 1e-8
    if (any(nonInteger)) {
      stop(paste0(
        "Non-integer evaluation scores detected while plotting discrete bars. Example value: ",
        as.character(scores[which(nonInteger)[1]])
      ))
    }

    scoreInts <- as.integer(round(scores, 0))
    scoreTab <- table(scoreInts)
    scoreVals <- as.integer(names(scoreTab))
    counts <- as.integer(scoreTab)

    xPos <- graphics::barplot(
      height = counts,
      names.arg = scoreVals,
      main = titleText,
      xlab = "Evaluation Total Score",
      ylab = "Count",
      col = barColor,
      border = "white"
    )

    graphics::text(
      x = xPos,
      y = counts,
      labels = scoreVals,
      pos = 3,
      cex = 0.75
    )

    invisible(NULL)
  }

  buildOnePlot <- function(caseZeroAsCase) {
    classTable <- buildSubjectClassification(fullTable, caseZeroAsCase = caseZeroAsCase)
    fpScores <- classTable$evaluationTotalScore[classTable$classLabel == "FP"]
    fnScores <- classTable$evaluationTotalScore[classTable$classLabel == "FN"]

    fileSuffix <- if (caseZeroAsCase) "caseZeroAsCase" else "caseZeroAsNonCase"
    outPath <- ithNextVersionedOutputPath(outputFolder, paste0(fileStem, "_fp_fn_score_frequency_", fileSuffix, ".png"))
    outCsvPath <- ithNextVersionedOutputPath(outputFolder, paste0(fileStem, "_fp_fn_score_frequency_", fileSuffix, ".csv"))

    fpFreq <- buildDiscreteFrequencyTable(fpScores, classLabel = "FP", caseHandlingLabel = fileSuffix)
    fnFreq <- buildDiscreteFrequencyTable(fnScores, classLabel = "FN", caseHandlingLabel = fileSuffix)
    frequencyTable <- rbind(fpFreq, fnFreq)
    utils::write.csv(frequencyTable, outCsvPath, row.names = FALSE)

    grDevices::png(filename = outPath, width = 1200, height = 500)
    oldPar <- graphics::par(no.readonly = TRUE)
    on.exit({
      graphics::par(oldPar)
      grDevices::dev.off()
    }, add = TRUE)

    graphics::par(mfrow = c(1, 2))

    plotDiscreteScoreCounts(
      scores = fpScores,
      titleText = paste0("FP Evaluation Scores (", fileSuffix, ")"),
      barColor = "tomato"
    )

    plotDiscreteScoreCounts(
      scores = fnScores,
      titleText = paste0("FN Evaluation Scores (", fileSuffix, ")"),
      barColor = "steelblue"
    )

    list(plotPath = outPath, csvPath = outCsvPath)
  }

  caseZeroHandlingOptions <- unique(as.logical(caseZeroHandlingOptions))
  caseZeroHandlingOptions <- caseZeroHandlingOptions[!is.na(caseZeroHandlingOptions)]
  if (length(caseZeroHandlingOptions) == 0) {
    caseZeroHandlingOptions <- FALSE
  }

  pathResults <- lapply(caseZeroHandlingOptions, function(caseZeroAsCase) {
    buildOnePlot(caseZeroAsCase = caseZeroAsCase)
  })

  names(pathResults) <- ifelse(caseZeroHandlingOptions, "caseZeroAsCase", "caseZeroAsNonCase")

  for (nameUp in names(pathResults)) {
    message(paste0("Wrote FP/FN score frequency plot: ", pathResults[[nameUp]]$plotPath))
    message(paste0("Wrote FP/FN score frequency CSV: ", pathResults[[nameUp]]$csvPath))
  }

  invisible(pathResults)
}

#' Test phenotype algorithm using rubric-evaluation file
#'
#' @param phenotype Name of phenotype for reporting.
#' @param analysisName Name of analysis for reporting.
#' @param runDateTime Run timestamp string.
#' @param connectionDetails DatabaseConnector connection details.
#' @param evaluationCohortFile File path to evaluation cohort (.csv/.tsv/.rds), or an
#' already-loaded evaluation data frame (useful when testing several phenotype cohorts
#' against the same scoring results).
#' @param phenotypeCohortId Cohort ID (or vector of IDs) to evaluate from cohort table.
#' @param cdmDatabaseSchema CDM database schema.
#' @param databaseId Database identifier for reporting.
#' @param cohortDatabaseSchema Cohort database schema.
#' @param cohortTable Cohort table name.
#' @param yesScoreOverride Optional scalar override for yesScore threshold.
#' @param noScoreOverride Optional scalar override for noScore threshold.
#' @param outputFolder Optional folder to write supporting subject-level reports.
#' @param sampleSubjectCount Number of subjects per class (TP/FP/TN/FN) to include in sample report.
#' @param frequencyPlotFpFnScores If TRUE, write FP/FN evaluation score frequency plots.
#' @param generateTestingPhenotypeFirst If TRUE, generate the testing phenotype cohort before analysis.
#' @param webapiBaseUrl WebAPI base URL used when generateTestingPhenotypeFirst is TRUE.
#' When provided, phenotype labels in output are sourced from the WebAPI cohort definition name for each `phenotypeCohortId` when available.
#' @param phenotypeCohortName Optional cohort name label used during generation.
#' @param generationIncremental Use incremental generation (skip if SQL unchanged).
#' @param splayPrior Days allowed prior to phenotype cohort start date.
#' @param splayPost Days allowed after phenotype cohort start date.
#' @param requireObservationPeriod Should cohort rows be constrained to observation period.
#' @param cohort_start_date Optional lower bound for cohort start date filtering (inclusive), in MM/DD/YYYY format.
#' @param cohort_end_date Optional upper bound for cohort start date filtering (inclusive), in MM/DD/YYYY format.
#' @param caseZeroAsCase Logical; if `TRUE`, also generate the `caseZeroAsCase` scenario in outputs. Defaults to `FALSE`, and `caseZeroAsNonCase` is always generated.
#' @param fullScoringFile Optional path to a `fullScoringFile_Cohort*.rds` artifact (single rubric).
#' When supplied, rule scores are re-summed from this file using `excludeNegativeScores`,
#' `yesScoreOverride`, and `noScoreOverride`. `evaluationCohortFile` (a `sumCohortScores_*`
#' artifact) still supplies the subject list and any threshold not overridden; subjects with
#' no scoring rows receive a total of 0.
#' @param excludeNegativeScores Logical; when `TRUE`, scoring rows with `score < 0` are dropped
#' before summing, except AE1 (the fixed zero-occurrence penalty), which is always applied. Requires `fullScoringFile`.
#' @param yesScoreAdjustment Optional single number added to the current (evaluation file) yes
#' threshold, for example `-2` lowers it by 2. Requires `fullScoringFile`; cannot be combined
#' with `yesScoreOverride`.
#' @param noScoreAdjustment Optional single number added to the current (evaluation file) no
#' threshold. Requires `fullScoringFile`; cannot be combined with `noScoreOverride`.
#' @param adjustableRuleId Optional character vector of rule IDs (for example `"ER1"`) whose
#' points are changed by `dxRuleScoreOverride` or `dxRuleScoreAdjustment` before summing.
#' @param dxRuleScoreOverride Optional single number that replaces the points of every
#' `adjustableRuleId` rule. Must be `>= 0`; requires `fullScoringFile` and `adjustableRuleId`.
#' @param dxRuleScoreAdjustment Optional single number added to the points of every
#' `adjustableRuleId` rule; the result must not be negative. Requires `fullScoringFile` and
#' `adjustableRuleId`; cannot be combined with `dxRuleScoreOverride`.
#' @param phenotypePopulationCache Optional environment. When supplied, the phenotype cohort
#' downloaded from the database is stored in it and reused by later calls for the same
#' connection, schemas, cohort, and settings, so repeated scoring runs query the database once.
#' Defaults to `NULL` (always download).
#' @param evaluationAdjusted Logical; set `TRUE` when `evaluationCohortFile` already holds totals
#' re-scored with the adjustment arguments (so they are not applied again and are only recorded
#' in the output). Cannot be combined with `fullScoringFile`. Defaults to `FALSE`.
#' @param randomCohortDefinitionId Optional cohort ID of the random comparison cohort in the same
#' cohort table. When supplied, only phenotype rows for subjects in that cohort are downloaded;
#' the metrics are unchanged because other subjects never enter the comparison.
#'
#' @return
#' A data.frame with metrics for `caseZeroAsNonCase` (always), and for
#' `caseZeroAsCase` when `caseZeroAsCase = TRUE`.
#'
#' @export
testPhenotypeAlgorithmRubric <- function(phenotype,
                                         analysisName,
                                         runDateTime,
                                         connectionDetails,
                                         evaluationCohortFile,
                                         phenotypeCohortId,
                                         cdmDatabaseSchema,
                                         databaseId,
                                         cohortDatabaseSchema,
                                         cohortTable,
                                         yesScoreOverride = NULL,
                                         noScoreOverride = NULL,
                                         outputFolder = NULL,
                                         sampleSubjectCount = 10,
                                         frequencyPlotFpFnScores = FALSE,
                                         generateTestingPhenotypeFirst = FALSE,
                                         webapiBaseUrl = NULL,
                                         phenotypeCohortName = NULL,
                                         generationIncremental = TRUE,
                                         splayPrior = 7,
                                         splayPost = 7,
                                         requireObservationPeriod = TRUE,
                                         cohort_start_date = NULL,
                                         cohort_end_date = NULL,
                                         caseZeroAsCase = FALSE,
                                         fullScoringFile = NULL,
                                         excludeNegativeScores = FALSE,
                                         yesScoreAdjustment = NULL,
                                         noScoreAdjustment = NULL,
                                         adjustableRuleId = NULL,
                                         dxRuleScoreOverride = NULL,
                                         dxRuleScoreAdjustment = NULL,
                                         evaluationAdjusted = FALSE,
                                         randomCohortDefinitionId = NULL,
                                         phenotypePopulationCache = NULL) {
  message(paste0("Starting rubric phenotype testing for condition: ", phenotype))

  if (length(connectionDetails) == 0) {
    stop("Must supply connectionDetails")
  }
  if (cohortDatabaseSchema == "") {
    stop("Must have a defined cohort schema")
  }
  if (cohortTable == "") {
    stop("Must have a defined cohort table")
  }
  if (is.null(phenotypeCohortId) || length(phenotypeCohortId) == 0) {
    stop("Must have a defined phenotypeCohortId")
  }
  stopifnot(is.null(outputFolder) || (is.character(outputFolder) && length(outputFolder) == 1 && nzchar(outputFolder)))
  stopifnot(is.numeric(sampleSubjectCount), length(sampleSubjectCount) == 1, sampleSubjectCount > 0)
  stopifnot(is.logical(frequencyPlotFpFnScores), length(frequencyPlotFpFnScores) == 1)
  stopifnot(is.logical(caseZeroAsCase), length(caseZeroAsCase) == 1)
  stopifnot(is.logical(excludeNegativeScores), length(excludeNegativeScores) == 1, !is.na(excludeNegativeScores))
  stopifnot(is.null(fullScoringFile) || (is.character(fullScoringFile) && length(fullScoringFile) == 1 && nzchar(fullScoringFile)))
  stopifnot(is.logical(evaluationAdjusted), length(evaluationAdjusted) == 1, !is.na(evaluationAdjusted))
  if (isTRUE(evaluationAdjusted) && !is.null(fullScoringFile)) {
    stop("Provide fullScoringFile or evaluationAdjusted = TRUE, not both")
  }
  hasScoringSource <- !is.null(fullScoringFile) || isTRUE(evaluationAdjusted)
  if (isTRUE(excludeNegativeScores) && !hasScoringSource) {
    stop("excludeNegativeScores = TRUE requires fullScoringFile")
  }
  if ((!is.null(yesScoreAdjustment) || !is.null(noScoreAdjustment)) && !hasScoringSource) {
    stop("yesScoreAdjustment/noScoreAdjustment require fullScoringFile")
  }
  adjustableRuleId <- toupper(trimws(as.character(unlist(adjustableRuleId, use.names = FALSE))))
  adjustableRuleId <- adjustableRuleId[!is.na(adjustableRuleId) & nzchar(adjustableRuleId)]
  if (!is.null(dxRuleScoreOverride) || !is.null(dxRuleScoreAdjustment)) {
    if (!is.null(dxRuleScoreOverride) && !is.null(dxRuleScoreAdjustment)) {
      stop("Provide either dxRuleScoreOverride or dxRuleScoreAdjustment, not both")
    }
    if (length(adjustableRuleId) == 0L) {
      stop("dxRuleScoreOverride/dxRuleScoreAdjustment require adjustableRuleId")
    }
    if (!hasScoringSource) {
      stop("dxRuleScoreOverride/dxRuleScoreAdjustment require fullScoringFile")
    }
  }

  parseCohortDate <- function(dateValue, dateName) {
    if (is.null(dateValue)) {
      return(as.Date(NA))
    }
    if (!is.character(dateValue) || length(dateValue) != 1) {
      stop(paste0(dateName, " must be a character value in MM/DD/YYYY format"))
    }
    parsedDate <- as.Date(dateValue, format = "%m/%d/%Y")
    if (is.na(parsedDate)) {
      stop(paste0(dateName, " must be a valid date in MM/DD/YYYY format (example: 01/01/2016)"))
    }
    parsedDate
  }

  parsedCohortStartDate <- parseCohortDate(cohort_start_date, "cohort_start_date")
  parsedCohortEndDate <- parseCohortDate(cohort_end_date, "cohort_end_date")

  if (!is.na(parsedCohortStartDate) && !is.na(parsedCohortEndDate) && parsedCohortStartDate > parsedCohortEndDate) {
    stop("cohort_start_date must be less than or equal to cohort_end_date")
  }

  cohortIdVec <- unique(as.integer(phenotypeCohortId))
  cohortIdVec <- cohortIdVec[!is.na(cohortIdVec)]
  if (length(cohortIdVec) == 0) {
    stop("No valid phenotypeCohortId values provided")
  }

  phenotypeLabelByCohortId <- stats::setNames(rep(phenotype, length(cohortIdVec)), as.character(cohortIdVec))
  if (!is.null(webapiBaseUrl) && is.character(webapiBaseUrl) && nzchar(webapiBaseUrl)) {
    for (currentCohortId in cohortIdVec) {
      webapiName <- getWebapiCohortName(webapiBaseUrl = webapiBaseUrl, cohortId = currentCohortId)
      if (!is.na(webapiName) && nzchar(webapiName)) {
        phenotypeLabelByCohortId[[as.character(currentCohortId)]] <- webapiName
      }
    }
  }

  if (isTRUE(generateTestingPhenotypeFirst)) {
    if (is.null(webapiBaseUrl) || !is.character(webapiBaseUrl) || !nzchar(webapiBaseUrl)) {
      stop("webapiBaseUrl must be provided when generateTestingPhenotypeFirst = TRUE")
    }

    for (currentCohortId in cohortIdVec) {
      message(paste0("Generating testing phenotype cohort before analysis (cohortId=", currentCohortId, ")..."))
      generateTestingPhenotypeCohort(
        connectionDetails = connectionDetails,
        webapiBaseUrl = webapiBaseUrl,
        phenotypeCohortId = currentCohortId,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        phenotypeCohortName = if (is.null(phenotypeCohortName) || !nzchar(phenotypeCohortName)) {
          NULL
        } else {
          paste0(phenotypeCohortName, "_", currentCohortId)
        },
        incremental = isTRUE(generationIncremental)
      )
    }
  }

  message("Loading evaluation cohort file...")
  evaluation <- readEvaluationCohortFile(evaluationCohortFile)

  if (isTRUE(evaluationAdjusted)) {
    message("Using evaluation totals already re-scored with the adjusted scoring rules.")
  } else if (!is.null(fullScoringFile)) {
    message("Re-scoring evaluation subjects from fullScoringFile using adjusted scoring rules...")
    evaluation <- applyAdjustedScoringToEvaluation(
      evaluation = evaluation,
      fullScoringFile = fullScoringFile,
      excludeNegativeScores = excludeNegativeScores,
      yesScoreOverride = yesScoreOverride,
      noScoreOverride = noScoreOverride,
      yesScoreAdjustment = yesScoreAdjustment,
      noScoreAdjustment = noScoreAdjustment,
      adjustableRuleId = adjustableRuleId,
      dxRuleScoreOverride = dxRuleScoreOverride,
      dxRuleScoreAdjustment = dxRuleScoreAdjustment
    )
  }

  message("Deriving case flags from total/yesScore/noScore using active thresholds...")
  evaluation <- computeCaseFromScores(
    evaluation,
    yesScoreOverride = yesScoreOverride,
    noScoreOverride = noScoreOverride
  )

  if (!is.na(parsedCohortStartDate)) {
    evaluation <- evaluation[evaluation$cohortStartDate >= parsedCohortStartDate, , drop = FALSE]
  }
  if (!is.na(parsedCohortEndDate)) {
    evaluation <- evaluation[evaluation$cohortStartDate <= parsedCohortEndDate, , drop = FALSE]
  }

  if (!is.na(parsedCohortStartDate) || !is.na(parsedCohortEndDate)) {
    message(paste0(
      "Applied cohort date filter to evaluation file: ",
      if (is.na(parsedCohortStartDate)) "-Inf" else as.character(parsedCohortStartDate),
      " to ",
      if (is.na(parsedCohortEndDate)) "Inf" else as.character(parsedCohortEndDate),
      ". Retained ", nrow(evaluation), " rows."
    ))
  }

  if (nrow(evaluation) == 0) {
    message("No evaluation rows remain after cohort date filtering.")
    return(data.frame())
  }

  allResults <- list()
  caseZeroHandlingOptions <- if (isTRUE(caseZeroAsCase)) c(FALSE, TRUE) else c(FALSE)

  for (currentCohortId in cohortIdVec) {
    message(paste0("Downloading phenotype cohort from database for cohortId=", currentCohortId, "..."))
    phenoPop <- getPhenotypePopulation(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      phenotypeCohortId = currentCohortId,
      requireObservationPeriod = requireObservationPeriod,
      randomCohortDefinitionId = randomCohortDefinitionId,
      cache = phenotypePopulationCache
    )

    if (nrow(phenoPop) == 0) {
      message(paste0("Phenotype cohort is empty for cohortId=", currentCohortId, "."))
      next
    }

    phenoPop$subjectId <- as.numeric(phenoPop$subjectId)
    phenoPop$phenoCohortStartDate <- as.Date(phenoPop$phenoCohortStartDate)

    if (!is.na(parsedCohortStartDate)) {
      phenoPop <- phenoPop[phenoPop$phenoCohortStartDate >= parsedCohortStartDate, , drop = FALSE]
    }
    if (!is.na(parsedCohortEndDate)) {
      phenoPop <- phenoPop[phenoPop$phenoCohortStartDate <= parsedCohortEndDate, , drop = FALSE]
    }

    if (nrow(phenoPop) == 0) {
      message(paste0("Phenotype cohort has no rows in requested date window for cohortId=", currentCohortId, "."))
      next
    }

    message(paste0("Joining evaluation and phenotype cohorts for cohortId=", currentCohortId, "..."))
    fullTable <- merge(
      evaluation,
      phenoPop[, c("subjectId", "phenoCohortStartDate")],
      by = "subjectId",
      all.x = TRUE
    )

    fullTable$cohortStartDate <- as.Date(fullTable$cohortStartDate)
    fullTable$phenoCohortStartDate <- as.Date(fullTable$phenoCohortStartDate)

    fullTable$case <- 0L
    fullTable$case[fullTable$total >= fullTable$yesScoreUsed] <- 1L
    fullTable$case[fullTable$total <= fullTable$noScoreUsed] <- -1L

    fullTable$hasPhenoRecord <- !is.na(fullTable$phenoCohortStartDate)
    fullTable$inRangeMatch <- fullTable$hasPhenoRecord & (
      fullTable$cohortStartDate >= (fullTable$phenoCohortStartDate - as.integer(splayPrior)) &
        fullTable$cohortStartDate <= (fullTable$phenoCohortStartDate + as.integer(splayPost))
    )

    dateMismatchRows <- fullTable[
      fullTable$hasPhenoRecord & !fullTable$inRangeMatch,
      c("subjectId", "cohortStartDate", "phenoCohortStartDate", "total", "yesScoreUsed", "noScoreUsed", "case"),
      drop = FALSE
    ]
    if (nrow(dateMismatchRows) > 0) {
      dateMismatchRows <- unique(dateMismatchRows)
    }
    names(dateMismatchRows) <- c(
      "evaluationSubjectId",
      "evaluationCohortStartDate",
      "testingCohortStartDate",
      "evaluationTotalScore",
      "evaluationYesScoreUsed",
      "evaluationNoScoreUsed",
      "evaluationCase"
    )

    testedSubjects <- unique(phenoPop$subjectId)
    matchedTestedSubjects <- unique(fullTable$subjectId[fullTable$inRangeMatch])
    testedSubjectsWithNoMatch <- setdiff(testedSubjects, matchedTestedSubjects)

    if (length(testedSubjectsWithNoMatch) > 0) {
      fullTable <- fullTable[!(fullTable$subjectId %in% testedSubjectsWithNoMatch), , drop = FALSE]
    }

    if (nrow(fullTable) == 0) {
      message(paste0("No retained rows after matching rules for cohortId=", currentCohortId, "."))
      next
    }

    fullTable <- fullTable[order(
      fullTable$subjectId,
      as.Date(fullTable$phenoCohortStartDate),
      as.Date(fullTable$cohortStartDate)
    ), , drop = FALSE]

    keepMatched <- fullTable$inRangeMatch & !duplicated(fullTable$subjectId)
    keepNonTested <- !(fullTable$subjectId %in% matchedTestedSubjects) & !fullTable$hasPhenoRecord
    fullTable <- fullTable[keepMatched | keepNonTested, , drop = FALSE]

    fullTable$inPhenotype <- FALSE
    fullTable$inPhenotype[fullTable$subjectId %in% matchedTestedSubjects] <- TRUE

    if (nrow(fullTable) == 0) {
      message(paste0("No rows available for scoring for cohortId=", currentCohortId, "."))
      next
    }

    message(paste0("Computing metrics for selected case=0 handling scenario(s) for cohortId=", currentCohortId, "..."))
    metricsList <- lapply(caseZeroHandlingOptions, function(caseZeroAsCase) {
      scoreRubricVsCohort(fullTable, caseZeroAsCase = caseZeroAsCase)
    })

    results <- do.call(rbind, metricsList)
    results$phenotype <- phenotypeLabelByCohortId[[as.character(currentCohortId)]]
    results$databaseId <- databaseId
    results$cohortId <- as.integer(currentCohortId)
    results$analysisName <- analysisName
    results$runDateTime <- runDateTime
    results$splayPrior <- as.integer(splayPrior)
    results$splayPost <- as.integer(splayPost)
    results$yesScoreOverride <- if (is.null(yesScoreOverride)) NA_real_ else as.numeric(yesScoreOverride)
    results$noScoreOverride <- if (is.null(noScoreOverride)) NA_real_ else as.numeric(noScoreOverride)
    results$excludeNegativeScores <- isTRUE(excludeNegativeScores)
    results$yesScoreAdjustment <- if (is.null(yesScoreAdjustment)) NA_real_ else as.numeric(yesScoreAdjustment)
    results$noScoreAdjustment <- if (is.null(noScoreAdjustment)) NA_real_ else as.numeric(noScoreAdjustment)
    results$adjustableRuleId <- if (length(adjustableRuleId) == 0L) NA_character_ else paste(adjustableRuleId, collapse = ";")
    results$dxRuleScoreOverride <- if (is.null(dxRuleScoreOverride)) NA_real_ else as.numeric(dxRuleScoreOverride)
    results$dxRuleScoreAdjustment <- if (is.null(dxRuleScoreAdjustment)) NA_real_ else as.numeric(dxRuleScoreAdjustment)

    results <- results[, c(
      "phenotype", "databaseId", "cohortId", "analysisName", "runDateTime",
      "caseZeroHandling",
      "totalSubjects", "estimatedCases", "estimatedPrevalence",
      "truePositives", "trueNegatives", "falsePositives", "falseNegatives",
      "sensitivity", "sensitivityCi95Lb", "sensitivityCi95Ub",
      "ppv", "ppvCi95Lb", "ppvCi95Ub",
      "specificity", "specificityCi95Lb", "specificityCi95Ub",
      "npv", "npvCi95Lb", "npvCi95Ub",
      "f1Score", "splayPrior", "splayPost", "yesScoreOverride", "noScoreOverride",
      "excludeNegativeScores", "yesScoreAdjustment", "noScoreAdjustment",
      "adjustableRuleId", "dxRuleScoreOverride", "dxRuleScoreAdjustment"
    )]

    writeRubricSupportingReports(
      outputFolder = outputFolder,
      phenotype = phenotype,
      analysisName = analysisName,
      runDateTime = runDateTime,
      phenotypeCohortId = currentCohortId,
      sampleSubjectCount = as.integer(sampleSubjectCount),
      fullTable = fullTable,
      dateMismatchRows = dateMismatchRows,
      caseZeroHandlingOptions = caseZeroHandlingOptions
    )

    if (isTRUE(frequencyPlotFpFnScores)) {
      writeFpFnFrequencyPlots(
        outputFolder = outputFolder,
        phenotype = phenotype,
        analysisName = analysisName,
        runDateTime = runDateTime,
        phenotypeCohortId = currentCohortId,
        fullTable = fullTable,
        caseZeroHandlingOptions = caseZeroHandlingOptions
      )
    }

    allResults[[length(allResults) + 1]] <- results
  }

  if (length(allResults) == 0) {
    message("Rubric phenotype algorithm evaluation completed with no analyzable cohorts.")
    return(data.frame())
  }

  message("Rubric phenotype algorithm evaluation completed.")
  do.call(rbind, allResults)
}

formatRubricPhenotypeResults <- function(results) {
  if (is.null(results) || nrow(results) == 0) {
    return(data.frame())
  }

  formatted <- results

  formatted$sensitivity95Ci <- sprintf(
    "%0.3f (%0.3f - %0.3f)",
    formatted$sensitivity,
    formatted$sensitivityCi95Lb,
    formatted$sensitivityCi95Ub
  )
  formatted$ppv95Ci <- sprintf(
    "%0.3f (%0.3f - %0.3f)",
    formatted$ppv,
    formatted$ppvCi95Lb,
    formatted$ppvCi95Ub
  )
  formatted$specificity95Ci <- sprintf(
    "%0.3f (%0.3f - %0.3f)",
    formatted$specificity,
    formatted$specificityCi95Lb,
    formatted$specificityCi95Ub
  )
  formatted$npv95Ci <- sprintf(
    "%0.3f (%0.3f - %0.3f)",
    formatted$npv,
    formatted$npvCi95Lb,
    formatted$npvCi95Ub
  )

  formatted$f1Score <- sprintf("%0.3f", formatted$f1Score)
  formatted$estimatedPrevalence <- sprintf("%0.3f", 100 * formatted$estimatedPrevalence)

  formatted$sensitivity <- sprintf("%0.6f", formatted$sensitivity)
  formatted$sensitivityCi95Lb <- sprintf("%0.6f", formatted$sensitivityCi95Lb)
  formatted$sensitivityCi95Ub <- sprintf("%0.6f", formatted$sensitivityCi95Ub)
  formatted$ppv <- sprintf("%0.6f", formatted$ppv)
  formatted$ppvCi95Lb <- sprintf("%0.6f", formatted$ppvCi95Lb)
  formatted$ppvCi95Ub <- sprintf("%0.6f", formatted$ppvCi95Ub)
  formatted$specificity <- sprintf("%0.6f", formatted$specificity)
  formatted$specificityCi95Lb <- sprintf("%0.6f", formatted$specificityCi95Lb)
  formatted$specificityCi95Ub <- sprintf("%0.6f", formatted$specificityCi95Ub)
  formatted$npv <- sprintf("%0.6f", formatted$npv)
  formatted$npvCi95Lb <- sprintf("%0.6f", formatted$npvCi95Lb)
  formatted$npvCi95Ub <- sprintf("%0.6f", formatted$npvCi95Ub)

  ciColumns <- c("sensitivity95Ci", "ppv95Ci", "specificity95Ci", "npv95Ci")
  otherColumns <- setdiff(names(formatted), ciColumns)
  anchor <- match("analysisName", otherColumns)
  if (!is.na(anchor)) {
    formatted <- formatted[, c(otherColumns[seq_len(anchor)], ciColumns, otherColumns[-seq_len(anchor)]), drop = FALSE]
  }

  formatted
}

#' Test phenotype algorithm using rubric-evaluation file and return formatted report table
#'
#' @inheritParams testPhenotypeAlgorithmRubric
#'
#' @return
#' A formatted report table similar to testPhenotypeAlgorithm output style,
#' with a row for `caseZeroAsNonCase` (always), and a row for
#' `caseZeroAsCase` when `caseZeroAsCase = TRUE`.
#'
#' @export
testPhenotypeAlgorithmRubricReport <- function(phenotype,
                                               analysisName,
                                               runDateTime,
                                               connectionDetails,
                                               evaluationCohortFile,
                                               phenotypeCohortId,
                                               cdmDatabaseSchema,
                                               databaseId,
                                               cohortDatabaseSchema,
                                               cohortTable,
                                               yesScoreOverride = NULL,
                                               noScoreOverride = NULL,
                                               outputFolder = NULL,
                                               sampleSubjectCount = 10,
                                               frequencyPlotFpFnScores = FALSE,
                                               generateTestingPhenotypeFirst = FALSE,
                                               webapiBaseUrl = NULL,
                                               phenotypeCohortName = NULL,
                                               generationIncremental = TRUE,
                                               splayPrior = 7,
                                               splayPost = 7,
                                               requireObservationPeriod = TRUE,
                                               cohort_start_date = NULL,
                                               cohort_end_date = NULL,
                                               caseZeroAsCase = FALSE,
                                               fullScoringFile = NULL,
                                               excludeNegativeScores = FALSE,
                                               yesScoreAdjustment = NULL,
                                               noScoreAdjustment = NULL,
                                               adjustableRuleId = NULL,
                                               dxRuleScoreOverride = NULL,
                                               dxRuleScoreAdjustment = NULL,
                                               evaluationAdjusted = FALSE,
                                               randomCohortDefinitionId = NULL,
                                               phenotypePopulationCache = NULL) {
  raw <- testPhenotypeAlgorithmRubric(
    phenotype = phenotype,
    analysisName = analysisName,
    runDateTime = runDateTime,
    connectionDetails = connectionDetails,
    evaluationCohortFile = evaluationCohortFile,
    phenotypeCohortId = phenotypeCohortId,
    cdmDatabaseSchema = cdmDatabaseSchema,
    databaseId = databaseId,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    yesScoreOverride = yesScoreOverride,
    noScoreOverride = noScoreOverride,
    outputFolder = outputFolder,
    sampleSubjectCount = sampleSubjectCount,
    frequencyPlotFpFnScores = frequencyPlotFpFnScores,
    generateTestingPhenotypeFirst = generateTestingPhenotypeFirst,
    webapiBaseUrl = webapiBaseUrl,
    phenotypeCohortName = phenotypeCohortName,
    generationIncremental = generationIncremental,
    splayPrior = splayPrior,
    splayPost = splayPost,
    requireObservationPeriod = requireObservationPeriod,
    cohort_start_date = cohort_start_date,
    cohort_end_date = cohort_end_date,
    caseZeroAsCase = caseZeroAsCase,
    fullScoringFile = fullScoringFile,
    excludeNegativeScores = excludeNegativeScores,
    yesScoreAdjustment = yesScoreAdjustment,
    noScoreAdjustment = noScoreAdjustment,
    adjustableRuleId = adjustableRuleId,
    dxRuleScoreOverride = dxRuleScoreOverride,
    dxRuleScoreAdjustment = dxRuleScoreAdjustment,
    evaluationAdjusted = evaluationAdjusted,
    randomCohortDefinitionId = randomCohortDefinitionId,
    phenotypePopulationCache = phenotypePopulationCache
  )

  formatRubricPhenotypeResults(raw)
}

#' Build FP/FN subject scoring criteria report
#'
#' Reads a `*_subject_samples.csv` file, a `fullScoringFile_*.rds` artifact,
#' and a `*_conciseRubric*.csv` file, then creates a subject-level report for
#' FP/FN rows showing which rules scored and the corresponding rubric criteria.
#'
#' @param subjectSampleFile Path to `*_subject_samples.csv`.
#' @param fullScoringFile Path to `fullScoringFile_*.rds`, or its already-loaded data frame.
#' @param conciseRubricFile Path to concise rubric CSV.
#' @param outputFile Optional output CSV path. If `NULL`, writes next to
#' `subjectSampleFile` with suffix `_scoringCriteriaReport.csv`.
#' @param classLabels Character vector of class labels to include. Defaults to
#' `c("FP", "FN")`.
#' @param cohortId Optional cohort ID override. When provided, this value is
#' used for all output rows in `CohortId`. When `NULL`, `cohortId` is read from
#' `subjectSampleFile`.
#' @param adjustableRuleId Optional rule IDs (for example `"ER1"`) whose `score` in the report is
#' changed by `dxRuleScoreOverride` or `dxRuleScoreAdjustment`. Use the same values as the scoring
#' run that produced `subjectSampleFile`, so the per-rule scores agree with `evaluationTotalScore`.
#' @param dxRuleScoreOverride Optional single number that replaces the report score of every
#' `adjustableRuleId` row.
#' @param dxRuleScoreAdjustment Optional single number added to the report score of every
#' `adjustableRuleId` row; cannot be combined with `dxRuleScoreOverride`.
#'
#' @return A data.frame written to CSV with one row per subject-rule score hit;
#' subjects with no score hits are retained with `NA` scoring columns.
#' @examples
#' \dontrun{
#' report <- createSubjectSampleScoringReport(
#'   subjectSampleFile = "P:/llm/rubrics/output_condition_subject_samples.csv",
#'   fullScoringFile = "P:/llm/rubrics/scoring/GERD/CCAE/fullScoringFile_Cohort14412_V1.rds",
#'   conciseRubricFile = "P:/llm/rubrics/GERD/GERD_conciseRubric_V1.csv",
#'   cohortId = 14412,
#'   adjustableRuleId = "ER1",
#'   dxRuleScoreOverride = 8
#' )
#' }
#' @export
createSubjectSampleScoringReport <- function(subjectSampleFile,
                                             fullScoringFile,
                                             conciseRubricFile,
                                             outputFile = NULL,
                                             classLabels = c("FP", "FN"),
                                             cohortId = NULL,
                                             adjustableRuleId = NULL,
                                             dxRuleScoreOverride = NULL,
                                             dxRuleScoreAdjustment = NULL) {
  stopifnot(is.character(subjectSampleFile), length(subjectSampleFile) == 1, nzchar(subjectSampleFile))
  if (!is.null(dxRuleScoreOverride) && !is.null(dxRuleScoreAdjustment)) {
    stop("Provide either dxRuleScoreOverride or dxRuleScoreAdjustment, not both")
  }
  if ((!is.null(dxRuleScoreOverride) || !is.null(dxRuleScoreAdjustment)) && length(adjustableRuleId) == 0L) {
    stop("dxRuleScoreOverride/dxRuleScoreAdjustment require adjustableRuleId")
  }
  stopifnot(is.data.frame(fullScoringFile) || (is.character(fullScoringFile) && length(fullScoringFile) == 1 && nzchar(fullScoringFile)))
  stopifnot(is.character(conciseRubricFile), length(conciseRubricFile) == 1, nzchar(conciseRubricFile))
  stopifnot(is.null(outputFile) || (is.character(outputFile) && length(outputFile) == 1 && nzchar(outputFile)))
  stopifnot(is.character(classLabels), length(classLabels) > 0)
  stopifnot(is.null(cohortId) || (length(cohortId) == 1 && !is.na(as.numeric(cohortId))))

  if (!file.exists(subjectSampleFile)) {
    stop("subjectSampleFile not found: ", subjectSampleFile)
  }
  if (!is.data.frame(fullScoringFile) && !file.exists(fullScoringFile)) {
    stop("fullScoringFile not found: ", fullScoringFile)
  }
  if (!file.exists(conciseRubricFile)) {
    stop("conciseRubricFile not found: ", conciseRubricFile)
  }

  sampleDf <- utils::read.csv(subjectSampleFile, stringsAsFactors = FALSE)
  requiredSampleCols <- c("classLabel", "evaluationSubjectId", "testingSubjectId")
  missingSampleCols <- setdiff(requiredSampleCols, names(sampleDf))
  if (length(missingSampleCols) > 0) {
    stop("subjectSampleFile is missing required columns: ", paste(missingSampleCols, collapse = ", "))
  }

  sampleDf <- sampleDf[sampleDf$classLabel %in% classLabels, , drop = FALSE]
  if (nrow(sampleDf) == 0) {
    stop("No rows remain in subjectSampleFile after filtering classLabel to: ", paste(classLabels, collapse = ", "))
  }

  if (is.null(cohortId)) {
    if (!("cohortId" %in% names(sampleDf))) {
      stop("subjectSampleFile is missing cohortId column; provide cohortId argument.")
    }
    sampleDf$CohortId <- as.numeric(sampleDf$cohortId)
  } else {
    sampleDf$CohortId <- rep(as.numeric(cohortId), nrow(sampleDf))
  }

  sampleDf$sampleRowId <- seq_len(nrow(sampleDf))
  sampleDf$subjectId <- sampleDf$evaluationSubjectId
  replaceWithTesting <- is.na(sampleDf$subjectId) & !is.na(sampleDf$testingSubjectId)
  sampleDf$subjectId[replaceWithTesting] <- sampleDf$testingSubjectId[replaceWithTesting]
  sampleDf$subjectId <- as.numeric(sampleDf$subjectId)

  if ("cohortStartDate" %in% names(sampleDf)) {
    sampleDf$cohortStartDate <- as.Date(sampleDf$cohortStartDate)
  } else {
    sampleDf$cohortStartDate <- as.Date(NA)
  }

  if (all(is.na(sampleDf$subjectId))) {
    stop("All filtered rows have missing subject IDs (evaluationSubjectId/testingSubjectId).")
  }

  scoredRaw <- if (is.data.frame(fullScoringFile)) fullScoringFile else readRDS(fullScoringFile)
  if (is.null(scoredRaw)) {
    scoredRaw <- data.frame(
      cohortDefinitionId = numeric(),
      subjectId = numeric(),
      cohortStartDate = as.Date(character()),
      rubricId = character(),
      ruleId = character(),
      score = numeric(),
      stringsAsFactors = FALSE
    )
  }
  scored <- as.data.frame(scoredRaw, stringsAsFactors = FALSE)

  requiredScoredCols <- c("subjectId", "ruleId")
  missingScoredCols <- setdiff(requiredScoredCols, names(scored))
  if (length(missingScoredCols) > 0) {
    stop("fullScoringFile is missing required columns: ", paste(missingScoredCols, collapse = ", "))
  }

  if (!("cohortStartDate" %in% names(scored))) {
    scored$cohortStartDate <- as.Date(NA)
  }
  if (!("rubricId" %in% names(scored))) {
    scored$rubricId <- NA_character_
  }
  if (!("score" %in% names(scored))) {
    scored$score <- NA_real_
  }
  if (!("cohortDefinitionId" %in% names(scored))) {
    scored$cohortDefinitionId <- NA_real_
  }

  scored$subjectId <- as.numeric(scored$subjectId)
  scored$cohortStartDate <- as.Date(scored$cohortStartDate)

  adjustableRuleId <- toupper(trimws(as.character(unlist(adjustableRuleId, use.names = FALSE))))
  if (length(adjustableRuleId) > 0L && (!is.null(dxRuleScoreOverride) || !is.null(dxRuleScoreAdjustment))) {
    ruleMatch <- toupper(trimws(as.character(scored$ruleId))) %in% adjustableRuleId
    scored$score <- as.numeric(scored$score)
    scored$score[ruleMatch] <- if (!is.null(dxRuleScoreOverride)) {
      as.numeric(dxRuleScoreOverride)
    } else {
      scored$score[ruleMatch] + as.numeric(dxRuleScoreAdjustment)
    }
  }
  mergeCols <- c("subjectId", "cohortStartDate", "cohortDefinitionId", "rubricId", "ruleId", "score")
  mergeCols <- mergeCols[mergeCols %in% names(scored)]
  merged <- merge(
    x = sampleDf,
    y = scored[, mergeCols, drop = FALSE],
    by = "subjectId",
    all.x = TRUE,
    suffixes = c("Sample", "Scoring")
  )

  if ("cohortStartDateSample" %in% names(merged) && "cohortStartDateScoring" %in% names(merged)) {
    sameDate <- is.na(merged$cohortStartDateSample) |
      is.na(merged$cohortStartDateScoring) |
      (merged$cohortStartDateSample == merged$cohortStartDateScoring)
    mergedFiltered <- merged[sameDate, , drop = FALSE]

    missingSampleRows <- setdiff(sampleDf$sampleRowId, unique(mergedFiltered$sampleRowId))
    if (length(missingSampleRows) > 0) {
      addBack <- sampleDf[sampleDf$sampleRowId %in% missingSampleRows, , drop = FALSE]
      addBack$cohortDefinitionId <- NA_real_
      addBack$rubricId <- NA_character_
      addBack$ruleId <- NA_character_
      addBack$score <- NA_real_
      addBack$cohortStartDateScoring <- as.Date(NA)
      names(addBack)[names(addBack) == "cohortStartDate"] <- "cohortStartDateSample"
      mergedFiltered <- rbind(mergedFiltered, addBack[, names(mergedFiltered), drop = FALSE])
    }
    merged <- mergedFiltered
  }

  rubric <- utils::read.csv(conciseRubricFile, stringsAsFactors = FALSE)
  if (!("ruleId" %in% names(rubric)) || !("criteria" %in% names(rubric))) {
    stop("conciseRubricFile must contain at least columns: ruleId, criteria")
  }

  rubricCols <- c("ruleId", "criteria", "score", "type", "rationale", "neededConceptSets")
  rubricIdCol <- NULL
  if ("rubricId" %in% names(rubric)) {
    rubricIdCol <- "rubricId"
  } else if ("rubric" %in% names(rubric)) {
    rubricIdCol <- "rubric"
  }
  if (!is.null(rubricIdCol)) {
    rubricCols <- unique(c(rubricIdCol, rubricCols))
  }
  rubricCols <- rubricCols[rubricCols %in% names(rubric)]
  rubricLookup <- rubric[, rubricCols, drop = FALSE]

  if (!is.null(rubricIdCol)) {
    names(rubricLookup)[names(rubricLookup) == rubricIdCol] <- "rubricId"
  }
  if ("criteria" %in% names(rubricLookup)) {
    names(rubricLookup)[names(rubricLookup) == "criteria"] <- "ruleCriteria"
  }
  if ("score" %in% names(rubricLookup)) {
    names(rubricLookup)[names(rubricLookup) == "score"] <- "rubricRuleScore"
  }

  joinBy <- "ruleId"
  if ("rubricId" %in% names(merged) && "rubricId" %in% names(rubricLookup)) {
    joinBy <- c("rubricId", "ruleId")
  }

  out <- merge(
    x = merged,
    y = rubricLookup,
    by = joinBy,
    all.x = TRUE,
    sort = FALSE
  )

  out$hasScoring <- !is.na(out$ruleId)

  if ("cohortStartDateSample" %in% names(out)) {
    out$cohortStartDate <- out$cohortStartDateSample
  }

  if ("score.x" %in% names(out) && !("score" %in% names(out))) {
    out$score <- out$score.x
  }
  if ("score.y" %in% names(out) && !("score" %in% names(out))) {
    out$score <- out$score.y
  }

  out <- out[order(out$sampleRowId, out$rubricId, ithRuleIdOrder(out$ruleId)), , drop = FALSE]

  requiredOutputCols <- c(
    "CohortId",
    "classLabel",
    "subjectId",
    "cohortStartDate",
    "phenoCohortStartDate",
    "evaluationTotalScore",
    "hasScoring",
    "rubricId",
    "ruleId",
    "score",
    "ruleCriteria",
    "type",
    "evaluationYesScoreUsed",
    "evaluationNoScoreUsed",
    "evaluationCase"
  )

  for (colName in requiredOutputCols) {
    if (!(colName %in% names(out))) {
      out[[colName]] <- NA
    }
  }
  out <- out[, requiredOutputCols, drop = FALSE]

  if (is.null(outputFile)) {
    sampleBase <- tools::file_path_sans_ext(basename(subjectSampleFile))
    outputFile <- file.path(dirname(subjectSampleFile), paste0(sampleBase, "_scoringCriteriaReport.csv"))
  }

  utils::write.csv(out, outputFile, row.names = FALSE)
  message("Wrote subject scoring criteria report: ", outputFile)

  out
}

# -----------------------------
# Example usage:
# -----------------------------
# connectionDetails <- DatabaseConnector::createConnectionDetails(
#   dbms = "spark",
#   server = "<databricks-host>",
#   user = "token",
#   password = "<personal-access-token>",
#   pathToDriver = "<path-to-jdbc-driver>",
#   port = "443",
#   extraSettings = "httpPath=<sql-warehouse-http-path>;ssl=1;transportMode=http"
# )
#
# rawResults <- testPhenotypeAlgorithmRubric(
#   phenotype = "Stevens-Johnson syndrome",
#   analysisName = "rubric_validation",
#   runDateTime = as.character(Sys.time()),
#   connectionDetails = connectionDetails,
#   evaluationCohortFile = "p:/path/to/evaluation_cohort.csv",
#   phenotypeCohortId = 26187001,
#   cdmDatabaseSchema = "catalog.cdm_schema",
#   databaseId = "my_database",
#   cohortDatabaseSchema = "catalog.results_schema",
#   cohortTable = "random_cohort",
#   yesScoreOverride = NULL,
#   noScoreOverride = NULL,
#   outputFolder = "p:/path/to/output",
#   sampleSubjectCount = 10,
#   frequencyPlotFpFnScores = TRUE,
#   generateTestingPhenotypeFirst = TRUE,
#   webapiBaseUrl = "https://your-ohdsi-server/WebAPI",
#   phenotypeCohortName = "Stevens-Johnson syndrome",
#   generationIncremental = TRUE,
#   splayPrior = 7,
#   splayPost = 7,
#   requireObservationPeriod = TRUE,
#   cohort_start_date = "01/01/2020",
#   cohort_end_date = "12/31/2020"
# )
# print(rawResults)
#
# reportResults <- testPhenotypeAlgorithmRubricReport(
#   phenotype = "Stevens-Johnson syndrome",
#   analysisName = "rubric_validation",
#   runDateTime = as.character(Sys.time()),
#   connectionDetails = connectionDetails,
#   evaluationCohortFile = "p:/path/to/evaluation_cohort.csv",
#   phenotypeCohortId = 26187001,
#   cdmDatabaseSchema = "catalog.cdm_schema",
#   databaseId = "my_database",
#   cohortDatabaseSchema = "catalog.results_schema",
#   cohortTable = "random_cohort",
#   yesScoreOverride = NULL,
#   noScoreOverride = NULL,
#   outputFolder = "p:/path/to/output",
#   sampleSubjectCount = 10,
#   frequencyPlotFpFnScores = TRUE,
#   generateTestingPhenotypeFirst = TRUE,
#   webapiBaseUrl = "https://your-ohdsi-server/WebAPI",
#   phenotypeCohortName = "Stevens-Johnson syndrome",
#   generationIncremental = TRUE,
#   splayPrior = 7,
#   splayPost = 7,
#   requireObservationPeriod = TRUE,
#   cohort_start_date = "01/01/2020",
#   cohort_end_date = "12/31/2020"
# )
# print(reportResults)
