# Incremental pipeline: build the evaluation cohort, score it, and run the phenotype reports,
# re-running only the units whose inputs changed since they last ran.
#
# Each unit of work has a fingerprint made of named components (database, settings, rubric,
# concept sets, cohort definitions, code, upstream run). The ledger in
# <outputDirectory>/pipeline/pipelineLedger.csv stores the fingerprint and component hashes of
# the last run of every unit, so the plan can say which component changed.

ithPipelineStepNames <- c("cohort", "score", "report")

ithPipelineLedgerColumns <- c(
  "step", "unitKey", "conditionName", "databaseId", "analysisName", "phenotypeCohortId",
  "fingerprint", "components", "status", "runId", "finishedAt", "message"
)

ithPipelineDirectory <- function(outputDirectory) {
  file.path(outputDirectory, "pipeline")
}

ithPipelineLedgerPath <- function(outputDirectory) {
  file.path(ithPipelineDirectory(outputDirectory), "pipelineLedger.csv")
}

ithPipelineEmptyLedger <- function() {
  ledger <- as.data.frame(
    stats::setNames(replicate(length(ithPipelineLedgerColumns), character(0), simplify = FALSE), ithPipelineLedgerColumns),
    stringsAsFactors = FALSE
  )
  ledger
}

ithPipelineReadLedger <- function(outputDirectory) {
  path <- ithPipelineLedgerPath(outputDirectory)
  if (!file.exists(path)) {
    return(ithPipelineEmptyLedger())
  }
  ledger <- utils::read.csv(path, colClasses = "character", na.strings = character(0), check.names = FALSE)
  for (column in setdiff(ithPipelineLedgerColumns, names(ledger))) {
    ledger[[column]] <- rep("", nrow(ledger))
  }
  ledger[, ithPipelineLedgerColumns, drop = FALSE]
}

ithPipelineWriteLedger <- function(ledger, outputDirectory) {
  path <- ithPipelineLedgerPath(outputDirectory)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Written to a temporary file first so an interrupted write never leaves a truncated ledger.
  temporaryPath <- paste0(path, ".tmp")
  utils::write.csv(ledger[, ithPipelineLedgerColumns, drop = FALSE], temporaryPath, row.names = FALSE)
  if (!file.copy(temporaryPath, path, overwrite = TRUE)) {
    stop("Could not write the pipeline ledger: ", path)
  }
  unlink(temporaryPath)
  invisible(path)
}

ithPipelineUnitKey <- function(step, conditionName, databaseId, analysisName = "", phenotypeCohortId = "") {
  paste(step, conditionName, databaseId, analysisName, phenotypeCohortId, sep = " | ")
}

ithPipelineHash <- function(x) {
  rlang::hash(x)
}

ithPipelineFileHash <- function(directory, baseName) {
  latest <- ithFindLatestVersion(directory, baseName)
  if (is.null(latest)) {
    return("missing")
  }
  unname(tools::md5sum(file.path(directory, latest)))
}

# Hash of every Ithaca function and object the step's entry function can reach. Source references
# are dropped first, so comment-only and formatting-only edits do not count as code changes.
ithPipelineCodeHash <- function(entryNames, namespace = asNamespace("Ithaca")) {
  namespaceNames <- ls(namespace, all.names = TRUE)
  seen <- character(0)
  queue <- entryNames
  while (length(queue) > 0L) {
    name <- queue[[1]]
    queue <- queue[-1]
    if (name %in% seen || !(name %in% namespaceNames)) next
    seen <- c(seen, name)
    value <- get(name, envir = namespace, inherits = FALSE)
    if (!is.function(value)) next
    text <- deparse(utils::removeSource(value))
    explicit <- unlist(regmatches(text, gregexpr("Ithaca:::?[A-Za-z0-9_.]+", text)), use.names = FALSE)
    referenced <- c(codetools::findGlobals(value, merge = TRUE), sub("^Ithaca:::?", "", explicit))
    queue <- c(queue, setdiff(intersect(referenced, namespaceNames), seen))
  }
  seen <- sort(seen)
  texts <- vapply(seen, function(name) {
    value <- get(name, envir = namespace, inherits = FALSE)
    if (is.function(value)) {
      value <- utils::removeSource(value)
    }
    paste(deparse(value), collapse = "\n")
  }, character(1))
  ithPipelineHash(texts)
}

ithPipelineStepCodeHash <- function(step) {
  entry <- switch(step,
    cohort = "buildRandomOmopCohortFromConfig",
    score = "scoreSubjectsAgainstRandomCohortFromConfig",
    report = "testPhenotypeAlgorithmRubricReportFromConfig"
  )
  ithPipelineCodeHash(entry)
}

# Hash of each WebAPI cohort definition's expression (JSON, else SQL), keyed by cohort ID.
ithPipelineFetchDefinitionHashes <- function(cohortIds, webapiBaseUrl) {
  exported <- ROhdsiWebApi::exportCohortDefinitionSet(
    baseUrl = webapiBaseUrl,
    cohortIds = as.integer(cohortIds),
    generateStats = FALSE
  )
  textColumn <- if ("json" %in% names(exported)) "json" else "sql"
  stats::setNames(
    vapply(exported[[textColumn]], function(x) ithPipelineHash(as.character(x)), character(1)),
    as.character(exported$cohortId)
  )
}

# Fetches definition hashes once per run. When WebAPI cannot be reached the last fetched hashes
# are reused (definitionHashes.csv), so an outage does not look like a definition change.
ithPipelineDefinitionHashes <- function(cohortIds, webapiBaseUrl, outputDirectory, verbose = TRUE) {
  cohortIds <- sort(unique(as.integer(cohortIds[!is.na(cohortIds)])))
  if (length(cohortIds) == 0L) {
    return(character(0))
  }
  cachePath <- file.path(ithPipelineDirectory(outputDirectory), "definitionHashes.csv")
  cached <- if (file.exists(cachePath)) {
    table <- utils::read.csv(cachePath, colClasses = "character")
    stats::setNames(table$definitionHash, table$cohortId)
  } else {
    character(0)
  }

  fetched <- tryCatch(
    ithPipelineFetchDefinitionHashes(cohortIds, webapiBaseUrl),
    error = function(e) {
      warning(
        "Could not fetch cohort definitions from WebAPI (", conditionMessage(e), "); ",
        "using the last fetched definition hashes.",
        call. = FALSE, immediate. = TRUE
      )
      NULL
    }
  )
  if (!is.null(fetched)) {
    cached[names(fetched)] <- fetched
    dir.create(dirname(cachePath), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(
      data.frame(cohortId = names(cached), definitionHash = unname(cached), stringsAsFactors = FALSE),
      cachePath, row.names = FALSE
    )
  }
  result <- cached[as.character(cohortIds)]
  result[is.na(result)] <- "unavailable"
  stats::setNames(unname(result), as.character(cohortIds))
}

ithPipelineLoadConceptLibrary <- function(config) {
  tryCatch(
    ithLoadConceptSetLibraryForScoring(
      outputDirectory = as.character(config$outputDirectory[[1]]),
      seedLibraryPath = config$seedLibraryPath,
      libraryVersion = config$conceptLibraryVersion
    )[[1]],
    error = function(e) NULL
  )
}

# Database identity from config only: the CDM schema name carries the release (for example
# cdm_optum_extended_dod_v4155); general.databaseVersion can add a version for any database.
ithPipelineDatabaseComponent <- function(config, databaseId) {
  cohortTableName <- config$cohortTableName
  if (is.list(cohortTableName) || (!is.null(names(cohortTableName)) && databaseId %in% names(cohortTableName))) {
    cohortTableName <- cohortTableName[[databaseId]]
  }
  list(
    databaseId = databaseId,
    cdm = as.character(config$cdm[[databaseId]]),
    catalog = tryCatch(ithGetCatalog(databaseId = databaseId, config = config), error = function(e) ""),
    cohortDatabaseSchema = as.character(ithCoalesce(config$cohortDatabaseSchema, "")[[1]]),
    cohortTablePrefix = as.character(ithCoalesce(config$cohortTablePrefix, "cohort")[[1]]),
    cohortTableName = if (is.null(cohortTableName)) "" else as.character(cohortTableName[[1]]),
    databaseVersion = if (is.null(config$databaseVersion[[databaseId]])) "" else as.character(config$databaseVersion[[databaseId]][[1]])
  )
}

ithPipelineRandomDefaults <- function(config) {
  if (!is.null(config$randomCohortDefaults)) ithCoalesce(config$randomCohortDefaults, list()) else ithCoalesce(config$randomCohort, list())
}

ithPipelineSqlFileHash <- function(path) {
  resolved <- tryCatch(resolveSqlFilePath(path), error = function(e) path)
  if (!file.exists(resolved)) {
    resolved <- system.file("sql", "sql_server", basename(path), package = "Ithaca")
  }
  if (!nzchar(resolved) || !file.exists(resolved)) {
    return("missing")
  }
  unname(tools::md5sum(resolved))
}

ithPipelineCohortComponents <- function(conditionSpec, databaseId, context) {
  config <- context$config
  randomDefaults <- ithPipelineRandomDefaults(config)
  sourceId <- ithCoalesce(conditionSpec$sourceCohortDefinitionId, randomDefaults$sourceCohortDefinitionId)
  sampleFromId <- ithCoalesce(conditionSpec$randomSampleFromCohortDefinitionId, randomDefaults$randomSampleFromCohortDefinitionId)
  definitionIds <- suppressWarnings(as.integer(c(unlist(sourceId), unlist(sampleFromId))))
  definitionIds <- definitionIds[!is.na(definitionIds)]
  sqlFile <- as.character(ithCoalesce(randomDefaults$sqlFilePath, file.path("inst", "sql", "sql_server", "build_random_omop_cohort.sql"))[[1]])
  validationSqlFile <- as.character(ithCoalesce(randomDefaults$validationSqlFilePath, file.path("inst", "sql", "sql_server", "validate_overwrite_random_omop_cohort.sql"))[[1]])
  list(
    database = context$databaseComponents[[databaseId]],
    settings = list(
      randomCohort = randomDefaults,
      sourceCohortDefinitionId = sourceId,
      randomCohortDefinitionId = conditionSpec$randomCohortDefinitionId,
      randomSampleFromCohortDefinitionId = sampleFromId,
      webapiBaseUrl = config$webapiBaseUrl,
      seedLibraryPath = config$seedLibraryPath
    ),
    sqlFiles = c(ithPipelineSqlFileHash(sqlFile), ithPipelineSqlFileHash(validationSqlFile)),
    definitions = unname(context$definitionHashes[as.character(definitionIds)]),
    code = context$codeHash("cohort")
  )
}

ithPipelineConceptSetComponent <- function(conditionName, context) {
  if (is.null(context$conceptLibrary)) {
    return("unavailable")
  }
  conditionDir <- file.path(context$outputDirectory, conditionName)
  shortRubric <- tryCatch(ithLoadLatestShortRubric(conditionName, context$outputDirectory), error = function(e) NULL)
  matched <- tryCatch(ithLoadLatest(conditionDir, "matchedConceptSets.rds"), error = function(e) NULL)
  if (is.null(shortRubric) || is.null(matched)) {
    return("unavailable")
  }
  pluginNames <- sort(ithScoringSqlConceptSets(shortRubric$sqlStatement))
  lapply(stats::setNames(pluginNames, pluginNames), function(pluginName) {
    selection <- ithSelectLibraryConceptSet(pluginName, matched, context$conceptLibrary)
    list(
      status = selection$status,
      conceptSet = if (identical(selection$status, "selected")) {
        as.character(jsonlite::toJSON(selection$csJson, auto_unbox = TRUE, null = "null", digits = NA))
      } else {
        ""
      }
    )
  })
}

ithPipelineScoreComponents <- function(conditionSpec, databaseId, context, upstream) {
  conditionName <- ithConditionSpecName(conditionSpec)
  conditionDir <- file.path(context$outputDirectory, conditionName)
  list(
    upstream = upstream,
    database = context$databaseComponents[[databaseId]],
    rubric = c(
      shortRubric = ithPipelineFileHash(conditionDir, paste0(conditionName, "_shortRubric.rds")),
      rubricList = ithPipelineFileHash(conditionDir, "rubricList.rds"),
      matchedConceptSets = ithPipelineFileHash(conditionDir, "matchedConceptSets.rds")
    ),
    conceptSets = ithPipelineConceptSetComponent(conditionName, context),
    settings = list(
      randomCohortDefinitionId = conditionSpec$randomCohortDefinitionId,
      useExistingScores = isTRUE(context$config$useExistingScores)
    ),
    code = context$codeHash("score")
  )
}

ithPipelineReportComponents <- function(conditionSpec, scoringRules, databaseId, phenotypeCohortId, context, upstream) {
  conditionName <- ithConditionSpecName(conditionSpec)
  list(
    upstream = upstream,
    database = context$databaseComponents[[databaseId]],
    settings = list(
      scoringRules = scoringRules,
      testCondition = conditionSpec$testCondition,
      randomCohortDefinitionId = conditionSpec$randomCohortDefinitionId
    ),
    definition = unname(context$definitionHashes[as.character(phenotypeCohortId)]),
    conciseRubric = if (isTRUE(scoringRules$createSubjectSample)) {
      ithPipelineFileHash(file.path(context$outputDirectory, conditionName), paste0(conditionName, "_conciseRubric.csv"))
    } else {
      "notUsed"
    },
    code = context$codeHash("report")
  )
}

ithPipelineComponentHashes <- function(components) {
  vapply(components, ithPipelineHash, character(1))
}

ithPipelineFingerprint <- function(componentHashes) {
  ithPipelineHash(componentHashes[order(names(componentHashes))])
}

ithPipelineChangedComponents <- function(componentHashes, ledgerComponents) {
  previous <- tryCatch(unlist(jsonlite::fromJSON(ledgerComponents)), error = function(e) NULL)
  if (is.null(previous)) {
    return("unknown")
  }
  names <- union(names(componentHashes), names(previous))
  changed <- names[vapply(names, function(name) {
    !identical(unname(componentHashes[name]), unname(previous[name]))
  }, logical(1))]
  if (length(changed) == 0L) "unknown" else changed
}

ithPipelineChangeLabel <- function(component, step) {
  switch(component,
    upstream = if (step == "score") "evaluation cohort was rebuilt" else "scores were recomputed",
    database = "database or database version changed",
    settings = "settings changed",
    sqlFiles = "random cohort SQL changed",
    definitions = "source cohort definition changed in WebAPI",
    definition = "phenotype cohort definition changed in WebAPI",
    rubric = "rubric or concept-set matching changed",
    conceptSets = "concept sets changed in the library",
    conciseRubric = "concise rubric changed",
    code = "Ithaca code used by this step changed",
    paste(component, "changed")
  )
}

ithPipelineUpstreamState <- function(upstreamKey, ledger, plannedRuns, upstreamStep) {
  if (upstreamKey %in% plannedRuns) {
    return(list(state = "pending", value = "pending"))
  }
  row <- ledger[ledger$unitKey == upstreamKey, , drop = FALSE]
  if (nrow(row) == 0L) {
    return(list(state = "untracked", value = "untracked"))
  }
  if (!identical(row$status[[1]], "done")) {
    return(list(state = "blocked", value = "blocked", reason = paste0(upstreamStep, " step has not succeeded")))
  }
  list(state = "done", value = row$runId[[1]])
}

ithPipelineDecide <- function(step, unitKey, componentHashes, ledger, fresh, upstreamState) {
  if (identical(upstreamState$state, "blocked")) {
    return(list(action = "blocked", reason = upstreamState$reason))
  }
  if (isTRUE(fresh)) {
    return(list(action = "run", reason = "fresh run requested"))
  }
  row <- ledger[ledger$unitKey == unitKey, , drop = FALSE]
  if (nrow(row) == 0L) {
    return(list(action = "run", reason = "not run before"))
  }
  if (!identical(row$status[[1]], "done")) {
    return(list(action = "run", reason = "last run failed"))
  }
  if (identical(upstreamState$state, "pending")) {
    return(list(action = "run", reason = ithPipelineChangeLabel("upstream", step)))
  }
  if (!identical(ithPipelineFingerprint(componentHashes), row$fingerprint[[1]])) {
    changed <- ithPipelineChangedComponents(componentHashes, row$components[[1]])
    return(list(action = "run", reason = paste(vapply(changed, ithPipelineChangeLabel, character(1), step = step), collapse = "; ")))
  }
  list(action = "skip", reason = "up to date")
}

ithPipelineRunCounter <- new.env(parent = emptyenv())

# Unique per execution (even within the same second), so dependants always see a rerun.
ithPipelineNewRunId <- function(fingerprint, unitKey) {
  ithPipelineRunCounter$value <- ithCoalesce(ithPipelineRunCounter$value, 0) + 1
  ithPipelineHash(list(fingerprint, unitKey, format(Sys.time(), "%Y%m%d%H%M%OS6"), Sys.getpid(), ithPipelineRunCounter$value))
}

ithPipelineLedgerRow <- function(unit, componentHashes, status, message = "") {
  finishedAt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  fingerprint <- ithPipelineFingerprint(componentHashes)
  data.frame(
    step = unit$step,
    unitKey = unit$unitKey,
    conditionName = unit$conditionName,
    databaseId = unit$databaseId,
    analysisName = unit$analysisName,
    phenotypeCohortId = unit$phenotypeCohortId,
    fingerprint = fingerprint,
    components = as.character(jsonlite::toJSON(as.list(componentHashes), auto_unbox = TRUE)),
    status = status,
    runId = ithPipelineNewRunId(fingerprint, unit$unitKey),
    finishedAt = finishedAt,
    message = gsub("[\r\n]+", " ", as.character(message)),
    stringsAsFactors = FALSE
  )
}

ithPipelineRecord <- function(ledger, row, outputDirectory) {
  ledger <- ledger[ledger$unitKey != row$unitKey[[1]], , drop = FALSE]
  ledger <- rbind(ledger, row[, ithPipelineLedgerColumns, drop = FALSE])
  ithPipelineWriteLedger(ledger, outputDirectory)
  ledger
}

ithPipelineReportRowPath <- function(outputDirectory, unitKey) {
  file.path(ithPipelineDirectory(outputDirectory), "reportRows", paste0(ithPipelineHash(unitKey), ".rds"))
}

# --- Step runners (separate functions so tests can replace them) ---------------------------

# Builds the random cohorts for `units` (conditionName, databaseId) together so they can run on
# workers; `onUnitDone(index, error)` is called as each finishes (error is NULL on success).
ithPipelineRunCohortUnits <- function(configPath, units, envir, onUnitDone = NULL) {
  config <- ithReadIthacaWorkflowConfig(configPath)
  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)
  conditionSpecs <- config$conditionSpecs
  specNames <- vapply(conditionSpecs, function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]), character(1))
  pairs <- data.frame(
    conditionIndex = match(units$conditionName, specNames),
    databaseId = as.character(units$databaseId),
    stringsAsFactors = FALSE
  )
  tasks <- ithRandomCohortTasks(config, databases, conditionSpecs, pairs)
  ithRunRandomCohortTasks(
    tasks,
    workers = ithConfigParallelWorkers(config, "cohortGeneration"),
    stopOnFailure = FALSE,
    onUnitDone = onUnitDone
  )
  invisible(TRUE)
}

ithPipelineRunScoreUnit <- function(configPath, conditionName, databaseId, envir) {
  scoreSubjectsAgainstRandomCohortFromConfig(configPath = configPath, databaseIds = databaseId, conditionNames = conditionName, envir = envir)
  invisible(TRUE)
}

# Generates the phenotype cohorts for each database in `cohortIdsByDatabase` (a list named by
# databaseId) and returns a list of error messages named by databaseId for the ones that failed.
ithPipelineGeneratePhenotypeCohorts <- function(configPath, cohortIdsByDatabase, envir) {
  config <- ithReadIthacaWorkflowConfig(configPath)
  databases <- ithResolveDatabasesFromConfig(config = config, envir = envir)
  verbose <- isTRUE(ithCoalesce(config$verbose, TRUE))
  databaseIds <- names(cohortIdsByDatabase)
  definitionSet <- ithGetWebapiCohortDefinitionSet(
    cohortIds = unique(unlist(cohortIdsByDatabase, use.names = FALSE)),
    webapiBaseUrl = as.character(config$webapiBaseUrl[[1]]),
    verbose = verbose
  )
  jobs <- lapply(databaseIds, function(databaseId) {
    list(
      database = databases[[databaseId]],
      cohortDefinitionSet = definitionSet[definitionSet$cohortId %in% cohortIdsByDatabase[[databaseId]], , drop = FALSE],
      cohortTable = NULL
    )
  })
  outcomes <- ithGenerateCohortJobs(
    jobs,
    workers = ithConfigParallelWorkers(config, "cohortGeneration"),
    stopOnError = TRUE,
    incremental = TRUE,
    createTables = TRUE,
    stopOnFailure = FALSE,
    verbose = verbose
  )
  errors <- list()
  for (outcome in outcomes) {
    if (!is.null(outcome) && !isTRUE(outcome$ok)) {
      errors[[outcome$databaseId]] <- paste(c(errors[[outcome$databaseId]], outcome$error), collapse = "; ")
    }
  }
  errors
}

ithPipelineRunReportGroup <- function(configPath, units, phenotypePopulationCache, envir) {
  testPhenotypeAlgorithmRubricReportFromConfig(
    configPath = configPath,
    conditionNames = unique(units$conditionName),
    databaseIds = unique(units$databaseId),
    analysisNames = unique(units$analysisName),
    units = units,
    generatePhenotypeCohorts = FALSE,
    saveReports = FALSE,
    phenotypePopulationCache = phenotypePopulationCache,
    envir = envir
  )
}

# --- Adoption of outputs that existed before the pipeline ledger --------------------------

ithPipelineScoreOutputsExist <- function(context, conditionSpec, databaseId) {
  cohortDir <- file.path(context$outputDirectory, "scoring", ithConditionSpecName(conditionSpec), databaseId)
  randomId <- conditionSpec$randomCohortDefinitionId[[1]]
  !is.null(ithFindLatestVersion(cohortDir, paste0("sumCohortScores_Cohort", randomId, ".rds"))) &&
    !is.null(ithFindLatestVersion(cohortDir, paste0("fullScoringFile_Cohort", randomId, ".rds")))
}

ithPipelineExistingReportRows <- function(context, unit) {
  conditionDir <- file.path(context$outputDirectory, "scoring", unit$conditionName)
  stem <- paste0(
    ithPhenotypeReportFileStem(unit$testCondition), "_",
    ithPhenotypeReportFileStem(unit$analysisName), "_phenotypeAlgorithmRubricReports.rds"
  )
  latest <- ithFindLatestVersion(conditionDir, stem)
  if (is.null(latest)) {
    return(NULL)
  }
  # A report older than the scores it reads was built from earlier scores, so it must rerun.
  randomId <- unit$conditionSpec$randomCohortDefinitionId[[1]]
  scoreDir <- file.path(conditionDir, unit$databaseId)
  scoreFiles <- c(
    ithFindLatestVersion(scoreDir, paste0("sumCohortScores_Cohort", randomId, ".rds")),
    ithFindLatestVersion(scoreDir, paste0("fullScoringFile_Cohort", randomId, ".rds"))
  )
  if (length(scoreFiles) > 0L) {
    scoresWritten <- max(file.mtime(file.path(scoreDir, scoreFiles)))
    if (file.mtime(file.path(conditionDir, latest)) < scoresWritten) {
      return(NULL)
    }
  }
  rows <- tryCatch(readRDS(file.path(conditionDir, latest)), error = function(e) NULL)
  if (!is.data.frame(rows) || !all(c("databaseId", "cohortId", "analysisName") %in% names(rows))) {
    return(NULL)
  }
  rows <- rows[
    as.character(rows$databaseId) == unit$databaseId &
      as.character(rows$cohortId) == unit$phenotypeCohortId &
      as.character(rows$analysisName) == unit$analysisName, , drop = FALSE
  ]
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows$conditionName <- unit$conditionName
  rows$testCondition <- unit$testCondition
  if (!("reportStatus" %in% names(rows))) {
    rows$reportStatus <- "ok"
  }
  rows
}

# --- Planning -------------------------------------------------------------------------------

ithPipelineSelectConditions <- function(conditionSpecs, conditionNames) {
  if (is.null(conditionNames)) {
    return(conditionSpecs)
  }
  names <- vapply(conditionSpecs, ithConditionSpecName, character(1))
  missing <- setdiff(conditionNames, names)
  if (length(missing) > 0L) {
    stop("Requested conditionNames are not configured: ", paste(missing, collapse = ", "))
  }
  conditionSpecs[names %in% conditionNames]
}

ithPipelineCohortUnits <- function(conditionSpecs, databaseIds) {
  units <- list()
  for (conditionSpec in conditionSpecs) {
    conditionName <- ithConditionSpecName(conditionSpec)
    for (databaseId in databaseIds) {
      units[[length(units) + 1L]] <- list(
        conditionSpec = conditionSpec, conditionName = conditionName, databaseId = databaseId,
        analysisName = "", phenotypeCohortId = ""
      )
    }
  }
  units
}

ithPipelineReportUnits <- function(conditionSpecs, databaseIds, config, analysisNames = NULL, verbose = TRUE) {
  units <- list()
  for (scoringRun in ithPhenotypeReportScoringRuns(config)) {
    for (conditionSpec in conditionSpecs) {
      conditionName <- ithConditionSpecName(conditionSpec)
      cohortIds <- tryCatch(ithPhenotypeReportCohortIds(conditionSpec), error = function(e) integer(0))
      if (length(cohortIds) == 0L) {
        ithMaybeMessage(verbose, "No phenotypeCohortId for ", conditionName, "; skipping its phenotype reports.")
        next
      }
      scoringRules <- tryCatch(
        ithResolvePhenotypeReportScoring(
          config,
          conditionName,
          adjustableRuleId = conditionSpec$adjustableRuleId,
          adjustableRuleScore = conditionSpec$adjustableRuleScore,
          overrides = scoringRun
        ),
        error = function(e) list(
          invalid = conditionMessage(e),
          analysisName = trimws(as.character(ithCoalesce(
            scoringRun$analysisName,
            ithCoalesce(config$phenotypeReportScoring$default$analysisName, "rubric_validation")
          )[[1]]))
        )
      )
      if (!is.null(analysisNames) && !(scoringRules$analysisName %in% analysisNames)) next
      for (databaseId in databaseIds) {
        for (cohortId in cohortIds) {
          units[[length(units) + 1L]] <- list(
            conditionSpec = conditionSpec, conditionName = conditionName, databaseId = databaseId,
            analysisName = scoringRules$analysisName, phenotypeCohortId = as.character(cohortId),
            testCondition = as.character(conditionSpec$testCondition[[1]]), scoringRules = scoringRules
          )
        }
      }
    }
  }
  units
}

#' Run the Ithaca evaluation pipeline incrementally
#'
#' Runs the three config-driven jobs in order: building the random evaluation cohort
#' ([buildRandomOmopCohortFromConfig()]), scoring it ([scoreSubjectsAgainstRandomCohortFromConfig()]),
#' and comparing the phenotype cohorts with it ([testPhenotypeAlgorithmRubricReportFromConfig()]).
#' Only the units of work whose inputs changed since their last successful run are executed:
#'
#' * cohort (condition x database): the database (CDM schema, catalog, cohort table,
#'   `general.databaseVersion`), the `randomCohort` settings and the condition's cohort IDs,
#'   the random cohort SQL files, the source and sample-pool cohort definitions in WebAPI, and
#'   the Ithaca code this step uses.
#' * score (condition x database): a rebuilt evaluation cohort, the short rubric, rubric list and
#'   concept-set matching files, the library concept sets the rubric SQL uses, and the step's code.
#' * report (analysis x condition x database x phenotype cohort): recomputed scores, the resolved
#'   phenotypeReportScoring settings (CSV row plus config), the phenotype cohort definition in
#'   WebAPI, the concise rubric (when `createSubjectSample` is on), and the step's code.
#'
#' A change only reruns the steps it affects; for example a changed phenotype cohort definition
#' reruns only that cohort's reports. The ledger is
#' `<outputDirectory>/pipeline/pipelineLedger.csv`; deleting a row forces that unit to run.
#' A failed unit is retried on the next run and blocks the units that depend on it. After any
#' report runs, the per-condition and combined report files are rebuilt from the saved rows
#' of every report unit.
#'
#' @param configPath Path to YAML configuration file.
#' @param steps Steps to consider, any of `"cohort"`, `"score"`, `"report"`.
#' @param fresh `FALSE` (default) to run only what changed, `TRUE` to rerun every selected unit,
#'   or a character vector of steps to rerun in full (their dependants rerun too).
#' @param conditionNames Optional condition names (`rubricName` or `testCondition`) to consider.
#' @param databaseIds Optional database IDs to consider.
#' @param analysisNames Optional report analysis names to consider.
#' @param dryRun Logical; when `TRUE`, only print and return the plan.
#' @param adoptExisting Logical; when `TRUE`, units that have no ledger entry but whose outputs
#'   already exist are recorded as done instead of rerun (reports only when they are newer than
#'   the scores they read). Use once when starting to use the pipeline on an existing output
#'   folder, after any run in progress has finished.
#' @param envir Environment where legacy `databaseSpecNames` objects are looked up by name.
#'
#' @return Invisibly, a data frame with one row per unit: `step`, `conditionName`, `databaseId`,
#'   `analysisName`, `phenotypeCohortId`, `action` (`run`, `skip`, `adopt`, `blocked`),
#'   `reason`, `status` (`done`, `failed`, `skipped`, `blocked`, `planned`) and `message`.
#' @examples
#' \dontrun{
#' runIthacaPipeline(dryRun = TRUE)
#' runIthacaPipeline()
#' runIthacaPipeline(fresh = "report", conditionNames = "suicidal ideation")
#' }
#' @export
runIthacaPipeline <- function(configPath = "config.yaml",
                              steps = c("cohort", "score", "report"),
                              fresh = FALSE,
                              conditionNames = NULL,
                              databaseIds = NULL,
                              analysisNames = NULL,
                              dryRun = FALSE,
                              adoptExisting = FALSE,
                              envir = .GlobalEnv) {
  steps <- match.arg(steps, ithPipelineStepNames, several.ok = TRUE)
  steps <- ithPipelineStepNames[ithPipelineStepNames %in% steps]
  freshSteps <- if (isTRUE(fresh)) steps else if (is.character(fresh)) match.arg(fresh, ithPipelineStepNames, several.ok = TRUE) else character(0)

  config <- ithReadIthacaConditionConfig(configPath)
  if (is.null(config$cdm) || length(config$cdm) == 0L) {
    stop("runIthacaPipeline needs databases under general.cdm")
  }
  verbose <- isTRUE(ithCoalesce(config$verbose, TRUE))
  outputDirectory <- as.character(config$outputDirectory[[1]])
  allDatabaseIds <- names(config$cdm)
  if (!is.null(databaseIds)) {
    missing <- setdiff(databaseIds, allDatabaseIds)
    if (length(missing) > 0L) {
      stop("Requested databaseIds are not configured: ", paste(missing, collapse = ", "))
    }
  }
  selectedDatabaseIds <- if (is.null(databaseIds)) allDatabaseIds else databaseIds
  allConditionSpecs <- config$conditionSpecs
  if (is.null(allConditionSpecs) || length(allConditionSpecs) == 0L) {
    stop("No conditionSpecs available in config")
  }
  conditionSpecs <- ithPipelineSelectConditions(allConditionSpecs, conditionNames)

  cohortUnits <- ithPipelineCohortUnits(conditionSpecs, selectedDatabaseIds)
  reportUnits <- if ("report" %in% steps) {
    ithPipelineReportUnits(conditionSpecs, selectedDatabaseIds, config, analysisNames, verbose)
  } else {
    list()
  }

  codeHashes <- list()
  definitionIds <- integer(0)
  if ("cohort" %in% steps) {
    randomDefaults <- ithPipelineRandomDefaults(config)
    for (conditionSpec in conditionSpecs) {
      definitionIds <- c(definitionIds, suppressWarnings(as.integer(c(
        unlist(ithCoalesce(conditionSpec$sourceCohortDefinitionId, randomDefaults$sourceCohortDefinitionId)),
        unlist(ithCoalesce(conditionSpec$randomSampleFromCohortDefinitionId, randomDefaults$randomSampleFromCohortDefinitionId))
      ))))
    }
  }
  definitionIds <- c(definitionIds, as.integer(vapply(reportUnits, function(unit) unit$phenotypeCohortId, character(1))))
  webapiBaseUrl <- as.character(ithCoalesce(config$webapiBaseUrl, "")[[1]])

  context <- list(
    config = config,
    outputDirectory = outputDirectory,
    databaseComponents = stats::setNames(lapply(allDatabaseIds, ithPipelineDatabaseComponent, config = config), allDatabaseIds),
    definitionHashes = ithPipelineDefinitionHashes(definitionIds, webapiBaseUrl, outputDirectory, verbose),
    conceptLibrary = if ("score" %in% steps) ithPipelineLoadConceptLibrary(config) else NULL,
    codeHash = function(step) {
      if (is.null(codeHashes[[step]])) {
        codeHashes[[step]] <<- ithPipelineStepCodeHash(step)
      }
      codeHashes[[step]]
    }
  )

  ledger <- ithPipelineReadLedger(outputDirectory)
  plannedRuns <- character(0)
  results <- list()
  addResult <- function(unit, decision, status, message = "") {
    results[[length(results) + 1L]] <<- data.frame(
      step = unit$step, conditionName = unit$conditionName, databaseId = unit$databaseId,
      analysisName = unit$analysisName, phenotypeCohortId = unit$phenotypeCohortId,
      action = decision$action, reason = decision$reason, status = status,
      message = gsub("[\r\n]+", " ", as.character(message)), stringsAsFactors = FALSE
    )
  }
  # Records one unit's outcome (NULL error = success); returns TRUE on success.
  finishUnit <- function(unit, outcome, componentsAfter) {
    if (is.null(outcome)) {
      hashes <- ithPipelineComponentHashes(componentsAfter())
      ledger <<- ithPipelineRecord(ledger, ithPipelineLedgerRow(unit, hashes, "done"), outputDirectory)
      addResult(unit, unit$decision, "done")
      TRUE
    } else {
      ledger <<- ithPipelineRecord(ledger, ithPipelineLedgerRow(unit, unit$componentHashes, "failed", outcome), outputDirectory)
      addResult(unit, unit$decision, "failed", outcome)
      warning("Pipeline ", unit$step, " failed for ", unit$unitKey, ": ", outcome, call. = FALSE, immediate. = TRUE)
      FALSE
    }
  }
  # Executes one unit; returns TRUE on success. Errors are recorded, never re-thrown.
  executeUnit <- function(unit, runner, componentsAfter) {
    ithMaybeMessage(verbose, "Pipeline ", unit$step, ": ", unit$unitKey, " (", unit$decision$reason, ")")
    outcome <- tryCatch({
      runner()
      NULL
    }, error = function(e) conditionMessage(e))
    finishUnit(unit, outcome, componentsAfter)
  }
  plan <- function(unit, step, components, upstreamState) {
    unit$step <- step
    unit$unitKey <- ithPipelineUnitKey(step, unit$conditionName, unit$databaseId, unit$analysisName, unit$phenotypeCohortId)
    unit$components <- components
    unit$componentHashes <- ithPipelineComponentHashes(components)
    unit$decision <- ithPipelineDecide(step, unit$unitKey, unit$componentHashes, ledger, step %in% freshSteps, upstreamState)
    unit
  }

  # Cohort step. Units that need to run are built together so they can use parallel workers.
  if ("cohort" %in% steps) {
    cohortToRun <- list()
    for (unit in cohortUnits) {
      unit <- plan(unit, "cohort", ithPipelineCohortComponents(unit$conditionSpec, unit$databaseId, context), list(state = "none"))
      if (identical(unit$decision$action, "run") && isTRUE(adoptExisting) && identical(unit$decision$reason, "not run before") &&
          ithPipelineScoreOutputsExist(context, unit$conditionSpec, unit$databaseId)) {
        unit$decision <- list(action = "adopt", reason = "existing outputs adopted")
      }
      if (isTRUE(dryRun) || !(unit$decision$action %in% c("run", "adopt"))) {
        if (identical(unit$decision$action, "run")) plannedRuns <- c(plannedRuns, unit$unitKey)
        addResult(unit, unit$decision, if (isTRUE(dryRun)) "planned" else if (unit$decision$action == "skip") "skipped" else "blocked")
        next
      }
      if (identical(unit$decision$action, "adopt")) {
        ledger <- ithPipelineRecord(ledger, ithPipelineLedgerRow(unit, unit$componentHashes, "done", "adopted existing outputs"), outputDirectory)
        addResult(unit, unit$decision, "done")
        next
      }
      cohortToRun[[length(cohortToRun) + 1L]] <- unit
    }

    if (length(cohortToRun) > 0L) {
      cohortRunTable <- data.frame(
        conditionName = vapply(cohortToRun, function(u) u$conditionName, character(1)),
        databaseId = vapply(cohortToRun, function(u) u$databaseId, character(1)),
        stringsAsFactors = FALSE
      )
      recorded <- logical(length(cohortToRun))
      for (unit in cohortToRun) {
        ithMaybeMessage(verbose, "Pipeline cohort: ", unit$unitKey, " (", unit$decision$reason, ")")
      }
      recordCohort <- function(index, error) {
        recorded[[index]] <<- TRUE
        finishUnit(cohortToRun[[index]], error, function() cohortToRun[[index]]$components)
      }
      batchError <- tryCatch({
        ithPipelineRunCohortUnits(configPath, cohortRunTable, envir, onUnitDone = recordCohort)
        NULL
      }, error = function(e) conditionMessage(e))
      # A unit the batch never started (for example a setup error) is recorded as failed.
      for (index in which(!recorded)) {
        recordCohort(index, ithCoalesce(batchError, "cohort build did not run"))
      }
    }
  }

  # Score step.
  if ("score" %in% steps) {
    for (unit in cohortUnits) {
      upstreamKey <- ithPipelineUnitKey("cohort", unit$conditionName, unit$databaseId, "", "")
      upstreamState <- ithPipelineUpstreamState(upstreamKey, ledger, plannedRuns, "cohort")
      components <- ithPipelineScoreComponents(unit$conditionSpec, unit$databaseId, context, upstreamState$value)
      unit <- plan(unit, "score", components, upstreamState)
      if (identical(unit$decision$action, "run") && isTRUE(adoptExisting) && identical(unit$decision$reason, "not run before") &&
          ithPipelineScoreOutputsExist(context, unit$conditionSpec, unit$databaseId)) {
        unit$decision <- list(action = "adopt", reason = "existing outputs adopted")
      }
      if (isTRUE(dryRun) || !(unit$decision$action %in% c("run", "adopt"))) {
        if (identical(unit$decision$action, "run")) plannedRuns <- c(plannedRuns, unit$unitKey)
        addResult(unit, unit$decision, if (isTRUE(dryRun)) "planned" else if (unit$decision$action == "skip") "skipped" else "blocked")
        next
      }
      if (identical(unit$decision$action, "adopt")) {
        ledger <- ithPipelineRecord(ledger, ithPipelineLedgerRow(unit, unit$componentHashes, "done", "adopted existing outputs"), outputDirectory)
        addResult(unit, unit$decision, "done")
        next
      }
      # Scoring can save a corrected short rubric, so the rubric is fingerprinted after the run.
      executeUnit(
        unit,
        function() ithPipelineRunScoreUnit(configPath, unit$conditionName, unit$databaseId, envir),
        function() ithPipelineScoreComponents(unit$conditionSpec, unit$databaseId, context, upstreamState$value)
      )
    }
  }

  # Report step.
  if ("report" %in% steps) {
    toRun <- list()
    touchedGroups <- character(0)
    for (unit in reportUnits) {
      upstreamKey <- ithPipelineUnitKey("score", unit$conditionName, unit$databaseId, "", "")
      upstreamState <- ithPipelineUpstreamState(upstreamKey, ledger, plannedRuns, "score")
      components <- ithPipelineReportComponents(
        unit$conditionSpec, unit$scoringRules, unit$databaseId, unit$phenotypeCohortId, context, upstreamState$value
      )
      unit <- plan(unit, "report", components, upstreamState)
      if (identical(unit$decision$action, "run") && isTRUE(adoptExisting) && identical(unit$decision$reason, "not run before")) {
        existingRows <- ithPipelineExistingReportRows(context, unit)
        if (!is.null(existingRows)) {
          unit$decision <- list(action = "adopt", reason = "existing outputs adopted")
          unit$existingRows <- existingRows
        }
      }
      if (isTRUE(dryRun) || !(unit$decision$action %in% c("run", "adopt"))) {
        addResult(unit, unit$decision, if (isTRUE(dryRun)) "planned" else if (unit$decision$action == "skip") "skipped" else "blocked")
        next
      }
      if (identical(unit$decision$action, "adopt")) {
        rowPath <- ithPipelineReportRowPath(outputDirectory, unit$unitKey)
        dir.create(dirname(rowPath), recursive = TRUE, showWarnings = FALSE)
        saveRDS(unit$existingRows, rowPath)
        ledger <- ithPipelineRecord(ledger, ithPipelineLedgerRow(unit, unit$componentHashes, "done", "adopted existing outputs"), outputDirectory)
        addResult(unit, unit$decision, "done")
        touchedGroups <- c(touchedGroups, paste(unit$conditionName, unit$analysisName, sep = "\r"))
        next
      }
      toRun[[length(toRun) + 1L]] <- unit
    }

    if (length(toRun) > 0L) {
      runTable <- data.frame(
        analysisName = vapply(toRun, function(u) u$analysisName, character(1)),
        conditionName = vapply(toRun, function(u) u$conditionName, character(1)),
        databaseId = vapply(toRun, function(u) u$databaseId, character(1)),
        phenotypeCohortId = as.integer(vapply(toRun, function(u) u$phenotypeCohortId, character(1))),
        stringsAsFactors = FALSE
      )
      generationErrors <- list()
      cohortIdsByDatabase <- sapply(unique(runTable$databaseId), function(databaseId) {
        unique(runTable$phenotypeCohortId[runTable$databaseId == databaseId])
      }, simplify = FALSE)
      failures <- tryCatch(
        ithPipelineGeneratePhenotypeCohorts(configPath, cohortIdsByDatabase, envir),
        error = function(e) stats::setNames(rep(list(conditionMessage(e)), length(cohortIdsByDatabase)), names(cohortIdsByDatabase))
      )
      for (databaseId in names(failures)) {
        generationErrors[[databaseId]] <- paste("phenotype cohort generation failed:", failures[[databaseId]])
      }

      phenotypePopulationCache <- new.env(parent = emptyenv())
      groupKeys <- paste(runTable$analysisName, runTable$conditionName, runTable$databaseId, sep = "\r")
      for (groupKey in unique(groupKeys)) {
        groupIndex <- which(groupKeys == groupKey)
        groupUnits <- toRun[groupIndex]
        databaseId <- runTable$databaseId[[groupIndex[[1]]]]
        groupError <- generationErrors[[databaseId]]
        rows <- NULL
        if (is.null(groupError)) {
          for (unit in groupUnits) ithMaybeMessage(verbose, "Pipeline report: ", unit$unitKey, " (", unit$decision$reason, ")")
          rows <- tryCatch(
            ithPipelineRunReportGroup(configPath, runTable[groupIndex, , drop = FALSE], phenotypePopulationCache, envir),
            error = function(e) {
              groupError <<- conditionMessage(e)
              NULL
            }
          )
        }
        for (unit in groupUnits) {
          unitRows <- if (is.data.frame(rows) && nrow(rows) > 0L) {
            rows[as.character(rows$cohortId) == unit$phenotypeCohortId & as.character(rows$databaseId) == unit$databaseId, , drop = FALSE]
          } else {
            NULL
          }
          message <- groupError
          if (is.null(message)) {
            if (is.null(unitRows) || nrow(unitRows) == 0L) {
              message <- "no report rows returned"
            } else if ("reportStatus" %in% names(unitRows) && any(startsWith(as.character(unitRows$reportStatus), "error"))) {
              message <- as.character(unitRows$reportStatus[startsWith(as.character(unitRows$reportStatus), "error")][[1]])
            }
          }
          if (!is.null(unitRows) && nrow(unitRows) > 0L) {
            rowPath <- ithPipelineReportRowPath(outputDirectory, unit$unitKey)
            dir.create(dirname(rowPath), recursive = TRUE, showWarnings = FALSE)
            saveRDS(unitRows, rowPath)
            touchedGroups <- c(touchedGroups, paste(unit$conditionName, unit$analysisName, sep = "\r"))
          }
          status <- if (is.null(message)) "done" else "failed"
          ledger <- ithPipelineRecord(ledger, ithPipelineLedgerRow(unit, unit$componentHashes, status, ithCoalesce(message, "")), outputDirectory)
          addResult(unit, unit$decision, status, ithCoalesce(message, ""))
          if (!is.null(message)) {
            warning("Pipeline report failed for ", unit$unitKey, ": ", message, call. = FALSE, immediate. = TRUE)
          }
        }
      }
    }

    if (!isTRUE(dryRun) && length(touchedGroups) > 0L) {
      ithPipelineSaveReportAggregates(config, allConditionSpecs, allDatabaseIds, unique(touchedGroups), verbose)
    }
  }

  results <- if (length(results) > 0L) do.call(rbind, results) else data.frame(
    step = character(0), conditionName = character(0), databaseId = character(0), analysisName = character(0),
    phenotypeCohortId = character(0), action = character(0), reason = character(0), status = character(0),
    message = character(0), stringsAsFactors = FALSE
  )
  ithPipelinePrintSummary(results, dryRun, verbose)
  invisible(results)
}

# Rebuilds the report files from the saved rows of every configured report unit: per-condition
# files only for the condition/analysis groups that changed, and the combined file.
ithPipelineSaveReportAggregates <- function(config, conditionSpecs, databaseIds, touchedGroups, verbose = TRUE) {
  outputDirectory <- as.character(config$outputDirectory[[1]])
  units <- ithPipelineReportUnits(conditionSpecs, databaseIds, config, verbose = FALSE)
  rows <- list()
  for (unit in units) {
    unitKey <- ithPipelineUnitKey("report", unit$conditionName, unit$databaseId, unit$analysisName, unit$phenotypeCohortId)
    rowPath <- ithPipelineReportRowPath(outputDirectory, unitKey)
    if (file.exists(rowPath)) {
      rows[[length(rows) + 1L]] <- readRDS(rowPath)
    }
  }
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  allRows <- ithBindRows(rows)
  groupKey <- paste(allRows$conditionName, allRows$analysisName, sep = "\r")
  for (key in intersect(unique(groupKey), touchedGroups)) {
    inGroup <- groupKey == key
    first <- which(inGroup)[[1]]
    ithSavePhenotypeConditionReport(
      allRows[inGroup, , drop = FALSE], outputDirectory,
      conditionName = allRows$conditionName[[first]],
      testCondition = allRows$testCondition[[first]],
      analysisName = allRows$analysisName[[first]],
      verbose = verbose
    )
  }
  ithSavePhenotypeCombinedReport(allRows, config, verbose = verbose)
}

ithPipelinePrintSummary <- function(results, dryRun, verbose) {
  if (!isTRUE(verbose)) {
    return(invisible(NULL))
  }
  if (nrow(results) == 0L) {
    message("Pipeline: nothing to do.")
    return(invisible(NULL))
  }
  counts <- stats::aggregate(list(units = results$step), by = list(step = results$step, action = results$action, status = results$status), FUN = length)
  message("Pipeline ", if (isTRUE(dryRun)) "plan" else "summary", ":\n", paste(utils::capture.output(print(counts, row.names = FALSE)), collapse = "\n"))
  toShow <- results[results$action != "skip", c("step", "conditionName", "databaseId", "analysisName", "phenotypeCohortId", "action", "reason", "status"), drop = FALSE]
  if (nrow(toShow) > 0L) {
    message(paste(utils::capture.output(print(toShow, row.names = FALSE)), collapse = "\n"))
  }
  invisible(NULL)
}
