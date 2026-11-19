# Cached download location so the GiBleed dataset is fetched once per machine.
if (!nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
  eunomiaCache <- file.path(tools::R_user_dir("Ithaca", "cache"), "eunomia")
  dir.create(eunomiaCache, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(EUNOMIA_DATA_FOLDER = eunomiaCache)
}

# Each call returns a database spec backed by its own copy of the GiBleed SQLite file, so tests never share state.
makeEunomiaDatabase <- function(cohortTable = "ithaca_test_cohort") {
  testthat::skip_if_not_installed("Eunomia")
  testthat::skip_if_not_installed("RSQLite")
  if (!file.exists(file.path(Sys.getenv("EUNOMIA_DATA_FOLDER"), "GiBleed_5.3.zip"))) {
    testthat::skip_if_offline("raw.githubusercontent.com")
  }
  connectionDetails <- tryCatch({
    details <- NULL
    utils::capture.output(suppressMessages(details <- Eunomia::getEunomiaConnectionDetails()))
    details
  }, error = function(e) testthat::skip(paste("Eunomia dataset unavailable:", conditionMessage(e))))
  list(
    databaseId = "Eunomia",
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    workDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = cohortTable
  )
}

eunomiaQuery <- function(database, sql) {
  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  DatabaseConnector::querySql(connection, SqlRender::translate(sql, "sqlite"), snakeCaseToCamelCase = TRUE)
}

# Cohort 1: persons with a gastrointestinal hemorrhage (index = first such condition).
# Cohort 2: everyone with a visit (index = first visit), used as the random comparison cohort.
eunomiaCohortDefinitions <- function() {
  insert <- paste0(
    "INSERT INTO @target_database_schema.@target_cohort_table ",
    "(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) "
  )
  Ithaca::ithMakeCohortDefinitionSet(
    cohortIds = c(1, 2),
    cohortSql = c(
      paste0(
        insert, "SELECT @target_cohort_id, person_id, MIN(condition_start_date), MIN(condition_start_date) ",
        "FROM @cdm_database_schema.condition_occurrence WHERE condition_concept_id = 192671 GROUP BY person_id;"
      ),
      paste0(
        insert, "SELECT @target_cohort_id, person_id, MIN(visit_start_date), MIN(visit_start_date) ",
        "FROM @cdm_database_schema.visit_occurrence GROUP BY person_id;"
      )
    ),
    cohortNames = c("giHemorrhage", "everyone")
  )
}

generateEunomiaCohorts <- function(database, definitions = eunomiaCohortDefinitions()) {
  utils::capture.output(suppressMessages(
    Ithaca:::ithGenerateCohorts(definitions, list(database), verbose = FALSE)
  ))
  invisible(definitions)
}

eunomiaCohortRows <- function(database, cohortId) {
  rows <- eunomiaQuery(database, paste0(
    "SELECT subject_id, cohort_start_date FROM main.", database$cohortTable,
    " WHERE cohort_definition_id = ", cohortId
  ))
  rows$cohortStartDate <- as.Date(rows$cohortStartDate)
  rows[order(rows$subjectId), , drop = FALSE]
}

# Subjects of cohort 2 with one of the concepts within +/- days of their index date, worked out in R.
expectedRuleSubjects <- function(database, conceptIds, days) {
  cohort <- eunomiaCohortRows(database, 2)
  conditions <- eunomiaQuery(database, paste0(
    "SELECT person_id, condition_start_date FROM main.condition_occurrence WHERE condition_concept_id IN (",
    paste(conceptIds, collapse = ","), ")"
  ))
  merged <- merge(cohort, conditions, by.x = "subjectId", by.y = "personId")
  merged$conditionStartDate <- as.Date(merged$conditionStartDate)
  inWindow <- abs(as.numeric(merged$conditionStartDate - merged$cohortStartDate)) <= days
  sort(unique(merged$subjectId[inWindow]))
}

eunomiaDescendants <- function(database, conceptId) {
  eunomiaQuery(database, paste0(
    "SELECT descendant_concept_id FROM main.concept_ancestor WHERE ancestor_concept_id = ", conceptId
  ))$descendantConceptId
}

# Rule SQL in the shape the scorer expects: returns cohortDefinitionId, subjectId, cohortStartDate.
eunomiaRuleSql <- function(conceptExpression, days) {
  paste0(
    "SELECT c.cohort_definition_id, c.subject_id, c.cohort_start_date FROM @cohortSchema.@cohortTable c ",
    "WHERE c.cohort_definition_id = @cohortId AND EXISTS (SELECT 1 FROM @databaseSchema.condition_occurrence co ",
    "WHERE co.person_id = c.subject_id AND co.condition_concept_id IN (", conceptExpression, ") ",
    "AND co.condition_start_date >= DATEADD(day, -", days, ", c.cohort_start_date) ",
    "AND co.condition_start_date <= DATEADD(day, ", days, ", c.cohort_start_date))"
  )
}

# Saves a short rubric, rubric list, and concept set library for one condition; returns the library path.
writeEunomiaRubric <- function(outputDirectory,
                               conditionName = "GI bleed",
                               usePlugin = FALSE,
                               ruleSql = NULL) {
  er1 <- if (usePlugin) "@GI_Hemorrhage_DIAGNOSIS_CODES" else "192671"
  sql <- list(
    ER1 = eunomiaRuleSql(er1, 30),
    ER2 = eunomiaRuleSql("4112343", 365),
    AE1 = eunomiaRuleSql("80180", 5)
  )
  sql[names(ruleSql)] <- ruleSql
  shortRubric <- data.frame(
    condition = conditionName, rubric = "R1", rubricName = conditionName, rubricYesScore = 7, rubricNoScore = 3,
    ruleId = names(sql), type = c("evidenceRules", "evidenceRules", "alternativeExplanations"),
    score = c(7, 2, -4), criteria = c("gi hemorrhage", "pharyngitis", "arthritis"), rationale = "r",
    neededConceptSets = c("GI_Hemorrhage_DIAGNOSIS_CODES", "Pharyngitis_DIAGNOSIS_CODES", "Arthritis_DIAGNOSIS_CODES"),
    sqlStatement = unlist(sql, use.names = FALSE), okToUse = TRUE,
    stringsAsFactors = FALSE
  )
  Ithaca:::ithSaveShortRubric(shortRubric, conditionName, outputDirectory, yesCombinationReport = FALSE)
  conditionDir <- file.path(outputDirectory, conditionName)
  Ithaca:::ithSaveVersioned(
    conditionDir,
    list(rubricList = list(list(rubricId = "R1", decisionThresholds = list(yes = 7, no = 3)))),
    "rubricList.rds"
  )

  libraryPath <- file.path(outputDirectory, "rubricConceptSetLibrary.duckdb")
  Ithaca:::ithDuckdbInitializeLibrary(libraryPath, verbose = FALSE)
  Ithaca:::ithDuckdbUpsertConceptSet(
    libraryPath = libraryPath,
    conceptSetName = "GI_Hemorrhage_DIAGNOSIS_CODES",
    cleanName = "GI Hemorrhage",
    conceptSet = list(items = list(list(
      concept = list(
        CONCEPT_ID = 192671L, CONCEPT_NAME = "Gastrointestinal hemorrhage", DOMAIN_ID = "Condition",
        VOCABULARY_ID = "SNOMED", CONCEPT_CLASS_ID = "Clinical Finding", STANDARD_CONCEPT = "S",
        CONCEPT_CODE = "74474003", INVALID_REASON = "V"
      ),
      isExcluded = FALSE, includeDescendants = TRUE, includeMapped = FALSE
    ))),
    embedding = c(0.1, 0.2, 0.3)
  )
  Ithaca:::ithSaveVersioned(
    conditionDir,
    data.frame(
      csName = "GI_Hemorrhage_DIAGNOSIS_CODES", libCsName = "GI_Hemorrhage_DIAGNOSIS_CODES",
      stringsAsFactors = FALSE
    ),
    "matchedConceptSets.rds"
  )
  libraryPath
}

scoreEunomia <- function(database, outputDirectory, libraryPath, ...) {
  env <- parent.frame()
  # The scorer writes lastQuery/query.sql into the working directory.
  withr::local_dir(withr::local_tempdir(.local_envir = env), .local_envir = env)
  suppressMessages(Ithaca:::ithScoreConditionDatabase(
    condition = list(rubricName = "GI bleed", randomCohortDefinitionId = 2L),
    database = database,
    outputDirectory = outputDirectory,
    seedLibraryPath = libraryPath,
    conceptLibraryVersion = Ithaca:::ithDuckdbGetLatestLibraryVersion(libraryPath),
    verbose = FALSE,
    ...
  ))
}

scoringDirectory <- function(outputDirectory, conditionName = "GI bleed", databaseId = "Eunomia") {
  file.path(outputDirectory, "scoring", conditionName, databaseId)
}
