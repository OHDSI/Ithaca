test_that("scoring stops before any rule runs when the random cohort has no rows", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  outputDirectory <- withr::local_tempdir()
  libraryPath <- writeEunomiaRubric(outputDirectory)
  withr::local_dir(withr::local_tempdir())
  llm <- makeFakeLlm()

  expect_error(
    suppressMessages(Ithaca:::ithScoreSubjects(
      conditionSpecs = list(list(rubricName = "GI bleed", randomCohortDefinitionId = 999L)),
      databases = list(Eunomia = database),
      outputDirectory = outputDirectory,
      seedLibraryPath = libraryPath,
      llmClient = llm,
      verbose = FALSE
    )),
    "GI bleed on Eunomia: random cohort 999 has no rows"
  )
  expect_false(dir.exists(file.path(outputDirectory, "scoring")))
  expect_false(dir.exists("lastQuery"))
})

test_that("the random cohort check lists every missing cohort and unreadable table", {
  specs <- list(
    list(rubricName = "Autism", randomCohortDefinitionId = 3417001L),
    list(rubricName = "Neutropenia", randomCohortDefinitionId = "20835001")
  )
  databases <- list(
    Ccae = list(databaseId = "Ccae", workDatabaseSchema = "work", cdmDatabaseSchema = "cdm"),
    Optum = list(databaseId = "Optum", workDatabaseSchema = "work", cdmDatabaseSchema = "cdm")
  )
  countRows <- function(cohortIds, database, cohortTable) {
    if (database$databaseId == "Optum") stop("table not found")
    c("20835001" = 5000)
  }

  error <- tryCatch(
    Ithaca:::ithCheckRandomCohortsReadyForScoring(specs, databases, list("ithaca_ccae", NULL), countRows = countRows),
    error = function(e) conditionMessage(e)
  )

  expect_match(error, "Autism on Ccae: random cohort 3417001 has no rows in work.ithaca_ccae", fixed = TRUE)
  expect_no_match(error, "Neutropenia on Ccae")
  expect_match(error, "Optum: could not read cohort table work.pv_table_cdm (table not found)", fixed = TRUE)
})

test_that("the random cohort check passes when every cohort has rows", {
  specs <- list(list(rubricName = "Neutropenia", randomCohortDefinitionId = 20835001L))
  databases <- list(Ccae = list(databaseId = "Ccae", workDatabaseSchema = "work", cdmDatabaseSchema = "cdm"))
  countRows <- function(cohortIds, database, cohortTable) c("20835001" = 5000)

  expect_true(Ithaca:::ithCheckRandomCohortsReadyForScoring(specs, databases, list(NULL), countRows = countRows))
})
