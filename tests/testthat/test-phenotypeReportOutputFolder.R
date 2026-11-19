test_that("the phenotype report writes its supporting files to the condition's scoring folder", {
  outputDirectory <- withr::local_tempdir()
  cohortDirectory <- file.path(outputDirectory, "scoring", "COPD", "DB1")
  dir.create(cohortDirectory, recursive = TRUE)
  saveRDS(list(), file.path(cohortDirectory, "sumCohortScores_Cohort1_V1.rds"))
  saveRDS(data.frame(), file.path(cohortDirectory, "fullScoringFile_Cohort1_V1.rds"))
  captured <- NULL
  testthat::local_mocked_bindings(
    readEvaluationCohortFile = function(...) data.frame(),
    ithDefaultCohortTable = function(...) "cohort",
    testPhenotypeAlgorithmRubricReport = function(...) {
      captured <<- list(...)
      data.frame()
    },
    .package = "Ithaca"
  )

  suppressMessages(Ithaca:::ithRunPhenotypeReportConditionDatabase(
    database = list(databaseId = "DB1", connectionDetails = list(), cdmDatabaseSchema = "cdm", cohortDatabaseSchema = "scratch"),
    conditionName = "COPD", phenotype = "COPD", randomCohortDefinitionId = 1, phenotypeCohortIds = 7,
    scoringRules = list(specified = FALSE, createSubjectSample = TRUE, analysisName = "default", sampleSubjectCount = 30),
    outputDirectory = outputDirectory, phenotypePopulationCache = NULL, verbose = FALSE
  ))

  expect_equal(normalizePath(captured$outputFolder, winslash = "/"), normalizePath(cohortDirectory, winslash = "/"))
  expect_equal(captured$sampleSubjectCount, 30)
})
