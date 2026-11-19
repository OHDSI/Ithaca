test_that("full scoring files are combined with concise rubric text", {
  outputDirectory <- withr::local_tempdir()
  configPath <- file.path(outputDirectory, "config.yaml")
  writeLines(
    c(
      "configVersion: 1",
      "general:",
      paste0("  outputDirectory: \"", normalizePath(outputDirectory, winslash = "/", mustWork = FALSE), "\""),
      "  conditionSpecs:",
      "    - testCondition: COPD",
      "      rubricName: COPD",
      "      dxCriteria: copd",
      "      randomCohortDefinitionId: 123",
      "  dxCriteria:",
      "    copd:",
      "      criteria: COPD diagnosis",
      "      additionalPoints: 0",
      "other:",
      "  databaseSpecNames: Db1"
    ),
    configPath
  )
  fakeDatabase <- list(databaseId = "Db1")
  testEnvironment <- new.env(parent = globalenv())
  assign("Db1", fakeDatabase, envir = testEnvironment)

  conditionDirectory <- file.path(outputDirectory, "COPD")
  scoringDirectory <- file.path(outputDirectory, "scoring", "COPD", "Db1")
  dir.create(conditionDirectory, recursive = TRUE)
  dir.create(scoringDirectory, recursive = TRUE)
  utils::write.csv(
    data.frame(
      rubric = c("R1", "R1"),
      ruleId = c("ER1", "AE1"),
      criteria = c("case criterion", "alternative criterion"),
      rationale = c("case rationale", "alternative rationale"),
      stringsAsFactors = FALSE
    ),
    file.path(conditionDirectory, "COPD_conciseRubric_V1.csv"),
    row.names = FALSE
  )
  saveRDS(
    data.frame(
      cohortDefinitionId = 123,
      subjectId = c(1, 2),
      cohortStartDate = as.Date(c("2026-01-01", "2026-01-02")),
      rubricId = "R1",
      ruleId = c("ER1", "AE1"),
      score = c(4, -4),
      stringsAsFactors = FALSE
    ),
    file.path(scoringDirectory, "fullScoringFile_Cohort123_V1.rds")
  )

  result <- combineFullScoringFilesFromConfig(
    configPath = configPath,
    outputDirectory = file.path(outputDirectory, "combined"),
    envir = testEnvironment,
    verbose = FALSE
  )

  outputPath <- file.path(outputDirectory, "combined", "COPD_Db1_fullScoringFile_Cohort123_V1.csv")
  expect_equal(result[["COPD::Db1"]]$status, "written")
  expect_true(file.exists(outputPath))
  combined <- utils::read.csv(outputPath, stringsAsFactors = FALSE)
  expect_equal(combined$subjectId, c(1, 2))
  expect_equal(combined$criteria, c("case criterion", "alternative criterion"))
  expect_equal(combined$rationale, c("case rationale", "alternative rationale"))
})