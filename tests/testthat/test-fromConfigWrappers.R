writeFromConfigFile <- function(outputDirectory, conditions) {
  specs <- vapply(conditions, function(name) {
    paste0(
      "    - testCondition: ", name, "\n      dxCriteria: outpatientOnly\n      additionalInformation: \"\"\n",
      "      rubricName: ", name, "\n      randomCohortDefinitionId: 1\n"
    )
  }, character(1))
  path <- tempfile(fileext = ".yaml")
  writeLines(paste0(
    "configVersion: 1\ngeneral:\n  outputDirectory: ", outputDirectory, "\n  verbose: false\n",
    "  dxCriteria:\n    outpatientOnly:\n      criteria: \"Two outpatient codes.\"\n      additionalPoints: 0\n",
    "  conditionSpecs:\n", paste(specs, collapse = ""),
    "other:\n  databaseSpecNames:\n    - fakeSpec\n"
  ), path)
  path
}

test_that("the config wrapper skips conditions without a review and keeps going after an error", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  writeReviewFile(folder, makeReview(makeReviewRow("ER3", condition = "COPD", suggestedScore = "2")))
  # Asthma has a review file but no concise rubric, so it fails on its own.
  dir.create(file.path(folder, "Asthma"))
  utils::write.csv(
    makeReview(makeReviewRow("ER3", condition = "Asthma", suggestedScore = "2")),
    file.path(folder, "Asthma", "Asthma_conciseRubricReview_V1.csv"),
    row.names = FALSE
  )
  config <- writeFromConfigFile(folder, c("COPD", "Asthma", "Missing"))
  testthat::local_mocked_bindings(
    ithGetLlmClientFromConfig = function(...) makeFakeLlm(),
    .package = "Ithaca"
  )

  results <- suppressMessages(applyConciseRubricReviewsFromConfig(configPath = config, updateSummary = FALSE))

  expect_equal(results$conditionName, c("COPD", "Asthma", "Missing"))
  expect_equal(results$status[1], "applied")
  expect_match(results$status[2], "^error: No concise rubric found")
  expect_equal(results$status[3], "skipped: no review file found")
})

test_that("the config wrapper rejects a config path passed as a condition name", {
  expect_error(
    applyConciseRubricReviewsFromConfig("config.yaml"),
    "looks like a config file"
  )
})

test_that("the edits wrapper applies only conditions whose concise rubric differs", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  config <- writeFromConfigFile(folder, c("COPD", "Missing"))
  concisePath <- conditionFile(folder, "COPD_conciseRubric_V1.csv")
  concise <- utils::read.csv(concisePath, stringsAsFactors = FALSE)
  concise$score[concise$ruleId == "ER3"] <- 3
  utils::write.csv(concise, concisePath, row.names = FALSE)
  testthat::local_mocked_bindings(
    ithGetLlmClientFromConfig = function(...) makeFakeLlm(),
    .package = "Ithaca"
  )

  results <- suppressMessages(applyConciseRubricEditsFromConfig(configPath = config))

  expect_equal(results$status, c("applied", "skipped: no concise rubric found"))
  expect_equal(results$changes[1], "scoresChanged")
  expect_true(file.exists(conditionFile(folder, "COPD_rubricDelta_V2.csv")))

  again <- suppressMessages(applyConciseRubricEditsFromConfig(configPath = config, conditionNames = "COPD"))
  expect_equal(again$status, "unchanged")
})
