test_that("createSubjectSampleScoringReport applies the rule score override to the per-rule scores", {
  directory <- withr::local_tempdir()
  samples <- data.frame(
    classLabel = "FP", evaluationSubjectId = 1, testingSubjectId = 1, cohortId = 5,
    cohortStartDate = "2020-01-01", stringsAsFactors = FALSE
  )
  samplePath <- file.path(directory, "samples_subject_samples.csv")
  utils::write.csv(samples, samplePath, row.names = FALSE)
  scored <- data.frame(
    cohortDefinitionId = 5, subjectId = 1, cohortStartDate = as.Date("2020-01-01"),
    rubricId = "R1", ruleId = c("ER1", "ER3"), score = c(3, 1), stringsAsFactors = FALSE
  )
  rubricPath <- file.path(directory, "conciseRubric.csv")
  utils::write.csv(
    data.frame(rubric = "R1", ruleId = c("ER1", "ER3"), criteria = c("a", "b"), score = c(3, 1), type = "evidenceRules"),
    rubricPath, row.names = FALSE
  )
  run <- function(...) {
    suppressMessages(createSubjectSampleScoringReport(
      samplePath, scored, rubricPath, outputFile = file.path(directory, "report.csv"), ...
    ))
  }

  plain <- run()
  expect_equal(plain$score[plain$ruleId == "ER1"], 3)

  overridden <- run(adjustableRuleId = "ER1", dxRuleScoreOverride = 8)
  expect_equal(overridden$score[overridden$ruleId == "ER1"], 8)
  expect_equal(overridden$score[overridden$ruleId == "ER3"], 1)

  adjusted <- run(adjustableRuleId = "ER1", dxRuleScoreAdjustment = 2)
  expect_equal(adjusted$score[adjusted$ruleId == "ER1"], 5)

  expect_error(run(dxRuleScoreOverride = 8), "require adjustableRuleId")
})
