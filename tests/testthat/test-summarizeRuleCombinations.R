test_that("rule combinations are pooled across databases with a no-rule row", {
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
      "  databaseSpecNames: [Db1, Db2]"
    ),
    configPath
  )
  testEnvironment <- new.env(parent = globalenv())
  assign("Db1", list(databaseId = "Db1"), envir = testEnvironment)
  assign("Db2", list(databaseId = "Db2"), envir = testEnvironment)

  dir.create(file.path(outputDirectory, "COPD"), recursive = TRUE)
  utils::write.csv(
    data.frame(rubric = "R1", ruleId = c("ER1", "ER2"), criteria = c("first", "second"), rationale = ""),
    file.path(outputDirectory, "COPD", "COPD_conciseRubric_V1.csv"),
    row.names = FALSE
  )

  writeScoring <- function(databaseId, hits, subjects) {
    directory <- file.path(outputDirectory, "scoring", "COPD", databaseId)
    dir.create(directory, recursive = TRUE)
    saveRDS(hits, file.path(directory, "fullScoringFile_Cohort123_V1.rds"))
    saveRDS(
      data.frame(subjectId = subjects, cohortStartDate = as.Date("2026-01-01")),
      file.path(directory, "finalScores_Cohort123_V1.rds")
    )
  }
  hit <- function(subjectId, ruleId) {
    data.frame(subjectId = subjectId, cohortStartDate = as.Date("2026-01-01"), rubricId = "R1", ruleId = ruleId,
               score = c(ER1 = 3, ER2 = 4)[[ruleId]])
  }
  writeScoring("Db1", rbind(hit(1, "ER1"), hit(1, "ER2"), hit(2, "ER1")), 1:4)
  writeScoring("Db2", rbind(hit(1, "ER2"), hit(2, "ER1"), hit(2, "ER2")), 1:3)

  result <- summarizeRuleCombinationsFromConfig(
    configPath, outputDirectory = file.path(outputDirectory, "out"), topN = 3, envir = testEnvironment, verbose = FALSE
  )

  expect_equal(result$COPD$status, "written")
  expect_equal(result$COPD$totalSubjects, 7L)
  summary <- utils::read.csv(result$COPD$outputPath, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(basename(result$COPD$outputPath), "COPD_ruleCombinations_Cohort123_V1.csv")
  expect_equal(names(summary), c(
    "Rank", "Combination", "Rule Count", "Total Score", "Subject Count", "Proportion Of Subjects",
    "Db1 Subject Count", "Db1 Proportion Of Subjects", "Db2 Subject Count", "Db2 Proportion Of Subjects", "Criteria"
  ))
  expect_equal(nrow(summary), 4L)
  expect_equal(summary$Combination[nrow(summary)], "(none)")
  expect_true(is.na(summary$Rank[nrow(summary)]))
  expect_equal(summary$Rank[-nrow(summary)], 1:3)
  expect_equal(summary[summary$Combination == "(none)", "Subject Count"], 3L)
  both <- summary[summary$Combination == "R1/ER1; R1/ER2", ]
  expect_equal(both[["Subject Count"]], 2L)
  expect_equal(both[["Total Score"]], 7)
  expect_equal(summary[summary$Combination == "(none)", "Total Score"], 0)
  expect_equal(c(both[["Db1 Subject Count"]], both[["Db2 Subject Count"]]), c(1L, 1L))
  expect_equal(c(both[["Db1 Proportion Of Subjects"]], both[["Db2 Proportion Of Subjects"]]), c(0.25, 0.333))
  only <- summary[summary$Combination == "R1/ER1", ]
  expect_equal(c(only[["Db1 Subject Count"]], only[["Db2 Subject Count"]]), c(1L, 0L))
  expect_equal(both$Criteria, "R1/ER1: first | R1/ER2: second")
})
