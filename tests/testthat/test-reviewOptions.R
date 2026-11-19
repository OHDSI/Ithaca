test_that("review options are split into one structured row per option", {
  findings <- data.frame(
    ruleId = c("AE1", "ER5"), category = "weighting", severity = "high", finding = "f",
    recommendation = c("Reclassify as hard exclusion", "Lower the score"),
    suggestedCriteria = "", suggestedScore = c("-100", "2"), suggestedType = c("hard_exclusion", ""),
    option2Description = c("Keep at -2", ""), option2SuggestedCriteria = "",
    option2SuggestedScore = c("-2", ""), option2SuggestedType = c("alternativeExplanations", ""),
    option3Description = c("Keep at -10", ""), option3SuggestedCriteria = "",
    option3SuggestedScore = c("-10", ""), option3SuggestedType = c("alternativeExplanations", ""),
    evidence = "e", evidenceConfidence = "high", sourceFindingIds = "1.1", rubric = "R1",
    stringsAsFactors = FALSE
  )

  expanded <- Ithaca:::ithExpandReviewOptions(findings)

  expect_equal(nrow(expanded), 4L)
  expect_equal(expanded$ruleId, c("AE1", "AE1", "AE1", "ER5"))
  expect_equal(expanded$suggestedScore, c("-100", "-2", "-10", "2"))
  expect_equal(expanded$recommendation[2:3], c("Keep at -2", "Keep at -10"))
  expect_equal(expanded$suggestedType[2], "alternativeExplanations")
  expect_false(any(grepl("^option[23]", names(expanded))))
})

test_that("every finding on the same rule becomes one option group", {
  out <- data.frame(
    rubric = "R1",
    ruleId = c("AE3", "AE3", "AE3", "ER5", "GLOBAL", "GLOBAL"),
    optionGroup = c("", "G3", "G3", "", "G1", "G1"),
    stringsAsFactors = FALSE
  )

  grouped <- Ithaca:::ithAssignReviewOptionGroups(out)

  expect_equal(grouped$optionGroup, c("G1", "G1", "G1", "", "G2", "G2"))
  expect_equal(grouped$option, c("Option 1 of 3", "Option 2 of 3", "Option 3 of 3", "", "Option 1 of 2", "Option 2 of 2"))
})

test_that("option groups are numbered across rubrics without clashing", {
  out <- data.frame(
    rubric = c("R1", "R1", "R2", "R2"),
    ruleId = c("AE1", "AE1", "AE1", "AE1"),
    optionGroup = "",
    stringsAsFactors = FALSE
  )

  grouped <- Ithaca:::ithAssignReviewOptionGroups(out)

  expect_equal(grouped$optionGroup, c("G1", "G1", "G2", "G2"))
})

test_that("options offered only in recommendation text are detected", {
  detect <- Ithaca:::ithDetectReviewAlternatives

  expect_true(nzchar(detect("Collapse ER7 or enforce a gap")))
  expect_true(nzchar(detect("Either (a) remove it or (b) lower the score")))
  expect_true(nzchar(detect("Alternatively, lower the score")))
  expect_false(nzchar(detect("Require an inpatient or emergency department visit")))
  expect_false(nzchar(detect("Lower the score to 2")))
})

test_that("runs cited by a finding are counted once each", {
  expect_equal(Ithaca:::ithCountReviewRuns(c("1.2, 3.1", "2.5", "1.1, 1.4", NA)), c(2L, 1L, 1L, 0L))
})
