review <- function(llm, outputDirectory, reviewRuns = 1L, ...) {
  suppressMessages(reviewConciseRubric(
    "COPD",
    outputDirectory = outputDirectory,
    dxCriteria = "Dx text",
    additionalInformation = "Extra text",
    llmClient = llm,
    reviewRuns = reviewRuns,
    verbose = FALSE,
    ...
  ))
}

reviewResponder <- function(...) {
  response <- makeFakeReviewResponse(...)
  function(kind, prompt, call) response
}

test_that("findings become structured review rows and are saved", {
  folder <- makeRubricFixture()
  llm <- makeFakeLlm(structured = reviewResponder(
    makeFakeFinding("ER3", suggestedScore = "2"),
    makeFakeFinding(
      "AE1", recommendation = "Reclassify as hard exclusion", suggestedType = "hard_exclusion",
      suggestedScore = "-100", option2Description = "Keep at -2",
      option2SuggestedType = "alternativeExplanations", option2SuggestedScore = "-2"
    ),
    makeFakeFinding("ER10", recommendation = "Lower the score", suggestedScore = "1")
  ))

  result <- review(llm, folder)

  expect_equal(result$ruleId, c("AE1", "AE1", "ER3", "ER10"))
  expect_equal(result$option, c("Option 1 of 2", "Option 2 of 2", "", ""))
  expect_equal(result$optionGroup[1:2], c("G1", "G1"))
  expect_equal(result$suggestedScore[1:2], c("-100", "-2"))
  expect_equal(result$timesFlagged, rep("1/1", 4))
  expect_equal(result$originalCriteria[3], "crit ER3")
  expect_equal(result$originalScore[3], "4")
  expect_false("hasAlternatives" %in% names(result))
  expect_true(all(result$Accept == ""))
  expect_true(file.exists(conditionFile(folder, "COPD_conciseRubricReview_V1.csv")))
  expect_equal(nrow(utils::read.csv(conditionFile(folder, "COPD_conciseRubricReview_V1.csv"))), 4L)
})

test_that("ER1 and ER2 are shown to the reviewer but never reviewed", {
  folder <- makeRubricFixture()
  llm <- makeFakeLlm(structured = reviewResponder(
    makeFakeFinding("ER1", suggestedScore = "2"),
    makeFakeFinding("ER2", suggestedScore = "1"),
    makeFakeFinding(
      "GLOBAL", recommendation = "Add a rule triggered when both ER1 and ER2 are met",
      suggestedType = "alternativeExplanations", suggestedScore = "-3", suggestedCriteria = "both met"
    ),
    makeFakeFinding("AE1", recommendation = "Cap the points of ER2 at 1", suggestedScore = "-1"),
    makeFakeFinding("ER3", suggestedScore = "2")
  ))

  result <- review(llm, folder)

  prompt <- llm$state$prompts[[1]]
  expect_match(prompt, '"ruleId": "ER1"', fixed = TRUE)
  expect_match(prompt, '"ruleId": "ER2"', fixed = TRUE)
  expect_match(prompt, '"fixed": true', fixed = TRUE)
  expect_equal(result$ruleId, "ER3")
})

test_that("only mentions of the exact IDs ER1 and ER2 are filtered", {
  folder <- makeRubricFixture()
  llm <- makeFakeLlm(structured = reviewResponder(
    makeFakeFinding("ER3", recommendation = "Make it more like ER12", suggestedScore = "2"),
    makeFakeFinding("AE1", recommendation = "Align with ER10", suggestedScore = "-2")
  ))

  expect_equal(review(llm, folder)$ruleId, c("AE1", "ER3"))
})

test_that("a rubric with only the fixed rules is not sent for review", {
  folder <- makeRubricFixture(ruleIds = c("ER1", "ER2"))
  llm <- makeFakeLlm()

  result <- review(llm, folder)

  expect_equal(nrow(result), 0L)
  expect_equal(unname(attr(result, "overallAssessment")), "No reviewable rules.")
  expect_equal(llm$state$structuredCalls, 0L)
})

test_that("every finding on one rule becomes a single option group", {
  folder <- makeRubricFixture()
  llm <- makeFakeLlm(structured = reviewResponder(
    makeFakeFinding("AE1", recommendation = "Expand the criterion", suggestedCriteria = "wider crit"),
    makeFakeFinding("AE1", recommendation = "Reduce the weight", suggestedScore = "-2"),
    makeFakeFinding("ER3", suggestedScore = "2")
  ))

  result <- review(llm, folder)

  expect_equal(result$ruleId, c("AE1", "AE1", "ER3"))
  expect_equal(result$optionGroup, c("G1", "G1", ""))
  expect_equal(result$option, c("Option 1 of 2", "Option 2 of 2", ""))
})

test_that("several runs are consolidated and timesFlagged comes from the cited runs", {
  folder <- makeRubricFixture()
  llm <- makeFakeLlm(structured = function(kind, prompt, call) {
    if (kind == "review") {
      return(makeFakeReviewResponse(makeFakeFinding("ER3", suggestedScore = as.character(call))))
    }
    list(
      overallAssessment = "merged",
      findings = cbind(makeFakeFinding("ER3", suggestedScore = "2"), sourceFindingIds = "1.1, 2.1, 3.1"),
      droppedFindings = data.frame(findingId = character(), reason = character())
    )
  })

  result <- review(llm, folder, reviewRuns = 3L)

  expect_equal(llm$state$kinds, c("review", "review", "review", "consolidation"))
  expect_match(llm$state$prompts[[4]], "1.1", fixed = TRUE)
  expect_equal(result$timesFlagged, "3/3")
  expect_equal(result$sourceFindingIds, "1.1, 2.1, 3.1")
  expect_true(file.exists(conditionFile(folder, "COPD_conciseRubricReviewRuns_V1.csv")))
})

test_that("run findings the consolidation does not account for are reported", {
  folder <- makeRubricFixture()
  llm <- makeFakeLlm(structured = function(kind, prompt, call) {
    if (kind == "review") {
      return(makeFakeReviewResponse(makeFakeFinding("ER3", suggestedScore = "2")))
    }
    list(
      overallAssessment = "merged",
      findings = cbind(makeFakeFinding("ER3", suggestedScore = "2"), sourceFindingIds = "1.1, 2.1"),
      droppedFindings = data.frame(findingId = character(), reason = character())
    )
  })

  expect_message(
    reviewConciseRubric(
      "COPD", outputDirectory = folder, dxCriteria = "Dx", additionalInformation = "Extra",
      llmClient = llm, reviewRuns = 3L, verbose = FALSE
    ),
    "did not account for 1 run finding"
  )
  runs <- utils::read.csv(conditionFile(folder, "COPD_conciseRubricReviewRuns_V1.csv"))
  expect_match(runs$droppedReason[runs$findingId == "3.1"], "not accounted for")
})

test_that("reviewRuns must be a positive whole number", {
  folder <- makeRubricFixture()

  expect_error(review(makeFakeLlm(), folder, reviewRuns = 0L), "reviewRuns must be a whole number")
})
