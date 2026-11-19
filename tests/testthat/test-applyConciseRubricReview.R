applyReview <- function(folder, llm, updateSummary = FALSE) {
  suppressMessages(applyConciseRubricReview(
    "COPD",
    outputDirectory = folder,
    configPath = file.path(folder, "noConfig.yaml"),
    llmClient = llm,
    promptDir = Ithaca:::ithPromptDir(),
    updateSummary = updateSummary,
    verbose = FALSE
  ))
}

reviewRow <- function(ruleId, ...) {
  makeReviewRow(ruleId, condition = "COPD", ...)
}

setup <- function(env = parent.frame()) {
  withr::local_options(ithaca.yesCombinationReport = FALSE, .local_envir = env)
  makeRubricFixture(env = env)
}

test_that("a score-only change writes new versions without any LLM call", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow("ER3", suggestedScore = "2")))
  llm <- makeFakeLlm()

  result <- applyReview(folder, llm)

  expect_equal(result$status, "applied")
  expect_equal(readConcise(folder, 2)$score[readConcise(folder, 2)$ruleId == "ER3"], 2)
  expect_true(file.exists(conditionFile(folder, "COPD_shortRubric_V2.rds")))
  expect_length(llm$state$prompts, 0L)

  shortRubric <- readRDS(conditionFile(folder, "COPD_shortRubric_V2.rds"))$shortRubric
  expect_equal(shortRubric$sqlStatement[shortRubric$ruleId == "ER3"], "select 1 -- ER3")
})

test_that("the rubric delta lists what changed and the review marks rows as applied", {
  folder <- setup()
  reviewPath <- writeReviewFile(folder, makeReview(reviewRow("ER3", suggestedScore = "2")))

  applyReview(folder, makeFakeLlm())

  delta <- utils::read.csv(conditionFile(folder, "COPD_rubricDelta_V2.csv"), stringsAsFactors = FALSE)
  expect_equal(delta$ruleId, "ER3")
  expect_equal(delta$field, "score")
  expect_equal(delta$oldValue, 4)
  expect_equal(delta$newValue, 2)
  expect_equal(utils::read.csv(reviewPath, stringsAsFactors = FALSE)$applied, "COPD_conciseRubric_V2.csv")
})

test_that("applying the same review again does nothing", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow("ER3", suggestedScore = "2")))
  applyReview(folder, makeFakeLlm())

  second <- applyReview(folder, makeFakeLlm())

  expect_equal(second$status, "nothingAccepted")
  expect_false(file.exists(conditionFile(folder, "COPD_conciseRubric_V3.csv")))
})

test_that("a criteria change infers concept sets and regenerates only that rule's SQL", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow("ER3", suggestedCriteria = "a wider criterion")))
  llm <- makeFakeLlm(structured = fakeConceptSets)

  result <- applyReview(folder, llm)

  expect_equal(result$status, "applied")
  shortRubric <- readRDS(conditionFile(folder, "COPD_shortRubric_V2.rds"))$shortRubric
  er3 <- shortRubric[shortRubric$ruleId == "ER3", ]
  expect_equal(er3$criteria, "a wider criterion")
  expect_equal(er3$neededConceptSets, "NewSet_DIAGNOSIS_CODES")
  expect_equal(er3$sqlStatement, "select 42 as fake_sql")
  expect_equal(shortRubric$sqlStatement[shortRubric$ruleId == "AE1"], "select 1 -- AE1")
  expect_equal(llm$state$kinds, "conceptSets")
})

test_that("a summary is written when updateSummary is TRUE", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow("ER3", suggestedScore = "2")))
  llm <- makeFakeLlm(text = function(prompt) "The rubric summary.")

  applyReview(folder, llm, updateSummary = TRUE)

  expect_equal(readLines(conditionFile(folder, "COPD_RuleSummary_V1.txt")), "The rubric summary.")
})

test_that("a retyped rule gets a new ID and is reported as retyped", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow("AE1", suggestedType = "hard_exclusion")))

  applyReview(folder, makeFakeLlm())

  concise <- readConcise(folder, 2)
  expect_false("AE1" %in% concise$ruleId)
  expect_equal(concise$type[concise$ruleId == "HE2"], "hard_exclusion")
  delta <- utils::read.csv(conditionFile(folder, "COPD_rubricDelta_V2.csv"), stringsAsFactors = FALSE)
  expect_true(all(delta$changeType == "retyped"))
  expect_equal(unique(delta$previousRuleId), "AE1")
})

test_that("an accepted GLOBAL row adds a rule with the next unused ID", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow(
    "GLOBAL", suggestedType = "evidenceRules", suggestedScore = "3", suggestedCriteria = "brand new rule"
  )))
  llm <- makeFakeLlm(structured = fakeConceptSets)

  result <- applyReview(folder, llm)

  expect_equal(result$status, "applied")
  concise <- readConcise(folder, 2)
  expect_equal(concise$criteria[concise$ruleId == "ER4"], "brand new rule")
  delta <- utils::read.csv(conditionFile(folder, "COPD_rubricDelta_V2.csv"), stringsAsFactors = FALSE)
  expect_true(all(delta$changeType[delta$ruleId == "ER4"] == "added"))
})

test_that("one bad row changes nothing and is reported in the review file", {
  folder <- setup()
  reviewPath <- writeReviewFile(folder, makeReview(
    reviewRow("ER3", suggestedScore = "2"),
    reviewRow("AE1", originalCriteria = "stale text", suggestedScore = "-2")
  ))

  result <- applyReview(folder, makeFakeLlm())

  expect_equal(result$status, "issues")
  expect_false(file.exists(conditionFile(folder, "COPD_conciseRubric_V2.csv")))
  review <- utils::read.csv(reviewPath, stringsAsFactors = FALSE)
  expect_equal(review$issues[1], "")
  expect_match(review$issues[2], "review is stale")
  expect_true(all(is.na(review$applied) | review$applied == ""))
})

test_that("the no-changes message is printed when issues are found", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow("ER3", Accept = "maybe")))

  expect_message(
    applyConciseRubricReview(
      "COPD", outputDirectory = folder, configPath = file.path(folder, "noConfig.yaml"),
      llmClient = makeFakeLlm(), promptDir = Ithaca:::ithPromptDir(), updateSummary = FALSE, verbose = FALSE
    ),
    "no changes were made, there were issues with the reviewed changes"
  )
})

test_that("two accepted options of one finding are rejected", {
  folder <- setup()
  writeReviewFile(folder, makeReview(
    reviewRow("AE1", optionGroup = "G1", option = "Option 1 of 2", suggestedScore = "-2"),
    reviewRow("AE1", optionGroup = "G1", option = "Option 2 of 2", suggestedScore = "-3")
  ))

  result <- applyReview(folder, makeFakeLlm())

  expect_equal(result$status, "issues")
  expect_false(file.exists(conditionFile(folder, "COPD_conciseRubric_V2.csv")))
})

test_that("an LLM failure during apply leaves every file unchanged", {
  folder <- setup()
  reviewPath <- writeReviewFile(folder, makeReview(reviewRow("ER3", suggestedCriteria = "a wider criterion")))
  llm <- makeFakeLlm(structured = function(kind, prompt, call) stop("model unavailable"))

  result <- applyReview(folder, llm)

  expect_equal(result$status, "issues")
  expect_false(file.exists(conditionFile(folder, "COPD_conciseRubric_V2.csv")))
  expect_false(file.exists(conditionFile(folder, "COPD_shortRubric_V2.rds")))
  expect_match(utils::read.csv(reviewPath, stringsAsFactors = FALSE)$issues, "Apply failed")
})

test_that("a review file without an accepted row is left alone", {
  folder <- setup()
  writeReviewFile(folder, makeReview(reviewRow("ER3", Accept = "", suggestedScore = "2")))

  expect_equal(applyReview(folder, makeFakeLlm())$status, "nothingAccepted")
})

test_that("hand edits to the concise rubric are applied with a delta", {
  folder <- setup()
  concisePath <- conditionFile(folder, "COPD_conciseRubric_V1.csv")
  concise <- utils::read.csv(concisePath, stringsAsFactors = FALSE)
  concise$score[concise$ruleId == "ER3"] <- 3
  utils::write.csv(concise, concisePath, row.names = FALSE)

  suppressMessages(applyConciseRubricEdits(
    "COPD", outputDirectory = folder, llmClient = makeFakeLlm(), promptDir = Ithaca:::ithPromptDir(),
    updateSummary = FALSE, verbose = FALSE
  ))

  expect_equal(readConcise(folder, 2)$score[readConcise(folder, 2)$ruleId == "ER3"], 3)
  delta <- utils::read.csv(conditionFile(folder, "COPD_rubricDelta_V2.csv"), stringsAsFactors = FALSE)
  expect_equal(delta$field, "score")
})

test_that("an adjustable rule cannot be removed by hand edits", {
  folder <- setup()
  concisePath <- conditionFile(folder, "COPD_conciseRubric_V1.csv")
  concise <- utils::read.csv(concisePath, stringsAsFactors = FALSE)
  utils::write.csv(concise[concise$ruleId != "ER1", ], concisePath, row.names = FALSE)

  expect_error(
    suppressMessages(applyConciseRubricEdits(
      "COPD", outputDirectory = folder, llmClient = makeFakeLlm(), promptDir = Ithaca:::ithPromptDir(),
      updateSummary = FALSE, verbose = FALSE, adjustableRuleId = "ER1"
    )),
    "no longer has ER1"
  )
})
