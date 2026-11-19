test_that("rule IDs sort by ER, HE, AE and then by number", {
  ids <- c("AE1", "ER10", "HE2", "ER2", "GLOBAL", "ER1", "HE1", "XX1", "ER2")

  sorted <- ids[order(Ithaca:::ithRuleIdOrder(ids))]

  expect_equal(sorted, c("GLOBAL", "ER1", "ER2", "ER2", "ER10", "HE1", "HE2", "AE1", "XX1"))
})

test_that("new rules and retyped rules get IDs that were never used before", {
  conditionDir <- tempfile()
  dir.create(conditionDir)
  # A removed ER9 from an earlier version must not be handed out again.
  utils::write.csv(
    data.frame(rubric = "R1", ruleId = c("ER3", "ER9")),
    file.path(conditionDir, "COPD_conciseRubric_V1.csv"),
    row.names = FALSE
  )
  existing <- rbind(
    makeShortRubricRow("ER3", "evidenceRules", 4, "c3"),
    makeShortRubricRow("AE1", "alternativeExplanations", -4, "c-ae1")
  )
  edited <- data.frame(
    rubric = "R1",
    ruleId = c("ER3", "AE1", ""),
    type = c("evidenceRules", "hard_exclusion", "evidenceRules"),
    criteria = c("c3", "c-ae1", "new rule"),
    stringsAsFactors = FALSE
  )

  assigned <- Ithaca:::ithAssignConciseRuleIds(edited, existing, conditionDir, "COPD", verbose = FALSE)

  expect_equal(assigned$ruleId, c("ER3", "HE1", "ER10"))
})

test_that("renaming a rule or listing it twice is rejected", {
  conditionDir <- tempfile()
  dir.create(conditionDir)
  existing <- makeShortRubricRow("ER3", "evidenceRules", 4, "c3")
  base <- data.frame(rubric = "R1", ruleId = "ER3", type = "evidenceRules", criteria = "c3", stringsAsFactors = FALSE)

  renamed <- base
  renamed$ruleId <- "ER7"
  expect_error(
    Ithaca:::ithAssignConciseRuleIds(renamed, existing, conditionDir, "COPD", verbose = FALSE),
    "not in the current rubric"
  )
  expect_error(
    Ithaca:::ithAssignConciseRuleIds(rbind(base, base), existing, conditionDir, "COPD", verbose = FALSE),
    "same rule more than once"
  )
})

test_that("rule type spellings are normalized", {
  normalize <- Ithaca:::ithNormalizeReviewRuleType

  expect_equal(normalize("Hard Exclusion"), "hard_exclusion")
  expect_equal(normalize("alternativeExplanation"), "alternativeExplanations")
  expect_equal(normalize("evidence rule"), "evidenceRules")
  expect_equal(normalize("delete"), "remove")
  expect_equal(normalize(""), "")
  expect_true(is.na(normalize("nonsense")))
})

test_that("Accept values are parsed", {
  parse <- Ithaca:::ithParseReviewAccept

  expect_true(all(vapply(c("y", "YES", "true", "X", "1"), parse, logical(1))))
  expect_false(any(vapply(c("", "n", "NO", "false", "0"), parse, logical(1))))
  expect_true(is.na(parse("maybe")))
  expect_true(is.na(parse(NA)))
})

test_that("review scores resolve per rule type", {
  resolve <- Ithaca:::ithResolveReviewScore

  expect_equal(resolve("hard_exclusion", ""), -100)
  expect_equal(resolve("evidenceRules", "3"), 3)
  expect_equal(resolve("alternativeExplanations", "-2"), -2)
  expect_match(resolve("evidenceRules", ""), "is required")
  expect_match(resolve("evidenceRules", "abc"), "not a single number")
})
