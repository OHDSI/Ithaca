plan <- function(review, concise = makeConciseRubric(), protectedRuleIds = character()) {
  Ithaca:::ithPlanConciseRubricReview(review, concise, protectedRuleIds)
}

test_that("a score-only change plans a modify without touching criteria", {
  result <- plan(makeReview(makeReviewRow("ER3", suggestedScore = "2")))

  expect_equal(result$issues, "")
  expect_equal(result$plans[[1]]$action, "modify")
  expect_equal(result$plans[[1]]$score, 2)
  expect_null(result$plans[[1]]$criteria)
})

test_that("newCriteria replaces suggestedCriteria", {
  result <- plan(makeReview(makeReviewRow(
    "ER3", suggestedCriteria = "from reviewer", newCriteria = "from user"
  )))

  expect_equal(result$plans[[1]]$criteria, "from user")
})

test_that("rows that are not accepted or already applied are skipped", {
  result <- plan(makeReview(
    makeReviewRow("ER3", Accept = "", suggestedScore = "2"),
    makeReviewRow("AE1", applied = "file.csv", suggestedScore = "-2")
  ))

  expect_equal(result$issues, c("", ""))
  expect_equal(result$accepted, c(FALSE, FALSE))
  expect_null(result$plans[[1]])
})

test_that("an unrecognized Accept value is an issue", {
  result <- plan(makeReview(makeReviewRow("ER3", Accept = "maybe", suggestedScore = "2")))

  expect_match(result$issues[[1]], "Unrecognized Accept value")
})

test_that("a stale originalCriteria is an issue", {
  result <- plan(makeReview(makeReviewRow("ER3", originalCriteria = "old text", suggestedScore = "2")))

  expect_match(result$issues[[1]], "review is stale")
})

test_that("score signs are enforced per rule type", {
  result <- plan(makeReview(
    makeReviewRow("AE1", suggestedScore = "3"),
    makeReviewRow("ER3", suggestedScore = "-1")
  ))

  expect_match(result$issues[[1]], "must be negative")
  expect_match(result$issues[[2]], "must be positive")
})

test_that("only one row per option group can be accepted", {
  result <- plan(makeReview(
    makeReviewRow("AE1", optionGroup = "G1", suggestedScore = "-2"),
    makeReviewRow("AE1", optionGroup = "G1", suggestedScore = "-3")
  ))

  expect_match(result$issues[[1]], "More than one change accepted for the same rule")
  expect_match(result$issues[[2]], "More than one change accepted for the same rule")
})

test_that("accepting one row of an option group is fine", {
  result <- plan(makeReview(
    makeReviewRow("AE1", Accept = "", optionGroup = "G1", suggestedScore = "-2"),
    makeReviewRow("AE1", optionGroup = "G1", suggestedScore = "-3")
  ))

  expect_equal(result$issues, c("", ""))
  expect_equal(result$plans[[2]]$score, -3)
})

test_that("an option with no suggested fields is an issue until newCriteria is written", {
  empty <- makeReviewRow("AE1", optionGroup = "G1")
  expect_match(plan(makeReview(empty))$issues[[1]], "no suggestedType, suggestedScore, or suggestedCriteria")

  resolved <- plan(makeReview(makeReviewRow(
    "AE1", optionGroup = "G1", newCriteria = "new crit", suggestedScore = "-2"
  )))
  expect_equal(resolved$issues, "")
})

test_that("an ungrouped recommendation that offers options needs newCriteria", {
  result <- plan(makeReview(makeReviewRow(
    "ER3", recommendation = "Collapse ER3 or enforce a gap", suggestedScore = "2"
  )))

  expect_match(result$issues[[1]], "more than one option")
})

test_that("remove deletes a rule but the rubric must keep an evidence rule", {
  removeAe <- plan(makeReview(makeReviewRow("AE1", suggestedType = "remove")))
  expect_equal(removeAe$plans[[1]]$action, "remove")
  expect_equal(removeAe$issues, "")

  removeOnlyEvidence <- plan(makeReview(makeReviewRow("ER3", suggestedType = "remove")))
  expect_match(removeOnlyEvidence$issues[[1]], "no evidence rules")
})

test_that("an adjustable rule cannot be removed or retyped", {
  removed <- plan(makeReview(makeReviewRow("ER3", suggestedType = "remove")), protectedRuleIds = "ER3")
  retyped <- plan(
    makeReview(makeReviewRow("ER3", suggestedType = "alternativeExplanations", suggestedScore = "-2")),
    protectedRuleIds = "ER3"
  )

  expect_match(removed$issues[[1]], "adjustableRuleId")
  expect_match(retyped$issues[[1]], "must stay an evidence rule")
})

test_that("GLOBAL rows can only add a new rule", {
  discusses <- plan(makeReview(makeReviewRow(
    "GLOBAL", recommendation = "Lower ER3 to 2", finding = "ER3 is too strong"
  )))
  expect_match(discusses$issues[[1]], "GLOBAL rows can only add a new rule")

  added <- plan(makeReview(makeReviewRow(
    "GLOBAL", suggestedType = "evidenceRules", suggestedScore = "3", suggestedCriteria = "brand new rule"
  )))
  expect_equal(added$issues, "")
  expect_equal(added$plans[[1]]$action, "add")
  expect_equal(added$plans[[1]]$score, 3)
})

test_that("review files without an optionGroup column must be regenerated", {
  review <- makeReview(makeReviewRow("ER3", suggestedScore = "2"))
  review$optionGroup <- NULL

  expect_match(plan(review)$issues[[1]], "predates structured options")
})

test_that("two accepted rows for one rule conflict only on the fields they both set", {
  compatible <- plan(makeReview(
    makeReviewRow("ER3", suggestedScore = "2"),
    makeReviewRow("ER3", suggestedCriteria = "new crit")
  ))
  expect_equal(compatible$issues, c("", ""))

  conflicting <- plan(makeReview(
    makeReviewRow("ER3", suggestedScore = "2"),
    makeReviewRow("ER3", suggestedScore = "3")
  ))
  expect_match(conflicting$issues[[1]], "Conflicts with other accepted row")
})
