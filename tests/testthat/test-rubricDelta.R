deltaRows <- function(delta, ruleId, field = NULL) {
  rows <- delta[delta$ruleId == ruleId, , drop = FALSE]
  if (!is.null(field)) rows <- rows[rows$field == field, , drop = FALSE]
  rows
}

test_that("changed text, score, and SQL are listed per field", {
  old <- makeShortRubricRow("ER3", "evidenceRules", 4, "c3")
  new <- makeShortRubricRow("ER3", "evidenceRules", 2, "c3 new", sql = "select 2")

  delta <- Ithaca:::ithBuildRubricDelta(old, new, "X")

  expect_setequal(delta$field, c("score", "criteria", "sqlStatement"))
  expect_equal(unique(delta$changeType), "modified")
  expect_equal(deltaRows(delta, "ER3", "score")$oldValue, "4")
  expect_equal(deltaRows(delta, "ER3", "score")$newValue, "2")
  expect_equal(deltaRows(delta, "ER3", "criteria")$newValue, "c3 new")
  expect_equal(deltaRows(delta, "ER3", "sqlStatement")$newValue, "select 2")
})

test_that("a retyped rule is matched back to the old rule by its criteria", {
  old <- makeShortRubricRow("AE1", "alternativeExplanations", -4, "same criteria")
  new <- makeShortRubricRow("HE1", "hard_exclusion", -100, "same criteria")

  delta <- Ithaca:::ithBuildRubricDelta(old, new, "X")

  expect_equal(unique(delta$changeType), "retyped")
  expect_equal(unique(delta$previousRuleId), "AE1")
  expect_equal(deltaRows(delta, "HE1", "ruleId")$oldValue, "AE1")
  expect_equal(deltaRows(delta, "HE1", "type")$newValue, "hard_exclusion")
})

test_that("added and deleted rules list their fields", {
  old <- makeShortRubricRow("ER5", "evidenceRules", 3, "gone")
  new <- makeShortRubricRow("ER6", "evidenceRules", 1, "brand new")

  delta <- Ithaca:::ithBuildRubricDelta(old, new, "X")

  expect_equal(unique(deltaRows(delta, "ER6")$changeType), "added")
  expect_equal(unique(deltaRows(delta, "ER5")$changeType), "deleted")
  expect_equal(deltaRows(delta, "ER6", "criteria")$newValue, "brand new")
  expect_equal(deltaRows(delta, "ER5", "criteria")$oldValue, "gone")
})

test_that("yes/no threshold changes are reported at rubric level", {
  old <- makeShortRubricRow("ER3", "evidenceRules", 4, "c3", yes = 10)
  new <- makeShortRubricRow("ER3", "evidenceRules", 4, "c3", yes = 12)

  delta <- Ithaca:::ithBuildRubricDelta(old, new, "X")

  expect_equal(nrow(delta), 1L)
  expect_equal(delta$ruleId, "(rubric)")
  expect_equal(delta$field, "rubricYesScore")
})

test_that("identical rubrics give an empty delta", {
  rubric <- makeShortRubricRow("ER3", "evidenceRules", 4, "c3")

  expect_equal(nrow(Ithaca:::ithBuildRubricDelta(rubric, rubric, "X")), 0L)
})
