test_that("concept-set names need one token and a controlled suffix", {
  problem <- Ithaca:::ithConceptSetNameProblem

  expect_equal(problem("COPD_DIAGNOSIS_CODES"), "")
  expect_match(problem("COPD codes_DIAGNOSIS_CODES"), "single underscore-separated name")
  expect_match(problem("COPD_CONCEPTS"), "controlled suffix")
  expect_equal(problem("COPD_CONCEPTS", requireControlledSuffix = FALSE), "")
})

test_that("a malformed name is repaired when exactly one known name fits", {
  checked <- Ithaca:::ithCheckRuleConceptSets(
    "p pulmonology_PROVIDER_CODES",
    knownNames = c("pulmonology_PROVIDER_CODES", "COPD_DIAGNOSIS_CODES")
  )

  expect_equal(checked$value, "pulmonology_PROVIDER_CODES")
  expect_length(checked$repairs, 1L)
  expect_length(checked$problems, 0L)
})

test_that("a malformed name is reported when no unique known name fits", {
  none <- Ithaca:::ithCheckRuleConceptSets("p bad name_PROVIDER_CODES", knownNames = "COPD_DIAGNOSIS_CODES")
  expect_length(none$problems, 1L)

  ambiguous <- Ithaca:::ithCheckRuleConceptSets(
    "a_PROVIDER_CODES&b_PROVIDER_CODES",
    knownNames = c("a_PROVIDER_CODES", "b_PROVIDER_CODES")
  )
  expect_match(ambiguous$problems[[1]], "could be")
})

test_that("legacy names without a controlled suffix are tolerated", {
  checked <- Ithaca:::ithCheckRuleConceptSets("OldName", legacyNames = "OldName")

  expect_length(checked$problems, 0L)
  expect_equal(checked$value, "OldName")
})

test_that("a name without a controlled suffix is rejected for new rules", {
  checked <- Ithaca:::ithCheckRuleConceptSets("NewName")

  expect_match(checked$problems[[1]], "controlled suffix")
})

test_that("credentials come only from environment variables", {
  withr_free_env <- function(name, value, code) {
    old <- Sys.getenv(name, unset = NA)
    setVar <- function(v) do.call(Sys.setenv, stats::setNames(list(v), name))
    on.exit(if (is.na(old)) Sys.unsetenv(name) else setVar(old), add = TRUE)
    if (is.na(value)) Sys.unsetenv(name) else setVar(value)
    force(code)
  }

  withr_free_env("ITHACA_TEST_KEY", "  secret  ", {
    expect_equal(Ithaca:::ithGetCredential("ITHACA_TEST_KEY"), "secret")
  })
  withr_free_env("ITHACA_TEST_KEY", NA, {
    expect_error(Ithaca:::ithGetCredential("ITHACA_TEST_KEY"), "Set environment variable ITHACA_TEST_KEY")
    expect_equal(Ithaca:::ithGetCredential("ITHACA_TEST_KEY", required = FALSE), "")
  })
})
