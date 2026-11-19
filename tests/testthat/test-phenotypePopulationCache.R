test_that("getPhenotypePopulation downloads once per key when given a cache", {
  calls <- 0L
  testthat::local_mocked_bindings(
    getPhenotypePopulationUncached = function(...) {
      calls <<- calls + 1L
      data.frame(subjectId = 1, phenoCohortStartDate = as.Date("2020-01-01"))
    },
    .package = "Ithaca"
  )
  details <- list(dbms = "sqlite", server = "a")
  fetch <- function(cohortId, cache, details = details) {
    Ithaca:::getPhenotypePopulation(details, "main", "main", "cohort", cohortId, TRUE, 2L, cache = cache)
  }
  cache <- new.env(parent = emptyenv())

  fetch(1, cache, details)
  fetch(1, cache, details)
  expect_equal(calls, 1L)
  fetch(2, cache, details)
  expect_equal(calls, 2L)
  fetch(1, cache, list(dbms = "sqlite", server = "b"))
  expect_equal(calls, 3L)

  fetch(1, NULL, details)
  fetch(1, NULL, details)
  expect_equal(calls, 5L)
})
