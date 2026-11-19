okJob <- function(task) list(ok = TRUE, value = task, error = NULL, seconds = 0)
failingJob <- function(task) {
  if (identical(task, "bad")) list(ok = FALSE, value = NULL, error = "boom", seconds = 0) else okJob(task)
}

test_that("serial runs stop at the first failure unless told to continue", {
  stopped <- Ithaca:::ithRunJobs(list("a", "bad", "c"), failingJob, workers = 1L)
  expect_true(stopped[[1]]$ok)
  expect_false(stopped[[2]]$ok)
  expect_null(stopped[[3]])

  continued <- Ithaca:::ithRunJobs(list("a", "bad", "c"), failingJob, workers = 1L, stopOnFirstError = FALSE)
  expect_equal(vapply(continued, function(outcome) outcome$ok, logical(1)), c(TRUE, FALSE, TRUE))
})

test_that("worker runs finish every job, report each one, and keep task order", {
  started <- integer(0)
  finished <- integer(0)
  outcomes <- Ithaca:::ithRunJobs(
    list("a", "bad", "c"), failingJob, workers = 2L,
    onStart = function(index) started <<- c(started, index),
    onFinish = function(index, outcome) finished <<- c(finished, index)
  )
  expect_setequal(started, 1:3)
  expect_setequal(finished, 1:3)
  expect_equal(vapply(outcomes, function(outcome) outcome$ok, logical(1)), c(TRUE, FALSE, TRUE))
  expect_equal(outcomes[[2]]$error, "boom")
})

# SQLite allows one writer at a time, so the worker tests below use one cohort per database.
test_that("cohorts generated on workers match a serial run", {
  serialDatabase <- makeEunomiaDatabase()
  serialDatabase$databaseId <- "EunomiaSerial"
  workerDatabaseA <- makeEunomiaDatabase()
  workerDatabaseA$databaseId <- "EunomiaA"
  workerDatabaseB <- makeEunomiaDatabase()
  workerDatabaseB$databaseId <- "EunomiaB"

  definitions <- eunomiaCohortDefinitions()[2, ]
  utils::capture.output(suppressMessages({
    Ithaca:::ithGenerateCohorts(definitions, list(serialDatabase), workers = 1L, verbose = FALSE)
    Ithaca:::ithGenerateCohorts(definitions, list(workerDatabaseA, workerDatabaseB), workers = 2L, verbose = FALSE)
  }))

  expected <- eunomiaCohortRows(serialDatabase, 2)
  expect_gt(nrow(expected), 0L)
  for (database in list(workerDatabaseA, workerDatabaseB)) {
    expect_equal(eunomiaCohortRows(database, 2), expected)
  }
})

test_that("with workers each cohort in a database is its own task, unless cohorts depend on each other", {
  database <- list(databaseId = "Db", workDatabaseSchema = "s", cohortTable = "t")
  definitions <- data.frame(cohortId = 1:3, cohortName = c("a", "b", "c"), sql = "x", stringsAsFactors = FALSE)
  jobs <- list(list(database = database, cohortDefinitionSet = definitions, cohortTable = NULL))

  serial <- Ithaca:::ithCohortGenerationTasks(jobs, 1L, TRUE, TRUE)
  expect_length(serial, 1L)

  split <- Ithaca:::ithCohortGenerationTasks(jobs, 3L, TRUE, TRUE)
  expect_length(split, 3L)
  expect_equal(vapply(split, function(task) task$cohortDefinitionSet$cohortId, integer(1)), 1:3)
  expect_true(all(vapply(split, function(task) identical(task$cohortTable, "t"), logical(1))))

  definitions$subsetParent <- NA_integer_
  jobs[[1]]$cohortDefinitionSet <- definitions
  expect_length(Ithaca:::ithCohortGenerationTasks(jobs, 3L, TRUE, TRUE), 1L)
})

test_that("a failing database is reported after the others finish", {
  good <- makeEunomiaDatabase()
  good$databaseId <- "Good"
  bad <- makeEunomiaDatabase()
  bad$databaseId <- "Bad"
  bad$cdmDatabaseSchema <- "missingSchema"

  jobs <- lapply(list(good, bad), function(database) {
    list(database = database, cohortDefinitionSet = eunomiaCohortDefinitions()[2, ], cohortTable = NULL)
  })

  utils::capture.output(suppressMessages(
    result <- Ithaca:::ithGenerateCohortJobs(jobs, workers = 2L, stopOnFailure = FALSE, verbose = FALSE)
  ))
  expect_true(result[[1]]$ok)
  expect_false(result[[2]]$ok)
  expect_equal(result[[2]]$databaseId, "Bad")
  expect_gt(nrow(eunomiaCohortRows(good, 2)), 0L)

  expect_error(
    suppressMessages(utils::capture.output(Ithaca:::ithGenerateCohortJobs(jobs, workers = 2L, verbose = FALSE))),
    "Cohort generation failed for database Bad"
  )
})

cohortTask <- function(condition, databaseId, random, source, pool = NULL, table = "ithaca_t") {
  list(
    conditionName = condition,
    databaseId = databaseId,
    args = list(
      database = list(cohortDatabaseSchema = "scratch", cohortTable = table),
      cohort_definition_id = random,
      sourceCohortDefinitionId = source,
      randomSampleFromCohortDefinitionId = pool
    )
  )
}

test_that("the pre-run check passes when every cohort ID is used once per database", {
  tasks <- list(
    cohortTask("GERD", "Ccae", 901, 801),
    cohortTask("Asthma", "Ccae", 902, 802),
    cohortTask("GERD", "Optum", 901, 801, table = "ithaca_o")
  )
  expect_true(Ithaca:::ithCheckRandomCohortTasks(tasks, workers = 4L))
})

test_that("the pre-run check names a random cohort ID shared by two conditions", {
  tasks <- list(cohortTask("GERD", "Ccae", 901, 801), cohortTask("Asthma", "Ccae", 901, 802))
  expect_error(
    Ithaca:::ithCheckRandomCohortTasks(tasks, workers = 1L),
    "Cohort ID 901 on database 'Ccae' \\(table scratch.ithaca_t\\) is the random cohort for 'GERD', 'Asthma'"
  )
})

test_that("the pre-run check catches a random ID that is also a source or pool ID", {
  tasks <- list(cohortTask("GERD", "Ccae", 901, 801), cohortTask("Asthma", "Ccae", 902, 901))
  expect_error(Ithaca:::ithCheckRandomCohortTasks(tasks), "random cohort for 'GERD' but is also the source cohort for 'Asthma'")
  same <- list(cohortTask("GERD", "Ccae", 901, 801, pool = 901))
  expect_error(Ithaca:::ithCheckRandomCohortTasks(same), "also the sample pool cohort for 'GERD'")
})

test_that("a shared source cohort only stops runs with several workers", {
  tasks <- list(cohortTask("GERD", "Ccae", 901, 801), cohortTask("Asthma", "Ccae", 902, 801))
  expect_true(Ithaca:::ithCheckRandomCohortTasks(tasks, workers = 1L))
  expect_error(
    Ithaca:::ithCheckRandomCohortTasks(tasks, workers = 3L),
    "source cohort for 'GERD', 'Asthma'.*cohortGeneration set to 1"
  )
  expect_error(Ithaca:::ithCheckRandomCohortTasks(tasks, workers = 3L), "Amend sourceCohortDefinitionId")
})
