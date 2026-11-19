# Extracted from test-cohortParallel.R:41

# prequel ----------------------------------------------------------------------
okJob <- function(task) list(ok = TRUE, value = task, error = NULL, seconds = 0)
failingJob <- function(task) {
  if (identical(task, "bad")) list(ok = FALSE, value = NULL, error = "boom", seconds = 0) else okJob(task)
}

# test -------------------------------------------------------------------------
serialDatabase <- makeEunomiaDatabase()
serialDatabase$databaseId <- "EunomiaSerial"
workerDatabaseA <- makeEunomiaDatabase()
workerDatabaseA$databaseId <- "EunomiaA"
workerDatabaseB <- makeEunomiaDatabase()
workerDatabaseB$databaseId <- "EunomiaB"
definitions <- eunomiaCohortDefinitions()
utils::capture.output(suppressMessages({
    Ithaca:::ithGenerateCohorts(definitions, list(serialDatabase), workers = 1L, verbose = FALSE)
    Ithaca:::ithGenerateCohorts(definitions, list(workerDatabaseA, workerDatabaseB), workers = 2L, verbose = FALSE)
  }))
