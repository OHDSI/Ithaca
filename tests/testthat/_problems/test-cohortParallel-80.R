# Extracted from test-cohortParallel.R:80

# prequel ----------------------------------------------------------------------
okJob <- function(task) list(ok = TRUE, value = task, error = NULL, seconds = 0)
failingJob <- function(task) {
  if (identical(task, "bad")) list(ok = FALSE, value = NULL, error = "boom", seconds = 0) else okJob(task)
}

# test -------------------------------------------------------------------------
outputDirectory <- withr::local_tempdir()
configPath <- file.path(outputDirectory, "config.yaml")
writeLines(pipelineConfigText(outputDirectory, databases = c(Ccae = "cdm_ccae_v1"), conditions = c("GERD", "Asthma", "Gout")), configPath)
