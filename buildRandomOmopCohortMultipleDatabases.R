# Build random OMOP cohorts for every condition in conditionSpecsCsvPath and
# every active CDM database in config.yaml.
# To run only what changed since the last run, use runIthacaPipeline() (runIthacaPipeline.R).
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("buildRandomOmopCohortMultipleDatabases.R")

library(Ithaca)

config <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")
databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = .GlobalEnv)

# ithReadIthacaWorkflowConfig() reads conditionSpecsCsvPath and replaces the
# inline condition list with the CSV rows. The config wrapper then loops over
# every condition and every database from config$cdm.
stopifnot(length(databases) > 0L)
stopifnot(length(config$conditionSpecs) > 0L)

results <- buildRandomOmopCohortFromConfig(
  configPath = "config.yaml"
)

# Inspect the result by condition and database.
str(results, max.level = 2)
