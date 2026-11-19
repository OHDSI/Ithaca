# Score subjects for every condition in conditionSpecsCsvPath and every active
# CDM database in config.yaml.
# To run only what changed since the last run, use runIthacaPipeline() (runIthacaPipeline.R).
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("scoreSubjectsMultipleDatabases.R")

library(Ithaca)

config <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")
databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = .GlobalEnv)

if (length(databases) == 0L) {
  stop("config.yaml must define at least one database under general.cdm")
}

if (is.null(config$conditionSpecs) || length(config$conditionSpecs) == 0L) {
  stop("No condition specifications were loaded from conditionSpecsCsvPath")
}

# conceptLibraryVersion is optional: null/"latest" uses the newest concept library
# version; an integer pins an earlier snapshot.

# This wrapper reads conditionSpecsCsvPath and scores every condition against
# every database resolved from config$cdm.
results <- scoreSubjectsAgainstRandomCohortFromConfig(
  configPath = "config.yaml"
)

# Results are named condition/database entries, for example:
# results[["Condition Name::OptumDod"]]
str(results, max.level = 2)
