# Run the evaluation pipeline: build the random evaluation cohort, score it, and run the
# phenotype reports, for every condition and database in config.yaml.
# Only units whose inputs changed since their last successful run are executed (new database,
# CDM version, condition, analysis row, cohort definition, rubric, concept sets, or code).
# Run rubric and concept-set creation first; the pipeline picks up their newest outputs.
# The ledger is <outputDirectory>/pipeline/pipelineLedger.csv.
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("runIthacaPipeline.R")

library(Ithaca)

# First time on an output folder that already has results: record them instead of rerunning.
# runIthacaPipeline(configPath = "config.yaml", adoptExisting = TRUE)

# Show what would run, and why, without running anything.
plan <- runIthacaPipeline(configPath = "config.yaml", dryRun = TRUE)

# Run what changed. Use fresh = TRUE (or fresh = "report", etc.) to rerun regardless.
results <- runIthacaPipeline(configPath = "config.yaml")

print(table(results$step, results$status))
