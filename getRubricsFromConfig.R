# Generate rubrics for every condition in conditionSpecsCsvPath using settings from config.yaml.
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("getRubricsFromConfig.R")

if (file.exists("DESCRIPTION") && dir.exists("R")) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    stop("Run this script from the Ithaca checkout with pkgload or devtools installed.")
  }
} else {
  library(Ithaca)
}

cfg <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")

llmClient <- Ithaca:::ithGetLlmClientFromConfig(cfg, "reasoning")

getRubrics(
  conditionList = cfg$conditionSpecs,
  outputDirectory = as.character(cfg$outputDirectory[[1]]),
  rubricTemplates = cfg$rubricTemplates,
  includeLabs = isTRUE(cfg$includeLabs),
  promptDir = cfg$promptDir,
  llmClient = llmClient,
  useLibraryHints = isTRUE(cfg$useLibraryHints),
  libraryHintSimilarityThreshold = as.numeric(Ithaca:::ithCoalesce(cfg$libraryHintSimilarityThreshold, 0.94)[[1]]),
  libraryHintTopCandidates = as.integer(Ithaca:::ithCoalesce(cfg$libraryHintTopCandidates, 5)[[1]]),
  useExistingRubric = isTRUE(cfg$useExistingRubric),
  verbose = isTRUE(Ithaca:::ithCoalesce(cfg$verbose, TRUE))
)
