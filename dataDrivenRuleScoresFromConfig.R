# Set each rule's score from the data for every condition in config.yaml, and apply the new scores
# to the concise rubric when the gate passes. Run it after the concept sets are built
# (createRubricConceptSetsFromConfig.R) and before scoring. Settings are in the dataDrivenScoring
# section of config.yaml; each condition's results are saved under
# <outputDirectory>/rubricEvidenceProbe/.
#
# Use applyChanges = FALSE to look first (nothing is applied), then
# applyDataDrivenRuleScoresFromConfig("config.yaml") to apply.
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("dataDrivenRuleScoresFromConfig.R")

library(Ithaca)

results <- applyDataDrivenRuleScoresFromConfig("config.yaml")
print(results)
