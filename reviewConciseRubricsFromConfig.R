# Critically review the latest concise rubric for every condition in config.yaml.
# Each condition's findings are saved as conciseRubricReview_V*.csv in its condition directory.
# Mark accepted findings in the Accept column (and optionally amend them in newCriteria).
#
# Example:
# source("reviewConciseRubricsFromConfig.R")

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

reviewResults <- reviewConciseRubricsFromConfig()
print(reviewResults)
