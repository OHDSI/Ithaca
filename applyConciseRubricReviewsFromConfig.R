# Apply the accepted findings (Accept column) from the latest conciseRubricReview_V*.csv for
# every condition in config.yaml. Changes are all-or-nothing per condition: if any accepted row
# has a problem, nothing is changed and an 'issues' column is written into that review file.
# Fix the issues, then rerun this script.
#
# Example:
# source("applyConciseRubricReviewsFromConfig.R")

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

applyReviewResults <- applyConciseRubricReviewsFromConfig()
print(applyReviewResults)
