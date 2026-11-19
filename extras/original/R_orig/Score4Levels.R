library(dplyr)
library(tidyr)

library(data.table)

#--------------------------------------------------
# 1) Derive rubric-level assessment from score columns
#--------------------------------------------------
derive_rubric_assessment <- function(total, yesScore, noScore) {
  data.table::fcase(
    is.na(total) | is.na(yesScore) | is.na(noScore), "insufficient",
    total >= yesScore, "case",
    total <= noScore, "non_case",
    default = "insufficient"
  )
}

#--------------------------------------------------
# 2) Subject-level 4-level classifier
#    Returns a named list so it fits naturally in data.table
#--------------------------------------------------
classify_subject_4level <- function(
    n_case,
    n_non_case,
    n_insufficient,
    n_total,
    density_case_threshold = 0.50,
    density_non_case_threshold = 0.50,
    positive_rate_case_threshold = 0.75,
    positive_rate_possible_threshold = 0.75,
    positive_rate_non_case_threshold = 0.25,
    sparse_density_threshold = 0.30,
    min_case_n = 2,
    min_non_case_n = 2,
    min_net_case = 2,
    max_net_non_case = -2
) {
  n_observed <- n_case + n_non_case
  evidence_density <- ifelse(n_total > 0, n_observed / n_total, NA_real_)
  positive_rate <- ifelse(n_observed > 0, n_case / n_observed, NA_real_)
  net_score <- n_case - n_non_case

  subject_assessment <- data.table::fcase(
    n_case >= min_case_n & #must be a case in at least 2 rubrics
      evidence_density >= density_case_threshold & #must have data for at least 4 rubrics
      !is.na(positive_rate) &
      positive_rate >= positive_rate_case_threshold & #must have one of the following: 2 case/2 total assessed, 3/3, 4/4, 4/5, 5/5, 5/6, 6/6, 6/7, 7/7
      net_score >= min_net_case, #must have at least 2 cases more than non cases
    "high_confidence_case",

    n_case >= 1 & #must have at least one case
      evidence_density < density_case_threshold &
      !is.na(positive_rate) &
      positive_rate >= positive_rate_possible_threshold, "possible_case", #same as for case but allows for 1/1

    n_non_case >= min_non_case_n & #must have at least 2 non-case assessments
      evidence_density >= density_non_case_threshold & #must have data for at least 4 rubrics
      !is.na(positive_rate) &
      positive_rate <= positive_rate_non_case_threshold & #can only be positive in 1/4,5,6,7 total assessed
      net_score <= max_net_non_case, #must have at least 2 more non cases than cases
    "likely_non_case",

    default = "indeterminate"
  )

  list(
    n_observed = n_observed,
    evidence_density = evidence_density,
    positive_rate = positive_rate,
    net_score = net_score,
    subject_assessment = subject_assessment
  )
}

#--------------------------------------------------
# 3) Main scoring function for a data.table
#--------------------------------------------------
score_subjects_from_rubric_table <- function(
    df,
    subject_col = "subjectId",
    rubric_col = "rubricId",
    cohort_col = "cohortDefinitionId",
    start_date_col = "cohortStartDate",
    total_col = "total",
    yes_col = "yesScore",
    no_col = "noScore"
) {
  data.table::setDT(df)

  needed_cols <- c(subject_col, rubric_col, cohort_col, start_date_col, total_col, yes_col, no_col)
  missing_cols <- setdiff(needed_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check duplicates
  dupes <- df[
    ,
    .N,
    by = c(cohort_col, subject_col, rubric_col, start_date_col)
  ][N > 1]

  if (nrow(dupes) > 0) {
    stop("Duplicate rows found for cohortDefinitionId x subjectId x rubricId x cohortStartDate")
  }

  # Rubric-level
  rubric_level <- data.table::copy(df)

  rubric_level[
    ,
    rubric_assessment := derive_rubric_assessment(
      total = get(total_col),
      yesScore = get(yes_col),
      noScore = get(no_col)
    )
  ]

  rubric_level[
    ,
    rubric_case_numeric := data.table::fcase(
      rubric_assessment == "case", 1L,
      rubric_assessment == "non_case", -1L,
      default = 0L
    )
  ]

  # Subject-level summary
  subject_level <- rubric_level[
    ,
    .(
      n_case = sum(rubric_assessment == "case", na.rm = TRUE),
      n_non_case = sum(rubric_assessment == "non_case", na.rm = TRUE),
      n_insufficient = sum(rubric_assessment == "insufficient", na.rm = TRUE),
      n_total = .N
    ),
    by = c(cohort_col, subject_col, start_date_col)
  ]

  # Apply classifier rowwise
  subject_level[
    ,
    c("n_observed", "evidence_density", "positive_rate", "net_score", "subject_assessment") :=
      classify_subject_4level(
        n_case = n_case,
        n_non_case = n_non_case,
        n_insufficient = n_insufficient,
        n_total = n_total
      )
  ]

  list(
    rubric_level = rubric_level,
    subject_level = subject_level
  )
}
# sumCohortScores <- readRDS("p:/llm/phenotypes/Spontaneous Abortion/Optum DOD/sumCohortScores_Cohort25108_V1.rds")
result <- score_subjects_from_rubric_table(sumCohortScores)

result$subject_level %>% count(subject_assessment, sort = TRUE)

# caseResults <- result$subject_level[result$subject_level$subject_assessment == "high_confidence_case",]
# caseResults <- result$subject_level[result$subject_level$subject_assessment != "likely_non_case",]
# caseResults <- result$subject_level[result$subject_level$subject_assessment != "indeterminate",]
# # caseResults <- caseResults[caseResults$n_case >= 4,]
# caseResults <- merge(caseResults, allSubjects, all.x = T)
# caseResults <- caseResults[,c("cohortDefinitionId", "subjectId", "cohortStartDate", "cohortEndDate")]
# caseResults$cohortDefinitionId <- 999997
# caseResults$subjectId <- bit64::as.integer64(caseResults$subjectId)
#
# insertTable(
#   connection = DatabaseConnector::connect(DOD_OHDA_DBSpec$connectionDetails),
#   databaseSchema = "scratch.scratch_all",
#   tableName = "rubric_cohort_optum_extended_dod_v3787",
#   data = caseResults,
#   dropTableIfExists = F,
#   createTable = F,
#   tempTable = FALSE,
#   oracleTempSchema = NULL,
#   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
#   bulkLoad = T,
#   progressBar = T,
#   camelCaseToSnakeCase = T
# )
#
# llmResults <- llmRubricScoring[,c(2,4,5, 6)]
# subjectLevel <- result$subject_level
# allResults <- merge(subjectLevel, llmResults, by.x="subjectId", by.y="subjectId")
#
# table(aiAfi$isCase)
#
# write.csv(allResults, "P:/llm/rubrics/acute myocardial infarction/amiLlmRubricV1.csv", row.names = F)
#
# aiYes_RubNo <- aiAfi[aiAfi$isCase == "yes" &
#                        aiAfi$subjectId %in% c(allResults$subjectId[allResults$subject_assessment == "likely_non_case"]),]
#
# aiNo_RubYes <- aiAfi[aiAfi$isCase == "no" &
#                        aiAfi$subjectId %in% c(allResults$subjectId[allResults$subject_assessment == "high_confidence_case" |
#                                                                      allResults$subject_assessment == "possible_case"]),]
#
# aiNo_RubInd <- aiAfi[(aiAfi$isCase == "no" | aiAfi$isCase == "yes") &
#                        aiAfi$subjectId %in% c(allResults$subjectId[allResults$subject_assessment == "indeterminate"]),]
#
# # test <- aiNo_RubInd
# # test <- aiNo_RubYes
# test <- aiYes_RubNo
# subjectScoring <- NULL
# for(subjectUp in 1:nrow(test)) {
#   pointsScored <- paste(unlist((unique(newRubric$shortVersion[newRubric$rubricRule  %in%
#                                                                 c(newscoredFile$rubricRule[newscoredFile$subjectId == test$subjectId[[subjectUp]]])]))), collapse = "; ")
#   pointsMissed <- paste(unlist((unique(newRubric$shortVersion[grepl("/ER", newRubric$rubricRule, fixed = TRUE) & !(newRubric$rubricRule  %in%
#                                                                                                                      c(newscoredFile$rubricRule[newscoredFile$subjectId == test$subjectId[[subjectUp]]]))]))), collapse = "; ")
#
#   testing <- sumCohortScores$case[sumCohortScores$subjectId == test$subjectId[[subjectUp]]]
#
#   final <- paste0("Subject: ", test$subjectId[[subjectUp]], "\n--Scored: ", pointsScored, "\n\n--Missed: ", pointsMissed,
#                   "\n\n--Rubric scores: ", paste(testing, collapse = ", "), "\n\n\n\n")
#
#   subjectScoring <- paste(subjectScoring, final)
# }
#
# cat(subjectScoring, file = "P:/llm/rubrics/acute myocardial infarction/LlmYesNoRubricInd4LevelV1.txt")
# cat(subjectScoring, file = "P:/llm/rubrics/acute myocardial infarction/LlmNoRubricYes4LevelV1.txt")
# cat(subjectScoring, file = "P:/llm/rubrics/acute myocardial infarction/LlmYesRubricNo4LevelV1.txt")
#
# a <- sumCohortScoresAmi[sumCohortScoresAmi$subjectId == 33003284717,]


