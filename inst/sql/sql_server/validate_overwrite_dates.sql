WITH source_cohort_ranked AS (
  SELECT
    subject_id,
    cohort_start_date,
    cohort_end_date,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id
      ORDER BY cohort_start_date, cohort_end_date
    ) AS rn
  FROM @cohort_database_schema.@cohort_table_name
  WHERE cohort_definition_id = @source_cohort_definition_id
),
source_subjects AS (
  SELECT
    subject_id,
    cohort_start_date,
    cohort_end_date
  FROM source_cohort_ranked
  WHERE rn = 1
),
random_cohort AS (
  SELECT
    subject_id,
    cohort_start_date,
    cohort_end_date
  FROM @cohort_database_schema.@cohort_table_name
  WHERE cohort_definition_id = @cohort_definition_id
)
SELECT
  COUNT(*) AS overlap_subject_rows,
  SUM(CASE
        WHEN random_cohort.cohort_start_date = source_subjects.cohort_start_date
         AND random_cohort.cohort_end_date = source_subjects.cohort_end_date
        THEN 1
        ELSE 0
      END) AS rows_aligned_to_source_dates,
  SUM(CASE
        WHEN random_cohort.cohort_start_date <> source_subjects.cohort_start_date
          OR random_cohort.cohort_end_date <> source_subjects.cohort_end_date
        THEN 1
        ELSE 0
      END) AS rows_not_aligned_to_source_dates
FROM random_cohort
INNER JOIN source_subjects
  ON random_cohort.subject_id = source_subjects.subject_id;
