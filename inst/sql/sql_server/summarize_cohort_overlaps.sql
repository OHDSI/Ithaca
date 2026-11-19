WITH source_ranked AS (
  SELECT
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date,
    ROW_NUMBER() OVER (
      PARTITION BY cohort_definition_id, subject_id
      ORDER BY cohort_start_date, cohort_end_date
    ) AS rn
  FROM @cohort_database_schema.@cohort_table_name
  WHERE cohort_definition_id IN (@cohort_ids_sql)
),
source_first AS (
  SELECT
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date
  FROM source_ranked
  WHERE rn = 1
),
random_rows AS (
  SELECT
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date
  FROM @cohort_database_schema.@cohort_table_name
  WHERE cohort_definition_id IN (@random_cohort_ids_sql)
)
SELECT
  source_first.cohort_definition_id AS source_cohort_definition_id,
  random_rows.cohort_definition_id AS random_cohort_definition_id,
  COUNT(*) AS overlap_subject_rows,
  SUM(CASE
        WHEN random_rows.cohort_start_date = source_first.cohort_start_date
         AND random_rows.cohort_end_date = source_first.cohort_end_date
        THEN 1
        ELSE 0
      END) AS rows_aligned_to_source_dates,
  SUM(CASE
        WHEN random_rows.cohort_start_date <> source_first.cohort_start_date
          OR random_rows.cohort_end_date <> source_first.cohort_end_date
        THEN 1
        ELSE 0
      END) AS rows_not_aligned_to_source_dates
FROM random_rows
INNER JOIN source_first
  ON random_rows.subject_id = source_first.subject_id
GROUP BY
  source_first.cohort_definition_id,
  random_rows.cohort_definition_id;
