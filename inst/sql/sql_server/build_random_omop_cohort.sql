DELETE FROM @cohort_database_schema.@cohort_table_name
WHERE cohort_definition_id = @cohort_definition_id;

INSERT INTO @cohort_database_schema.@cohort_table_name (
  cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
)
SELECT
  @cohort_definition_id AS cohort_definition_id,
  person_id AS subject_id,
  cohort_start_date,
  cohort_start_date AS cohort_end_date
FROM (
  SELECT
    person_id,
    cohort_start_date,
    ROW_NUMBER() OVER (ORDER BY NEWID()) AS rn_global
  FROM (
    SELECT
      person_id,
      cohort_start_date
    FROM (
      SELECT
        vo.person_id,
        CAST(vo.visit_start_date AS DATE) AS cohort_start_date,
        ROW_NUMBER() OVER (
          PARTITION BY vo.person_id
          ORDER BY NEWID()
        ) AS rn_person
      FROM @cdm_database_schema.visit_occurrence vo
      INNER JOIN @cdm_database_schema.person p
        ON vo.person_id = p.person_id
      WHERE vo.visit_start_date IS NOT NULL
        AND CAST(vo.visit_start_date AS DATE) >= CAST('@min_visit_start_date' AS DATE)
        AND CAST(vo.visit_start_date AS DATE) <= CAST('@max_visit_end_date' AS DATE)
        AND YEAR(CAST(vo.visit_start_date AS DATE)) - p.year_of_birth >= @min_age
        AND YEAR(CAST(vo.visit_start_date AS DATE)) - p.year_of_birth <= @max_age
        AND (
          @random_sample_from_cohort_definition_id = -1
          OR EXISTS (
            SELECT 1
            FROM @cohort_database_schema.@cohort_table_name sample_pool
            WHERE sample_pool.cohort_definition_id = @random_sample_from_cohort_definition_id
              AND sample_pool.subject_id = vo.person_id
              AND CAST(sample_pool.cohort_start_date AS DATE) = CAST(vo.visit_start_date AS DATE)
          )
        )
    ) random_visit_per_person
    WHERE rn_person = 1
  ) one_visit_per_person
) sampled_subjects
WHERE rn_global <= @sample_size;

MERGE INTO @cohort_database_schema.@cohort_table_name AS target
USING (
  SELECT
    subject_id,
    cohort_start_date,
    cohort_end_date
  FROM (
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
  ) source_cohort_ranked
  WHERE rn = 1
) AS source
ON target.cohort_definition_id = @cohort_definition_id
AND target.subject_id = source.subject_id
WHEN MATCHED THEN UPDATE SET
  cohort_start_date = source.cohort_start_date,
  cohort_end_date = source.cohort_end_date;
