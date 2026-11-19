WITH subjectData AS (
  SELECT subject_id, cohort_start_date
  FROM @cohortSchema.@cohortTable
  WHERE cohort_definition_id IN (@cohorts)
)
SELECT DISTINCT concept_id, concept_name
FROM (
SELECT c.concept_id, c.concept_name
FROM @cdm_schema.condition_occurrence con
JOIN @cdm_schema.concept c
  ON c.concept_id = con.condition_concept_id
JOIN subjectData i
  ON con.person_id = i.subject_id
WHERE con.condition_start_date >= DATEADD(day, -60, i.cohort_start_date)
  AND con.condition_start_date <= DATEADD(day, @endDays, i.cohort_start_date)
UNION ALL
SELECT c.concept_id, c.concept_name
FROM @cdm_schema.drug_exposure d
JOIN @cdm_schema.concept c
  ON c.concept_id = d.drug_concept_id
JOIN subjectData i
  ON d.person_id = i.subject_id
WHERE d.drug_exposure_start_date >= DATEADD(day, -60, i.cohort_start_date)
  AND d.drug_exposure_start_date <= DATEADD(day, @endDays, i.cohort_start_date)
UNION ALL
SELECT c.concept_id, c.concept_name
FROM @cdm_schema.procedure_occurrence p
JOIN @cdm_schema.concept c
  ON c.concept_id = p.procedure_concept_id
JOIN subjectData i
  ON p.person_id = i.subject_id
WHERE p.procedure_date >= DATEADD(day, -60, i.cohort_start_date)
  AND p.procedure_date <= DATEADD(day, @endDays, i.cohort_start_date)
UNION ALL
SELECT c.concept_id, c.concept_name
FROM @cdm_schema.measurement m
JOIN @cdm_schema.concept c
  ON c.concept_id = m.measurement_concept_id
JOIN subjectData i
  ON m.person_id = i.subject_id
WHERE m.measurement_date >= DATEADD(day, -60, i.cohort_start_date)
  AND m.measurement_date <= DATEADD(day, @endDays, i.cohort_start_date)
) all_items;
