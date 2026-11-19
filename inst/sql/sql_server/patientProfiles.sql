SELECT concept_name, day_in_study
FROM (
SELECT DISTINCT concept_name, day_in_study, type
FROM (
SELECT c.concept_name, '2c' AS type, DATEDIFF(day, @startDate, con.condition_start_date) AS day_in_study
FROM @cdm_schema.condition_occurrence con
JOIN @cdm_schema.concept c
  ON c.concept_id = con.condition_concept_id
WHERE con.person_id IN (@subjectId)
  AND con.condition_start_date >= DATEADD(day, -60, @startDate)
  AND con.condition_start_date <= DATEADD(day, 60, @startDate)
  AND con.condition_concept_id NOT IN (
    SELECT ca.descendant_concept_id
    FROM @cdm_schema.concept_ancestor ca
    WHERE ca.ancestor_concept_id = @excludedConditionCode
  )
UNION ALL
SELECT c.concept_name, '4d' AS type, DATEDIFF(day, @startDate, d.drug_exposure_start_date) AS day_in_study
FROM @cdm_schema.drug_exposure d
JOIN @cdm_schema.concept c
  ON c.concept_id = d.drug_concept_id
WHERE d.person_id IN (@subjectId)
  AND d.drug_exposure_start_date >= DATEADD(day, -60, @startDate)
  AND d.drug_exposure_start_date <= DATEADD(day, 60, @startDate)
UNION ALL
SELECT c.concept_name, '5p' AS type, DATEDIFF(day, @startDate, p.procedure_date) AS day_in_study
FROM @cdm_schema.procedure_occurrence p
JOIN @cdm_schema.concept c
  ON c.concept_id = p.procedure_concept_id
WHERE p.person_id IN (@subjectId)
  AND p.procedure_date >= DATEADD(day, -60, @startDate)
  AND p.procedure_date <= DATEADD(day, 60, @startDate)
UNION ALL
SELECT c.concept_name, '3m' AS type, DATEDIFF(day, @startDate, m.measurement_date) AS day_in_study
FROM @cdm_schema.measurement m
JOIN @cdm_schema.concept c
  ON c.concept_id = m.measurement_concept_id
WHERE m.person_id IN (@subjectId)
  AND m.measurement_date >= DATEADD(day, -60, @startDate)
  AND m.measurement_date <= DATEADD(day, 60, @startDate)
UNION ALL
SELECT CONCAT('Visit type: ', c.concept_name, '; Provider type: ', c2.concept_name,
  '; Visit length: ', CAST(DATEDIFF(day, v.visit_start_date, v.visit_end_date) + 1 AS VARCHAR(20)), ' days') AS concept_name,
  '1v' AS type, DATEDIFF(day, @startDate, v.visit_start_date) AS day_in_study
FROM @cdm_schema.visit_occurrence v
JOIN @cdm_schema.concept c
  ON c.concept_id = v.visit_concept_id
JOIN @cdm_schema.provider p
  ON p.provider_id = v.provider_id
JOIN @cdm_schema.concept c2
  ON c2.concept_id = p.specialty_concept_id
WHERE v.person_id IN (@subjectId)
  AND v.visit_start_date >= DATEADD(day, -60, @startDate)
  AND v.visit_start_date <= DATEADD(day, 60, @startDate)
) all_items
WHERE concept_name NOT IN ('Unlisted evaluation and management service',
'No matching concept',
'Body mass index (BMI) [Ratio]',
'Body height',
'Body weight',
'Body surface area Derived from formula',
'Body temperature',
'Systolic blood pressure',
'Diastolic blood pressure',
'Respiratory rate')
) profile_items
ORDER BY day_in_study, type;
