WITH input AS (
  SELECT personId, CAST(startingDate AS DATE) AS startingDate
  FROM (VALUES
@allValues
  ) AS v(personId, startingDate)
)
SELECT personId, concept_id, concept_name, day_in_study, domain_id, type
FROM (
SELECT DISTINCT personId, concept_id, concept_name, day_in_study, domain_id, type
FROM (
  SELECT i.personId, c.concept_id, c.concept_name, '2cInclude' AS type, DATEDIFF(day, i.startingDate, con.condition_start_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.condition_occurrence con
  JOIN @cdm_schema.concept c
    ON c.concept_id = con.condition_concept_id
  JOIN input i
    ON con.person_id = i.personId
  WHERE con.condition_start_date >= DATEADD(day, -60, i.startingDate)
    AND con.condition_start_date <= DATEADD(day, @endDays, i.startingDate)
  UNION ALL
  SELECT i.personId, c.concept_id, c.concept_name, '2cExclude' AS type, DATEDIFF(day, i.startingDate, con.condition_start_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.condition_occurrence con
  JOIN @cdm_schema.concept c
    ON c.concept_id = con.condition_concept_id
  JOIN input i
    ON con.person_id = i.personId
  WHERE con.condition_start_date >= DATEADD(day, -60, i.startingDate)
    AND con.condition_start_date <= DATEADD(day, @endDays, i.startingDate)
    {@excludedConditionCode != ""} ? {
    AND con.condition_concept_id NOT IN (
      SELECT ca.descendant_concept_id
      FROM @cdm_schema.concept_ancestor ca
      WHERE ca.ancestor_concept_id IN (@excludedConditionCode)
    )}
  UNION ALL
  SELECT i.personId, c.concept_id, c.concept_name, '4d' AS type, DATEDIFF(day, i.startingDate, d.drug_exposure_start_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.drug_exposure d
  JOIN @cdm_schema.concept c
    ON c.concept_id = d.drug_concept_id
  JOIN input i
    ON d.person_id = i.personId
  WHERE d.drug_exposure_start_date >= DATEADD(day, -60, i.startingDate)
    AND d.drug_exposure_start_date <= DATEADD(day, @endDays, i.startingDate)
  UNION ALL
  SELECT i.personId, c.concept_id, c.concept_name, '6o' AS type, DATEDIFF(day, i.startingDate, o.observation_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.observation o
  JOIN @cdm_schema.concept c
    ON c.concept_id = o.observation_concept_id
  JOIN input i
    ON o.person_id = i.personId
  WHERE o.observation_date >= DATEADD(day, -60, i.startingDate)
    AND o.observation_date <= DATEADD(day, @endDays, i.startingDate)
  UNION ALL
  SELECT i.personId, c.concept_id, c.concept_name, '5p' AS type, DATEDIFF(day, i.startingDate, p.procedure_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.procedure_occurrence p
  JOIN @cdm_schema.concept c
    ON c.concept_id = p.procedure_concept_id
  JOIN input i
    ON p.person_id = i.personId
  WHERE p.procedure_date >= DATEADD(day, -60, i.startingDate)
    AND p.procedure_date <= DATEADD(day, @endDays, i.startingDate)
  UNION ALL
  SELECT i.personId, c.concept_id, c.concept_name, '3m' AS type, DATEDIFF(day, i.startingDate, m.measurement_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.measurement m
  JOIN @cdm_schema.concept c
    ON c.concept_id = m.measurement_concept_id
  JOIN input i
    ON m.person_id = i.personId
  WHERE m.measurement_date >= DATEADD(day, -60, i.startingDate)
    AND m.measurement_date <= DATEADD(day, @endDays, i.startingDate)
  UNION ALL
  SELECT i.personId, c.concept_id,
    CONCAT('Visit type: ', c.concept_name, '; Provider type: ', c2.concept_name,
      '; Visit length: ', CAST(DATEDIFF(day, v.visit_start_date, v.visit_end_date) + 1 AS VARCHAR(20)), ' days') AS concept_name,
    '1v' AS type, DATEDIFF(day, i.startingDate, v.visit_start_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.visit_occurrence v
  JOIN @cdm_schema.concept c
    ON c.concept_id = v.visit_concept_id
  JOIN input i
    ON v.person_id = i.personId
  JOIN @cdm_schema.provider p
    ON p.provider_id = v.provider_id
  JOIN @cdm_schema.concept c2
    ON c2.concept_id = p.specialty_concept_id
  WHERE v.visit_start_date >= DATEADD(day, -60, i.startingDate)
    AND v.visit_start_date <= DATEADD(day, @endDays, i.startingDate)
    AND v.visit_concept_id <> 581458 --skip pharmacy visits
    AND p.specialty_concept_id <> 581458 --skip pharmacy visits
  UNION ALL --add in the provider specialty concept id - will produce the same string as the visit so the 2 concepts will end up in the same concept set
  SELECT i.personId, c2.concept_id,
    CONCAT('Visit type: ', c.concept_name, '; Provider type: ', c2.concept_name,
      '; Visit length: ', CAST(DATEDIFF(day, v.visit_start_date, v.visit_end_date) + 1 AS VARCHAR(20)), ' days') AS concept_name,
    '1v' AS type, DATEDIFF(day, i.startingDate, v.visit_start_date) AS day_in_study,
    c.domain_id
  FROM @cdm_schema.visit_occurrence v
  JOIN @cdm_schema.concept c
    ON c.concept_id = v.visit_concept_id
  JOIN input i
    ON v.person_id = i.personId
  JOIN @cdm_schema.provider p
    ON p.provider_id = v.provider_id
  JOIN @cdm_schema.concept c2
    ON c2.concept_id = p.specialty_concept_id
  WHERE v.visit_start_date >= DATEADD(day, -60, i.startingDate)
    AND v.visit_start_date <= DATEADD(day, @endDays, i.startingDate)
    AND v.visit_concept_id <> 581458 --skip pharmacy visits
    AND p.specialty_concept_id <> 581458 --skip pharmacy visits
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
