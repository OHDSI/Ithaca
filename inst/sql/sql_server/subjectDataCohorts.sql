WITH subjectData AS (
  select subject_id, cohort_start_date
  from @cohortSchema.@cohortTable
  where cohort_definition_id in (@cohorts)
)
select distinct concept_id, concept_name
from (
select c.concept_id, concept_name
from @cdm_schema.condition_occurrence con
join @cdm_schema.concept c
	on c.concept_id = con.condition_concept_id
join subjectData i
  on con.person_id = i.subject_id
where con.condition_start_date >= DATEADD(day, -60, i.cohort_start_date)
	and con.condition_start_date <= DATEADD(day, @endDays, i.cohort_start_date)
union all
select c.concept_id, concept_name
from @cdm_schema.drug_exposure d
join @cdm_schema.concept c
	on c.concept_id = d.drug_concept_id
join subjectData i
  on d.person_id = i.subject_id
where d.drug_exposure_start_date >= DATEADD(day, -60, i.cohort_start_date)
	and d.drug_exposure_start_date <= DATEADD(day, @endDays, i.cohort_start_date)
union all
select c.concept_id, concept_name
from @cdm_schema.procedure_occurrence p
join @cdm_schema.concept c
	on c.concept_id = p.procedure_concept_id
join subjectData i
  on p.person_id = i.subject_id
where p.procedure_date >= DATEADD(day, -60, i.cohort_start_date)
	and p.procedure_date <= DATEADD(day, @endDays, i.cohort_start_date)
union all
select c.concept_id, concept_name
from @cdm_schema.measurement m
join @cdm_schema.concept c
	on c.concept_id = m.measurement_concept_id
join subjectData i
  on m.person_id = i.subject_id
where m.measurement_date >= DATEADD(day, -60, i.cohort_start_date)
	and m.measurement_date <= DATEADD(day, @endDays, i.cohort_start_date)
);
