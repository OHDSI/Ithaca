WITH input(personId, startingDate) AS (
  VALUES
@allValues
)
select personId, concept_id, concept_name, day_in_study, type
from (
select distinct personId, concept_id, concept_name, day_in_study, type
from (
select i.personId, c.concept_id, concept_name, "2cInclude" as type, DATEDIFF(con.condition_start_date, i.startingDate) day_in_study
from @cdm_schema.condition_occurrence con
join @cdm_schema.concept c
	on c.concept_id = con.condition_concept_id
join input i
  on con.person_id = i.personId
where con.condition_start_date >= DATEADD(day, -60, i.startingDate)
	and con.condition_start_date <= DATEADD(day, @endDays, i.startingDate)
union all
select i.personId, c.concept_id, concept_name, "2cExclude" as type, DATEDIFF(con.condition_start_date, i.startingDate) day_in_study
from @cdm_schema.condition_occurrence con
join @cdm_schema.concept c
	on c.concept_id = con.condition_concept_id
join input i
  on con.person_id = i.personId
where con.condition_start_date >= DATEADD(day, -60, i.startingDate)
	and con.condition_start_date <= DATEADD(day, @endDays, i.startingDate)
	{@excludedConditionCode != ""} ? {
	  and con.condition_concept_id not in (
  		select ca.descendant_concept_id
  		from @cdm_schema.concept_ancestor ca
  		where ca.ancestor_concept_id in (@excludedConditionCode)
  	)}
union all
select i.personId, c.concept_id, concept_name, "4d" as type, DATEDIFF(d.drug_exposure_start_date, i.startingDate) day_in_study
from @cdm_schema.drug_exposure d
join @cdm_schema.concept c
	on c.concept_id = d.drug_concept_id
join input i
  on d.person_id = i.personId
where d.drug_exposure_start_date >= DATEADD(day, -60, i.startingDate)
	and d.drug_exposure_start_date <= DATEADD(day, @endDays, i.startingDate)
union all

select i.personId, c.concept_id, concept_name, "6o" as type, DATEDIFF(o.observation_date, i.startingDate) day_in_study
from @cdm_schema.observation o
join @cdm_schema.concept c
	on c.concept_id = o.observation_concept_id
join input i
  on o.person_id = i.personId
where o.observation_date >= DATEADD(day, -60, i.startingDate)
	and o.observation_date <= DATEADD(day, @endDays, i.startingDate)
union all

select i.personId, c.concept_id, concept_name, "5p" as type, DATEDIFF(p.procedure_date, i.startingDate) day_in_study
from @cdm_schema.procedure_occurrence p
join @cdm_schema.concept c
	on c.concept_id = p.procedure_concept_id
join input i
  on p.person_id = i.personId
where p.procedure_date >= DATEADD(day, -60, i.startingDate)
	and p.procedure_date <= DATEADD(day, @endDays, i.startingDate)

union all
select i.personId, c.concept_id, concept_name, "3m" as type, DATEDIFF(m.measurement_date, i.startingDate) day_in_study
from @cdm_schema.measurement m
join @cdm_schema.concept c
	on c.concept_id = m.measurement_concept_id
join input i
  on m.person_id = i.personId
where m.measurement_date >= DATEADD(day, -60, i.startingDate)
	and m.measurement_date <= DATEADD(day, @endDays, i.startingDate)
union all
select i.personId, c.concept_id, concat("Visit type: ", c.concept_name, "; Provider type: ", c2.concept_name,
  "; Visit length: ", cast((DATEDIFF(v.visit_end_date, v.visit_start_date) + 1) as string), " days") as concept_name,
  "1v" as type, DATEDIFF(v.visit_start_date, i.startingDate) day_in_study
from @cdm_schema.visit_occurrence v
join @cdm_schema.concept c
	on c.concept_id = v.visit_concept_id
join input i
  on v.person_id = i.personId
join @cdm_schema.provider p
	on p.provider_id = v.provider_id
join @cdm_schema.concept c2
	on c2.concept_id = p.specialty_concept_id
where v.visit_start_date >= DATEADD(day, -60, i.startingDate)
	and v.visit_start_date <= DATEADD(day, @endDays, i.startingDate)
)
where concept_name not in ("Unlisted evaluation and management service",
"No matching concept",
"Body mass index (BMI) [Ratio]",
"Body height",
"Body weight",
"Body surface area Derived from formula",
"Body temperature",
"Systolic blood pressure",
"Diastolic blood pressure",
"Respiratory rate")
order by day_in_study, type
);
