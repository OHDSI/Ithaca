select concept_name, day_in_study
from (
select  distinct concept_name, day_in_study, type
from (
select concept_name, "2c" as type, DATEDIFF(con.condition_start_date, @startDate) day_in_study
from @cdm_schema.condition_occurrence con
join @cdm_schema.concept c
	on c.concept_id = con.condition_concept_id
where con.person_id in (@subjectId)
	and con.condition_start_date >= DATEADD(day, -60, @startDate)
	and con.condition_start_date <= DATEADD(day, 60, @startDate)
  	and con.condition_concept_id not in (
		select ca.descendant_concept_id
		from @cdm_schema.concept_ancestor ca
		where ca.ancestor_concept_id = @excludedConditionCode
	)
union all
select concept_name, "4d" as type, DATEDIFF(d.drug_exposure_start_date, @startDate) day_in_study
from @cdm_schema.drug_exposure d
join @cdm_schema.concept c
	on c.concept_id = d.drug_concept_id
where d.person_id in (@subjectId)
	and d.drug_exposure_start_date >= DATEADD(day, -60, @startDate)
	and d.drug_exposure_start_date <= DATEADD(day, 60, @startDate)
union all
select concept_name, "5p" as type, DATEDIFF(p.procedure_date, @startDate) day_in_study
from @cdm_schema.procedure_occurrence p
join @cdm_schema.concept c
	on c.concept_id = p.procedure_concept_id
where p.person_id in (@subjectId)
	and p.procedure_date >= DATEADD(day, -60, @startDate)
	and p.procedure_date <= DATEADD(day, 60, @startDate)
union all
select concept_name, "3m" as type, DATEDIFF(m.measurement_date, @startDate) day_in_study
from @cdm_schema.measurement m
join @cdm_schema.concept c
	on c.concept_id = m.measurement_concept_id
where m.person_id in (@subjectId)
	and m.measurement_date >= DATEADD(day, -60, @startDate)
	and m.measurement_date <= DATEADD(day, 60, @startDate)
union all
select concat("Visit type: ", c.concept_name, "; Provider type: ", c2.concept_name,
  "; Visit length: ", cast((DATEDIFF(v.visit_end_date, v.visit_start_date) + 1) as string), " days") as concept_name,
  "1v" as type, DATEDIFF(v.visit_start_date, @startDate) day_in_study
from @cdm_schema.visit_occurrence v
join @cdm_schema.concept c
	on c.concept_id = v.visit_concept_id
join @cdm_schema.provider p
	on p.provider_id = v.provider_id
join @cdm_schema.concept c2
	on c2.concept_id = p.specialty_concept_id
where v.person_id in (@subjectId)
	and v.visit_start_date >= DATEADD(day, -60, @startDate)
	and v.visit_start_date <= DATEADD(day, 60, @startDate)
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
