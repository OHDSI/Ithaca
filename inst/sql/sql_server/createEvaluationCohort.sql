{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_database_table = 'cohort'
{DEFAULT @work_database_schema = 'CDM_SIM' }
{DEFAULT @caseCohortId = 0 }
{DEFAULT @caseFirstOccurrenceOnly}
{DEFAULT @tempDB = "scratch.dbo" }
{DEFAULT @test_cohort = "test_cohort" }
{DEFAULT @ageLimit = 0}
{DEFAULT @upperAgeLimit = 120}
{DEFAULT @gender = c(8507, 8532)}
{DEFAULT @race = 0}
{DEFAULT @ethnicity = 0}
{DEFAULT @startDate = '19000101' }
{DEFAULT @endDate = '21000101' }
{DEFAULT @baseSampleSize = 150000 }
{DEFAULT @inclusionEvaluationCohortId = 0 }
{DEFAULT @inclusionEvaluationDaysFromStart = 0 }
{DEFAULT @inclusionEvaluationDaysFromEnd = 0 }
{DEFAULT @duringInclusionEvaluationOnly}
{DEFAULT @exclusionEvaluationCohortId = 0 }
{DEFAULT @exclusionEvaluationDaysFromStart = 0 }
{DEFAULT @exclusionEvaluationDaysFromEnd = 0 }
{DEFAULT @visitType = c(9201) }
{DEFAULT @minimumOffsetFromStart = 365}
{DEFAULT @minimumOffsetFromEnd = 365}
{DEFAULT @randomVisitTable = ''}

DROP TABLE IF EXISTS #finalCohort;
DROP TABLE IF EXISTS #adjustedCaseCohort;
DROP TABLE IF EXISTS #persons;

--if using first occurrence only, subset the cohort to the first occurrence per subject
{@caseFirstOccurrenceOnly == TRUE} ?
{select cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
 into #adjustedCaseCohort
 from (
    select *, row_number() over (PARTITION BY subject_id order by cohort_start_date) rn
    from @cohort_database_schema.@cohort_database_table
    where cohort_definition_id = @caseCohortId) a
    where rn = 1;}

--else allow all occurrences to go through to main sql
{@caseFirstOccurrenceOnly == FALSE} ?
{select *
 into #adjustedCaseCohort
 from @cohort_database_schema.@cohort_database_table
 where cohort_definition_id = @caseCohortId;}

--subset a random set of subjects
  select *
  into #persons
  from (
    select person_id, row_number() over (order by NewId()) rn
    from @cdm_database_schema.person p
{@inclusionEvaluationCohortId != 0} ? { --subjects must be from inclusion evaluation cohort if specified
    join @cohort_database_schema.@cohort_database_table co
      on co.cohort_definition_id = @inclusionEvaluationCohortId
        and co.subject_id = p.person_id}) a
  where rn <= 20000000
  order by rn;

-- create cohort using prevalence cohort and visit table
with visits as ( --and a random visit from each subject selected
select distinct v.person_id, first_value(visit_start_date)
  over (partition by v.person_id ORDER BY NewId()) visit_start_date
{@randomVisitTable == ''} ? {FROM @cdm_database_schema.visit_occurrence v}
{@randomVisitTable != ''} ? {FROM @work_database_schema.@randomVisitTable v}
join @cdm_database_schema.observation_period o
  on v.person_id = o.person_id
    AND v.visit_start_date >= dateadd(d, @minimumOffsetFromStart, o.observation_period_start_date)
    and dateadd(d, @minimumOffsetFromEnd, v.visit_start_date) <= o.observation_period_end_date
{@inclusionEvaluationCohortId != 0} ? { --visits must be within designated period from inclusion evaluation cohort if specified
join @cohort_database_schema.@cohort_database_table co
  on co.cohort_definition_id = @inclusionEvaluationCohortId
    and co.subject_id = v.person_id
    and v.visit_start_date >= dateadd(day, @inclusionEvaluationDaysFromStart, co.cohort_start_date)

    {@duringInclusionEvaluationOnly == FALSE} ? {--no limit to the ending date designated by inclusionEvaluationDaysFromEnd
    and v.visit_start_date <= dateadd(day, @inclusionEvaluationDaysFromEnd, co.cohort_start_date)}

    {@duringInclusionEvaluationOnly == TRUE} ? {--limit ending date to min value of days offset and end of cohort
    and v.visit_start_date <=
      CASE
        WHEN dateadd(day, @inclusionEvaluationDaysFromEnd, co.cohort_start_date) > co.cohort_end_date THEN co.cohort_end_date
        ELSE dateadd(day, @inclusionEvaluationDaysFromEnd, co.cohort_start_date)
      END
      }

    }
where v.person_id in (
  select person_id
  from #persons)
  and v.visit_concept_id in (@visitType)
  and v.visit_start_date >= cast('@startDate' AS DATE)
	and v.visit_start_date <= cast('@endDate' AS DATE)

{@exclusionEvaluationCohortId != 0} ? { --visits must NOT be within designated period from exclusion evaluation cohort if specified
minus
select distinct v.person_id, visit_start_date
from @cdm_database_schema.visit_occurrence v
join @cohort_database_schema.@cohort_database_table coExcl
  on coExcl.cohort_definition_id = @exclusionEvaluationCohortId
    and coExcl.subject_id = v.person_id
    and v.visit_start_date >= dateadd(day, @exclusionEvaluationDaysFromStart, coExcl.cohort_start_date)
    and v.visit_start_date <= dateadd(day, @exclusionEvaluationDaysFromEnd, coExcl.cohort_start_date)
where v.person_id in (
  select person_id
  from #persons)
  }

	)
select *
into #finalCohort
from (
  select @caseCohortId as cohort_definition_id, person_id as subject_id, visit_start_date as cohort_start_date,
    dateadd(day, 1, visit_start_date) as cohort_end_date
  from visits
  where person_id not in ( --noncases from those not in case cohort
    select subject_id
    from @cohort_database_schema.@cohort_database_table
    where cohort_definition_id = @caseCohortId)
  union
  select c.* --cases from those also in case cohort - use adjusted case cohort, i.e., only using first occurrence if specified
  from visits v
  join #adjustedCaseCohort c
    on v.person_id = c.subject_id
{@inclusionEvaluationCohortId != 0} ? { --cases must be within designated period from inclusion evaluation cohort if specified
  join @cohort_database_schema.@cohort_database_table co
    on co.cohort_definition_id = @inclusionEvaluationCohortId
      and co.subject_id = c.subject_id
      and c.cohort_start_date >= dateadd(day, @inclusionEvaluationDaysFromStart, co.cohort_start_date)
    {@duringInclusionEvaluationOnly == FALSE} ? {--no limit to the ending date designated by inclusionEvaluationDaysFromEnd
    and c.cohort_start_date <= dateadd(day, @inclusionEvaluationDaysFromEnd, co.cohort_start_date)}
    {@duringInclusionEvaluationOnly == TRUE} ? {--limit ending date to min value of days offset and end of cohort
    and c.cohort_start_date <=
      CASE
        WHEN dateadd(day, @inclusionEvaluationDaysFromEnd, co.cohort_start_date) > co.cohort_end_date THEN co.cohort_end_date
        ELSE dateadd(day, @inclusionEvaluationDaysFromEnd, co.cohort_start_date)
      END }
      }
) a
;

DROP TABLE IF EXISTS @work_database_schema.@test_cohort;

with negs as (select top @baseSampleSize co.subject_id as person_id, FIRST_VALUE(v.visit_start_date) OVER (PARTITION BY v.person_id ORDER BY NewId()) visit_start_date
    	from #finalCohort co
			join @cdm_database_schema.visit_occurrence v
				  on v.person_id = co.subject_id
					and v.visit_concept_id in (@visitType)

					and v.visit_start_date >= dateadd(day, 0, co.COHORT_START_DATE)
					and v.visit_start_date <= dateadd(day, 0, co.COHORT_START_DATE)

					and v.visit_start_date >= cast('@startDate' AS DATE)
			    and v.visit_start_date <= cast('@endDate' AS DATE)
		   join (
		  select person_id,
			datediff(day, min(observation_period_start_date), min(observation_period_end_date)) lenPd,
			min(observation_period_start_date) observation_period_start_date,
			min(observation_period_end_date) observation_period_end_date,
			count(observation_period_id) cntPd
		  from @cdm_database_schema.observation_period
		  group by person_id) obs2
		  on v.person_id = obs2.person_id
			AND v.visit_start_date >= dateadd(d, @minimumOffsetFromStart, obs2.observation_period_start_date)
			and dateadd(d, @minimumOffsetFromEnd, v.visit_start_date) <= obs2.observation_period_end_date
			and cntPd = 1
				join @cdm_database_schema.person p
				  on co.subject_id = p.person_id
					and  year(co.COHORT_START_DATE) - year_of_birth >= @ageLimit
					and year(co.COHORT_START_DATE) - year_of_birth <= @upperAgeLimit
					and gender_concept_id in (@gender)
					{@race != 0} ? {AND race_concept_id in (@race)}
		{@ethnicity != 0} ? {AND ethnicity_concept_id in (@ethnicity)}
				where co.cohort_definition_id = @caseCohortId
  order by NewId())
select distinct CAST(0 AS BIGINT) as COHORT_DEFINITION_ID, person_id as SUBJECT_ID,
	dateadd(day, 0, visit_start_date) COHORT_START_DATE,
	dateadd(day, 1, visit_start_date) COHORT_END_DATE
INTO @work_database_schema.@test_cohort
from (SELECT *
	  from negs) negs
;

TRUNCATE TABLE #finalCohort;
DROP TABLE #finalCohort;

TRUNCATE TABLE #adjustedCaseCohort;
DROP TABLE #adjustedCaseCohort;

TRUNCATE TABLE #persons;
DROP TABLE #persons;

