select distinct ca.ancestor_concept_id as concept_id
from @cdm_schema.concept_ancestor ca
join @cdm_schema.concept c
	on c.concept_id = ca.ancestor_concept_id
where descendant_concept_id in (@conceptList)
	and c.concept_class_id = 'Ingredient';
