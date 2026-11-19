select syn.*, c.concept_name, c2.concept_name as finding_site
from @cdm_schema.concept c
join @cdm_schema.concept_synonym syn
	on syn.concept_id = c.concept_id
join @cdm_schema.concept_relationship cr
	on cr.concept_id_1 = c.concept_id
	and cr.relationship_id = 'Has finding site'
join @cdm_schema.concept c2
	on c2.concept_id = cr.concept_id_2
where c.concept_id in (@concepts);
