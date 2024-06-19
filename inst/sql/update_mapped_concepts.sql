-- SQL code to check for deprecated and upgraded concepts from the FinOMOP vocabulary mappings using latest OHDSI vocabulary
-- DESCRIPTION:
--
-- Filter for all concepts from FinOMOP vocabularies and their relationship with standard/non-standard concepts along with
-- invalid_reason of these standard/non-standard concepts.
-- Look in the concept_relationship table for relationships `Maps to`, `Concept poss_eq to`,`Concept replaced by` and
-- `Concept same_as to` for the standard/non-standard concepts as these imply that they are either deprecated or upgraded.
--
-- PARAMETERS:
--
-- cdmDatabaseSchema: schema of CDM vocabulary tables


-- Get all the concepts from FinOMOP vocabularies with concept_id > 2000000000 and have been mapped
WITH finomop_concepts AS (
  SELECT c.concept_id,
         c.concept_code,
         c.concept_name,
         c.vocabulary_id,
         cr.concept_id_2 AS current_mapped_concept,
         cr.relationship_id
  FROM @cdmDatabaseSchema.concept AS c
  LEFT JOIN @cdmDatabaseSchema.concept_relationship AS cr
  ON cr.concept_id_1 = c.concept_id
  WHERE c.concept_id > 2000000000 AND
        cr.relationship_id IS NOT NULL
  ORDER BY c.concept_id
),
-- Get the current mapped concept status through standard_concept and invalid_reason
finomop_concepts_standard AS (
  SELECT fc.concept_id,
         fc.concept_code,
         fc.concept_name,
         fc.vocabulary_id,
         fc.current_mapped_concept,
         fc.relationship_id,
         c.invalid_reason AS current_mapped_concept_invalid_reason,
         c.standard_concept AS current_mapped_concept_standard_concept
  FROM finomop_concepts AS fc
  LEFT JOIN @cdmDatabaseSchema.concept AS c
  ON c.concept_id = fc.current_mapped_concept
),
-- Check whether the current_mapped_concept is either upgraded or discontiued
-- Here, we are checking the following
-- 1. Whether a current_mapped_concept is not standard but has been upgraded to Standard in the latest OHDSI release
-- 2 Whether a current_mapped_concept is standard but has been upgraded/dicontinued in the latest OHDSI release
finomop_concepts_standard_status AS (
  SELECT fcs.concept_id,
         fcs.concept_code,
         fcs.concept_name,
         fcs.vocabulary_id,
         fcs.current_mapped_concept,
         fcs.relationship_id,
         fcs.current_mapped_concept_invalid_reason,
         fcs.current_mapped_concept_standard_concept,
         cvc.invalid_reason AS new_invalid_reason,
         cvc.standard_concept AS new_standard_concept
  FROM finomop_concepts_standard AS fcs
  LEFT JOIN @cdmDatabaseSchema.concept AS cvc
  ON cvc.concept_id = fcs.current_mapped_concept
  WHERE (fcs.current_mapped_concept_standard_concept IS NULL AND cvc.invalid_reason IS NOT NULL) OR
        (fcs.current_mapped_concept_standard_concept = 'S' AND cvc.invalid_reason IS NOT NULL)
)
-- Get the latest mappings of these current_mapped_concept upgraded or discontinued
-- Latest mappings through concept relationship which is 'Maps to', 'Concept poss_eq to','Concept replaced by', 'Concept same_as to'
SELECT fcss.concept_id AS source_concept_id,
       fcss.concept_name AS source_concept_name,
       fcss.vocabulary_id,
       fcss.current_mapped_concept AS obsolete_mapped_concept_id,
       cvcr.relationship_id AS obselete_to_target_relation
       cvcr.concept_id_2 AS target_concept_id,
       cvc.concept_name AS target_concept_name,
       cvc.domain_id AS target__domain_id
FROM finomop_concepts_standard_status AS fcss
LEFT JOIN @cdmDatabaseSchema.concept_relationship AS cvcr
ON cvcr.concept_id_1 = fcss.current_mapped_concept AND
   cvcr.relationship_id IN ('Maps to', 'Concept poss_eq to','Concept replaced by', 'Concept same_as to')
LEFT JOIN @cdmDatabaseSchema.concept AS cvc
ON cvcr.concept_id_2 = cvc.concept_id
WHERE cvcr.concept_id_2 IS NOT NULL
ORDER BY fcss.concept_id;
