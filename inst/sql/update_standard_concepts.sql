-- SQL code to check for deprecated and upgraded concepts from the FinOMOP vocabulary mappings using latest OHDSI vocabulary
-- DESCRIPTION:
--
-- Filter for all concepts from FinOMOP vocabularies and their relationship with standard concepts along with invalid_reason of
-- these standard concepts.
-- Look in the concept_relationship table for relationships `Concept poss_eq to`,`Concept replaced by` and `Concept same_as to`
-- for the standard concepts as these imply that they are either deprecated or upgraded.
--
--
-- PARAMETERS:
--
-- cdmDatabaseSchema: schema of CDM vocabulary tables
--

WITH finomop_concepts AS (
  SELECT c.concept_id, c.concept_code, c.concept_name, c.vocabulary_id, cr.concept_id_2, cr.relationship_id
  FROM @cdmDatabaseSchema.concept AS c
  LEFT JOIN @cdmDatabaseSchema.concept_relationship AS cr
  ON cr.concept_id_1 = c.concept_id
  WHERE c.vocabulary_id IN (
                            'FHL','HPN',
                            'ICD8fi','ICD9fi','ICD10fi',
                            'LABfi', 'LABfi_TMP', 'LABfi_TKU', 'LABfi_HUS', 'MEDSPECfi',
                            'MICROBEfi', 'MICROBEfi_TKU',
                            'NCSPfi', 'SNOMED2fi', 'ProcedureModifier',
                            'REIMB', 'SPAT',
                            'UNITfi', 'VNRfi',
                            'ICPC', 'HPO',
                            'ProfessionalCode', 'FGVisitType'
                           ) AND
        cr.relationship_id IS NOT NULL
  ORDER BY c.concept_id
), finomop_concepts_standard AS (
  SELECT fc.*, c.invalid_reason
  FROM finomop_concepts AS fc
  LEFT JOIN @cdmDatabaseSchema.concept AS c
  ON c.concept_id = fc.concept_id_2
)
SELECT fcs.concept_id AS sourceConceptId,
       fcs.concept_name AS sourceName,
       fcs.vocabulary_id,
       cvcr.concept_id_2 AS targetConceptId,
       cvc.concept_name AS targetConceptName,
       cvc.domain_id AS tartgetConceptDomainId
FROM finomop_concepts_standard AS fcs
LEFT JOIN @cdmDatabaseSchema.concept_relationship AS cvcr
ON cvcr.concept_id_1 = fcs.concept_id_2 AND
   cvcr.relationship_id IN ('Concept poss_eq to','Concept replaced by', 'Concept same_as to')
LEFT JOIN @cdmDatabaseSchema.concept AS cvc
ON cvcr.concept_id_2 = cvc.concept_id
WHERE cvcr.concept_id_2 IS NOT NULL
ORDER BY fcs.concept_id;
