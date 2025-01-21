-- VOCABULARY table
-- Delete previous rows with the same vocabulary_id as in the source_to_concept_map table
-- Insert one row for each vocabulary_id in the source_to_concept_map table
--  - vocabulary_name is the same as vocabulary_id
--  - vocabulary_reference is NULL
--  - vocabulary_version is NULL
--  - vocabulary_concept_id is 0

DELETE FROM
    @vocabularyDatabaseSchema.VOCABULARY
WHERE
    vocabulary_id IN (
        SELECT DISTINCT source_vocabulary_id 
        FROM @vocabularyDatabaseSchema.@sourceToConceptMapTable
        WHERE source_vocabulary_id IS NOT NULL
    );

INSERT INTO
    @vocabularyDatabaseSchema.VOCABULARY (
        vocabulary_id,
        vocabulary_name,
        vocabulary_reference,
        vocabulary_version,
        vocabulary_concept_id
    )
SELECT DISTINCT
    @vocabularyDatabaseSchema.@sourceToConceptMapTable.source_vocabulary_id AS vocabulary_id,
    @vocabularyDatabaseSchema.@sourceToConceptMapTable.source_vocabulary_id AS vocabulary_name,
    @vocabularyDatabaseSchema.@sourceToConceptMapTable.source_vocabulary_id AS vocabulary_reference,
    '' AS vocabulary_version,
    0 AS vocabulary_concept_id
FROM
    @vocabularyDatabaseSchema.@sourceToConceptMapTable
WHERE
    @vocabularyDatabaseSchema.@sourceToConceptMapTable.source_vocabulary_id IS NOT NULL;


-- CONCEPT table
-- Delete previous rows with the same vocabulary_id as in the source_to_concept_map table
-- Insert one row for each source_code in the source_to_concept_map table
--  - concept_id is 0
--  - concept_name is the same as source_code
--  - domain_id is 'Source'
--  - vocabulary_id is the same as source_vocabulary_id
--  - concept_class_id is 'Source'
--  - standard_concept is NULL
--  - concept_code is the same as source_code
--  - valid_start_date is the same as source_valid_start_date
--  - valid_end_date is the same as source_valid_end_date
--  - invalid_reason is NULL

DELETE FROM
    @vocabularyDatabaseSchema.CONCEPT
WHERE
    vocabulary_id IN (
        SELECT DISTINCT source_vocabulary_id 
        FROM @vocabularyDatabaseSchema.@sourceToConceptMapTable
        WHERE source_vocabulary_id IS NOT NULL
    );

WITH domain_summary AS (
    SELECT 
        source_code,
        STRING_AGG(domain_id, ' ') WITHIN GROUP (ORDER BY domain_id) AS calculated_domain,
        COUNT(DISTINCT domain_id) AS n_domains
    FROM @vocabularyDatabaseSchema.@sourceToConceptMapTable
    WHERE source_code IS NOT NULL
    GROUP BY source_code
),
recalculated_domains AS (
    SELECT 
        source_code,
        CASE 
            WHEN calculated_domain = 'Condition Device' THEN 'Condition/Device'
            WHEN calculated_domain = 'Condition Measurement' THEN 'Condition/Meas'
            WHEN calculated_domain = 'Condition Observation' THEN 'Condition/Obs'
            WHEN calculated_domain = 'Condition Procedure' THEN 'Condition/Procedure'
            WHEN calculated_domain = 'Device Drug' THEN 'Device/Drug'
            WHEN calculated_domain = 'Device Procedure' THEN 'Device/Procedure'
            WHEN calculated_domain = 'Drug Procedure' THEN 'Drug/Procedure'
            WHEN calculated_domain = 'Measurement Procedure' THEN 'Meas/Procedure'
            WHEN calculated_domain = 'Observation Procedure' THEN 'Obs/Procedure'
            ELSE calculated_domain
        END AS domain_id
    FROM domain_summary
    WHERE n_domains > 1
)
SELECT DISTINCT
    s.*,
    COALESCE(r.domain_id, s.domain_id) AS final_domain_id
FROM @vocabularyDatabaseSchema.@sourceToConceptMapTable s
LEFT JOIN recalculated_domains r ON s.source_code = r.source_code
WHERE s.source_code IS NOT NULL;
