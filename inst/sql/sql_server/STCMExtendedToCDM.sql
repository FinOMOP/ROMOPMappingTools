-- 1. VOCABULARY table
-- Done separately in order to add version and description

-- 2. CONCEPT_CLASS table
-- Delete previous rows with the same vocabulary_id as in the source_to_concept_map table
-- Insert one row for each source_concept_class in the source_to_concept_map table
--  - concept_class_id = source_concept_map_table.source_concept_class
--  - concept_class_name = source_concept_map_table.source_concept_class
--  - concept_class_concept_id = 0
--
DELETE FROM
    @vocabularyDatabaseSchema.CONCEPT_CLASS
WHERE
    concept_class_id IN (
        SELECT
            DISTINCT stcm.source_concept_class
        FROM
            @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
        WHERE
            stcm.source_vocabulary_id IS NOT NULL
    );

INSERT INTO
    @vocabularyDatabaseSchema.CONCEPT_CLASS (
        concept_class_id,
        concept_class_name,
        concept_class_concept_id
    )
SELECT
    DISTINCT stcm.source_concept_class AS concept_class_id,
    stcm.source_concept_class AS concept_class_name,
    0 AS concept_class_concept_id
FROM
    @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
WHERE
    stcm.source_concept_class IS NOT NULL;

-- 3. CONCEPT table
-- Delete previous rows with the same vocabulary_id as in the source_to_concept_map table
-- Insert one row for each **unique** source_concept_id in the source_to_concept_map table
--  - concept_id = source_to_concept_map.source_concept_id
--  - concept_name = source_to_concept_map.source_code_description
--  - domain_id = calculated domain from the domain or domains mapped to the source_concept_id, if it not mapped, use source_to_concept_map.source_domain
--  - vocabulary_id = source_to_concept_map.source_vocabulary_id
--  - concept_class_id = source_to_concept_map.source_concept_class
--  - standard_concept = NULL
--  - concept_code = source_to_concept_map.source_code
--  - valid_start_date = if source_to_concept_map.valid_start_date is not NULL, use it, otherwise use '1970-01-01'
--  - valid_end_date = if source_to_concept_map.valid_end_date is not NULL, use it, otherwise use '2099-12-31'
--  - invalid_reason = NULL
--
DELETE FROM
    @vocabularyDatabaseSchema.CONCEPT
WHERE
    vocabulary_id IN (
        SELECT
            DISTINCT stcm.source_vocabulary_id
        FROM
            @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
        WHERE
            stcm.source_vocabulary_id IS NOT NULL
    );

INSERT INTO
    @vocabularyDatabaseSchema.CONCEPT (
        concept_id,
        concept_name,
        domain_id,
        vocabulary_id,
        concept_class_id,
        standard_concept,
        concept_code,
        valid_start_date,
        valid_end_date,
        invalid_reason
    )

WITH mapped_domains AS (
    SELECT
        domains.source_concept_id AS source_concept_id,
        STRING_AGG(domains.domain_id, ' ') AS domain_ids,
        CASE
            WHEN domain_ids = 'Condition Device' THEN 'Condition/Device'
            WHEN domain_ids = 'Condition Measurement' THEN 'Condition/Meas'
            WHEN domain_ids = 'Condition Observation' THEN 'Condition/Obs'
            WHEN domain_ids = 'Condition Procedure' THEN 'Condition/Procedure'
            WHEN domain_ids = 'Device Drug' THEN 'Device/Drug'
            WHEN domain_ids = 'Device Procedure' THEN 'Device/Procedure'
            WHEN domain_ids = 'Drug Procedure' THEN 'Drug/Procedure'
            WHEN domain_ids = 'Measurement Procedure' THEN 'Meas/Procedure'
            WHEN domain_ids = 'Observation Procedure' THEN 'Obs/Procedure'
            -- TEMP for some reason the sorting is not working, so I repeat the same logic for the reverse
            WHEN domain_ids = 'Device Condition' THEN 'Condition/Device'
            WHEN domain_ids = 'Measurement Condition' THEN 'Condition/Meas'
            WHEN domain_ids = 'Observation Condition' THEN 'Condition/Obs'
            WHEN domain_ids = 'Procedure Condition' THEN 'Condition/Procedure'
            WHEN domain_ids = 'Drug Device' THEN 'Device/Drug'
            WHEN domain_ids = 'Procedure Device' THEN 'Device/Procedure'
            WHEN domain_ids = 'Procedure Drug' THEN 'Drug/Procedure'
            WHEN domain_ids = 'Procedure Measurement' THEN 'Meas/Procedure'
            WHEN domain_ids = 'Procedure Observation' THEN 'Obs/Procedure'
            -- END TEMP
            ELSE domain_ids
        END AS calculated_domain
    FROM
        (
            SELECT
                DISTINCT stcm.source_concept_id AS source_concept_id,
                c.domain_id AS domain_id
            FROM
                @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
                INNER JOIN @vocabularyDatabaseSchema.CONCEPT AS c ON stcm.target_concept_id = c.concept_id
            WHERE
                stcm.target_concept_id != 0
            ORDER BY
                c.domain_id
        ) AS domains
    GROUP BY
        domains.source_concept_id
    ORDER BY
        domains.source_concept_id
)
SELECT
    DISTINCT stcm.source_concept_id AS concept_id,
    stcm.source_code_description AS concept_name,
    IF(
        md.calculated_domain IS NULL,
        stcm.source_domain,
        md.calculated_domain
    ) AS domain_id,
    stcm.source_vocabulary_id AS vocabulary_id,
    stcm.source_concept_class AS concept_class,
    NULL AS standard_concept,
    stcm.source_code AS concept_code,
    CAST(
        COALESCE(stcm.valid_start_date, '1970-01-01') AS DATE
    ) AS valid_start_date,
    CAST(
        COALESCE(stcm.valid_end_date, '2099-12-31') AS DATE
    ) AS valid_end_date,
    NULL AS invalid_reason
FROM
    @vocabularyDatabaseSchema.@sourceToConceptMapTable stcm
    LEFT JOIN mapped_domains AS md ON stcm.source_concept_id = md.source_concept_id
ORDER BY
    stcm.source_concept_id;


-- 4. CONCEPT_RELATIONSHIP table
-- Delete previous rows with the same concept_id_1 or concept_id_2 as in the source_to_concept_map.source_concept_id 
-- Insert one row for each relationship of type 'Maps to', 'Maps from', 'Subsumes', 'Is a'
-- 'Maps to' as follows:
--  - concept_id_1 = source_to_concept_map.source_concept_id
--  - concept_id_2 = source_to_concept_map.target_concept_id
--  - relationship_id = 'Maps to'
--  - valid_start_date = if source_to_concept_map.valid_start_date is not NULL, use it, otherwise use '1970-01-01'
--  - valid_end_date = if source_to_concept_map.valid_end_date is not NULL, use it, otherwise use '2099-12-31'
--  - invalid_reason = NULL 
-- 'Maps from' as follows:
--  - concept_id_1 = source_to_concept_map.target_concept_id
--  - concept_id_2 = source_to_concept_map.source_concept_id
--  - relationship_id = 'Maps from'
--  - valid_start_date = if source_to_concept_map.valid_start_date is not NULL, use it, otherwise use '1970-01-01'
--  - valid_end_date = if source_to_concept_map.valid_end_date is not NULL, use it, otherwise use '2099-12-31'
--  - invalid_reason = NULL 
-- 'Subsumes' as follows:
--  - concept_id_1 = source_to_concept_map.source_concept_id
--  - concept_id_2 = any concept_id in source_to_concept_map.source_parents_concept_ids
--  - relationship_id = 'Subsumes'
--  - valid_start_date = if source_to_concept_map.valid_start_date is not NULL, use it, otherwise use '1970-01-01'
--  - valid_end_date = if source_to_concept_map.valid_end_date is not NULL, use it, otherwise use '2099-12-31'
--  - invalid_reason = NULL 
-- 'Is a' as follows:
--  - concept_id_1 = any concept_id in source_to_concept_map.source_parents_concept_ids
--  - concept_id_2 = source_to_concept_map.source_concept_id
--  - relationship_id = 'Is a'
--  - valid_start_date = if source_to_concept_map.valid_start_date is not NULL, use it, otherwise use '1970-01-01'
--  - valid_end_date = if source_to_concept_map.valid_end_date is not NULL, use it, otherwise use '2099-12-31'
--  - invalid_reason = NULL 
DELETE FROM
    @vocabularyDatabaseSchema.CONCEPT_RELATIONSHIP
WHERE
    concept_id_1 IN (
        SELECT
            DISTINCT stcm.source_concept_id
        FROM
            @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
    ) OR concept_id_2 IN (
        SELECT
            DISTINCT stcm.source_concept_id
        FROM
            @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
    );

-- maps to
INSERT INTO
    @vocabularyDatabaseSchema.CONCEPT_RELATIONSHIP (
        concept_id_1,
        concept_id_2,
        relationship_id,
        valid_start_date,
        valid_end_date,
        invalid_reason
    )

SELECT DISTINCT
    stcm.source_concept_id AS concept_id_1,
    stcm.target_concept_id AS concept_id_2,
    'Maps to' AS relationship_id,
    CAST(
        COALESCE(stcm.valid_start_date, '1970-01-01') AS DATE
    ) AS valid_start_date,
    CAST(
        COALESCE(stcm.valid_end_date, '2099-12-31') AS DATE
    ) AS valid_end_date,
    NULL AS invalid_reason
FROM
    @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
WHERE
    stcm.source_concept_id != 0
    AND stcm.target_concept_id != 0;

INSERT INTO
    @vocabularyDatabaseSchema.CONCEPT_RELATIONSHIP (
        concept_id_1,
        concept_id_2,
        relationship_id,
        valid_start_date,
        valid_end_date,
        invalid_reason
    )

-- maps from
SELECT DISTINCT
    stcm.target_concept_id AS concept_id_1,
    stcm.source_concept_id AS concept_id_2,
    'Mapped from' AS relationship_id,
    CAST(
        COALESCE(stcm.valid_start_date, '1970-01-01') AS DATE
    ) AS valid_start_date,
    CAST(
        COALESCE(stcm.valid_end_date, '2099-12-31') AS DATE
    ) AS valid_end_date,
    NULL AS invalid_reason
FROM
    @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
WHERE
    stcm.target_concept_id != 0
    AND stcm.source_concept_id != 0;

-- subsumes
INSERT INTO 
    @vocabularyDatabaseSchema.CONCEPT_RELATIONSHIP (
        concept_id_1,
        concept_id_2,
        relationship_id,
        valid_start_date,
        valid_end_date,
        invalid_reason
    )

WITH RECURSIVE split_parents AS (
    SELECT 
        source_concept_id,
        valid_start_date,
        valid_end_date,
        -- Get the first value before the delimiter
        SUBSTRING(source_parents_concept_ids, 1, 
            CASE 
                WHEN POSITION('|' IN source_parents_concept_ids) = 0 
                THEN LENGTH(source_parents_concept_ids)
                ELSE POSITION('|' IN source_parents_concept_ids) - 1
            END) AS source_parents_concept_ids,
        -- Get the remaining string after the delimiter
        CASE 
            WHEN POSITION('|' IN source_parents_concept_ids) = 0 
            THEN NULL
            ELSE SUBSTRING(source_parents_concept_ids, 
                         POSITION('|' IN source_parents_concept_ids) + 1)
        END AS remaining_string
    FROM @vocabularyDatabaseSchema.@sourceToConceptMapTable AS stcm
    WHERE stcm.source_parents_concept_ids IS NOT NULL
    
    UNION ALL
    
    SELECT 
        source_concept_id,
        valid_start_date,
        valid_end_date,
        SUBSTRING(remaining_string, 1, 
            CASE 
                WHEN POSITION('|' IN remaining_string) = 0 
                THEN LENGTH(remaining_string)
                ELSE POSITION('|' IN remaining_string) - 1
            END),
        CASE 
            WHEN POSITION('|' IN remaining_string) = 0 
            THEN NULL
            ELSE SUBSTRING(remaining_string, 
                         POSITION('|' IN remaining_string) + 1)
        END
    FROM split_parents
    WHERE remaining_string IS NOT NULL
)

SELECT DISTINCT
    CAST(sp.source_concept_id AS INTEGER) AS concept_id_1,
    CAST(sp.source_parents_concept_ids AS INTEGER) AS concept_id_2,
    'Subsumes' AS relationship_id,
    CAST(
        COALESCE(sp.valid_start_date, '1970-01-01') AS DATE
    ) AS valid_start_date,
    CAST(
        COALESCE(sp.valid_end_date, '2099-12-31') AS DATE
    ) AS valid_end_date,
    NULL AS invalid_reason
FROM split_parents AS sp
WHERE sp.source_parents_concept_ids IS NOT NULL
UNION ALL

-- is a
SELECT DISTINCT
    CAST(sp.source_parents_concept_ids AS INTEGER) AS concept_id_1,
    CAST(sp.source_concept_id AS INTEGER) AS concept_id_2,
    'Is a' AS relationship_id,
    CAST(
        COALESCE(sp.valid_start_date, '1970-01-01') AS DATE
    ) AS valid_start_date,
    CAST(
        COALESCE(sp.valid_end_date, '2099-12-31') AS DATE
    ) AS valid_end_date,
    NULL AS invalid_reason
FROM split_parents AS sp
WHERE sp.source_parents_concept_ids IS NOT NULL