-- DESCRIPTION:
-- Adds non-standard FinOMOP and specified vocabularies concept ancestry to the concetp_ancestor table
--
-- PARAMETERS:
--
-- - vocabularyDatabaseSchema: schema containing the vocabulary and STCM tables
-- - vocabularyList: comma-separated list of vocabulary_ids to include

-- 1-  Create a temporary table with concept relationships that have
-- 1-1 Any concept > 2 billion or from specified vocabularies
-- 1-2 Only get that have relationship `Subsumes` present

DROP TABLE IF EXISTS #relationships;

SELECT cr.concept_id_1, cr.concept_id_2
INTO #relationships
FROM @vocabularyDatabaseSchema.concept c
INNER JOIN @vocabularyDatabaseSchema.concept_relationship cr
  ON cr.concept_id_1 = c.concept_id
WHERE c.vocabulary_id IN (@vocabularyList)
  AND cr.relationship_id = 'Subsumes'
ORDER BY cr.concept_id_1, cr.concept_id_2;

-- 2- Insert the created non-standard concept ancestries to concept_ancestor table in omop vocab
INSERT INTO @vocabularyDatabaseSchema.concept_ancestor
(
	ancestor_concept_id,
	descendant_concept_id,
	min_levels_of_separation,
	max_levels_of_separation
)
-- 3- Create a min and max distance as 1 for every concept relation
-- 3-1 Recursively check descendant_concept_id until there are no descendants
WITH RECURSIVE ancestor_cte AS (
-- Base case: direct relationships
	SELECT concept_id_1 AS ancestor_concept_id,
			concept_id_2 AS descendant_concept_id,
			1 AS min_levels_of_separation,
			1 AS max_levels_of_separation
	FROM #relationships

	UNION ALL

-- Recursive case: find descendant relationships
	SELECT r.concept_id_1 AS ancestor_concept_id,
			c.descendant_concept_id AS descendant_concept_id,
			c.min_levels_of_separation + 1 AS min_levels_of_separation,
			c.max_levels_of_separation + 1 AS max_levels_of_separation
	FROM #relationships r
	JOIN ancestor_cte c
	ON r.concept_id_2 = c.ancestor_concept_id
), ancestor_cte_self_reference AS (
  SELECT ancestor_concept_id,
         descendant_concept_id,
         min_levels_of_separation,
         max_levels_of_separation
  FROM ancestor_cte
  UNION ALL
# Add self reference for each ancestor_concept_id
  SELECT r.concept_id_1 AS ancestor_concept_id,
			   r.concept_id_1 AS descendant_concept_id,
			   0 AS min_levels_of_separation,
			   0 AS max_levels_of_separation
  FROM (
    SELECT DISTINCT concept_id_1
    FROM #relationships
  ) AS r
  UNION ALL
# Add self reference for each descendant_concept_id
  SELECT r.concept_id_2 AS ancestor_concept_id,
			   r.concept_id_2 AS descendant_concept_id,
			   0 AS min_levels_of_separation,
			   0 AS max_levels_of_separation
  FROM (
    SELECT DISTINCT concept_id_2
    FROM #relationships
  ) AS r
)
SELECT *
FROM ancestor_cte_self_reference;


-- 4- Remove the temporary table
DROP TABLE #relationships;
