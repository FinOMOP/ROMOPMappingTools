{DEFAULT @overwrite = TRUE}

{@overwrite == 'TRUE'} ? {
-- Delete if exists
DROP TABLE IF EXISTS @vocabularyDatabaseSchema.@sourceToConceptMapTable;
}

--HINT DISTRIBUTE ON RANDOM
CREATE TABLE @vocabularyDatabaseSchema.@sourceToConceptMapTable (
			source_code varchar(50) NOT NULL,
			source_concept_id integer NOT NULL,
			source_vocabulary_id varchar(20) NOT NULL,
			source_code_description varchar(255) NULL,
			target_concept_id integer NOT NULL,
			target_vocabulary_id varchar(20) NOT NULL,
			valid_start_date date NOT NULL,
			valid_end_date date NOT NULL,
			invalid_reason varchar(1) NULL , 
            source_concept_class varchar(20) NULL,
            source_domain varchar(20) NULL,
            source_parents_concept_ids varchar(255) NULL
			);