# Tables description and rules

## Usagi file format

### Usagi default columns

One row per sourceCode + conceptId combination.

| Name                         | Type      | Description                                         | Rules checked by validateUsagiFile                                    |
|------------------------------|-----------|-----------------------------------------------------|-----------------------------------------------------------------------|
| sourceCode                   | character | The source code to be mapped                        | not empty, unique sourceCode + conceptId combinations                 |
| sourceName                   | character | The name/description of the source code             | not empty, less than 255 characters                                   |
| sourceFrequency              | integer   | How frequently this code appears in the source data | not empty, set to -1 if not known                                     |
| sourceAutoAssignedConceptIds | integer   | Automatically assigned concept IDs                  |                                                                       |
| matchScore                   | double    | Score indicating quality of the mapping match (0-1) |                                                                       |
| mappingStatus                | character | Status of the mapping (APPROVED, UNCHECKED, etc.)   | Required, one of the following: APPROVED, UNCHECKED, FLAGGED, INEXACT |
| equivalence                  | character | Type of equivalence (EQUIVALENT, BROADER, etc.)     |                                                                       |
| statusSetBy                  | character | User who set the mapping status                     |                                                                       |
| statusSetOn                  | double    | Timestamp when status was set                       |                                                                       |
| conceptId                    | integer   | Target OMOP concept ID                              | not empty, if 0 the mappingStatus cannot be APPROVED                  |
| conceptName                  | character | Name of target OMOP concept                         |                                                                       |
| domainId                     | character | Domain of target concept (Condition, Drug, etc.)    |                                                                       |
| mappingType                  | character | Type of mapping (MAPS_TO, etc.)                     |                                                                       |
| comment                      | character | Comments about the mapping                          |                                                                       |
| createdBy                    | character | User who created the mapping                        |                                                                       |
| createdOn                    | double    | Timestamp when mapping was created                  |                                                                       |
| assignedReviewer             | character | User assigned to review the mapping                 |                                                                       |

### Usagi extended columns

The Usagi file is considered a C&CR file if it has the following
columns: ADD\_<INFO:sourceConceptId>, ADD\_<INFO:sourceConceptClass> and
ADD\_<INFO:sourceDomain>.

The pair ADD\_<INFO:sourceValidStartDate> and
ADD\_<INFO:sourceValidEndDate> are optional. If not included, the
respective columns in the CONCEPT table will be set to the default
values, which are 1900-01-01 and 2099-12-31.

The pair ADD\_<INFO:sourceParents> and
ADD\_<INFO:sourceParentVocabulary> are optional. If included, they will
be use to populate the CONCEPT_RELATIONSHIP table with the ‘Is a’ and
‘Subsumes’ relationships.

The ADD\_<INFO:validationMessages> column is added by validateUsagiFile
and contains the messages from the validation checks.

| Name                               | Type      | Description                        | Rules                                                                                                                                                |
|------------------------------------|-----------|------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| ADD\_<INFO:sourceConceptId>        | double    | Source vocabulary concept ID       | not empty, number on the range given by sourceConceptIdOffset, must be unique per each sourceCode                                                    |
| ADD\_<INFO:sourceConceptClass>     | character | Concept class in source vocabulary | not empty, less than 20 characters                                                                                                                   |
| ADD\_<INFO:sourceDomain>           | character | Domain in source vocabulary        | not empty, value exist in the DOMAIN table, when the code maps to more than one concept the combined domain is valid                                 |
| ADD\_<INFO:sourceValidStartDate>   | date      | Start date of validity in source   | if empty, the default value is 1900-01-01, the value must be before ADD\_<INFO:sourceValidEndDate>                                                   |
| ADD\_<INFO:sourceValidEndDate>     | date      | End date of validity in source     | if empty, the default value is 2099-12-31, the value must be after ADD\_<INFO:sourceValidStartDate>                                                  |
| ADD\_<INFO:sourceParents>          | character | Parent codes in source vocabulary  | not empty, if more that one parent, separated by, combination of sourceParents and sourceParentVocabulary must exits in the CDM or in the usagi file |
| ADD\_<INFO:sourceParentVocabulary> | character | Vocabularies of parent codes       | if empty, the vocabulary is itself, if more that one parent, separated by                                                                            |
| ADD\_<INFO:validationMessages>     | character | Column added by validateUsagiFile  | Optional                                                                                                                                             |
| ADD\_<INFO:autoUpdatingInfo>       | character | Column added by updateUsagiFile    | Optional                                                                                                                                             |

## vocabularies.csv file format

The `vocabularies.csv` file is used to describe the vocabularies to be
processed. It is a csv file with the following columns:

| Name                     | Type      | Description                                                 | Rules                               |
|--------------------------|-----------|-------------------------------------------------------------|-------------------------------------|
| source_vocabulary_id     | character | The id of the vocabulary                                    | not empty, less than 20 characters  |
| source_vocabulary_name   | character | A description of the vocabulary                             | not empty, less than 255 characters |
| source_concept_id_offset | integer   | The offset of the source concept id                         | not empty, number over 2 billion    |
| path_to_usagi_file       | character | The path to the vocabulary’s Usagi file                     | not empty, file must exist          |
| path_to_news_file        | character | The path to the vocabulary’s news file                      | not empty, file must exist          |
| ignore                   | boolean   | Indicates if the vocabulary should be ignored in processing | not empty                           |

## SOURCE_TO_CONCEPT_MAP_EXTENDED table format

The SOURCE_TO_CONCEPT_MAP_EXTENDED is an extension of the
SOURCE_TO_CONCEPT_MAP table, see
[CDM](https://ohdsi.github.io/CommonDataModel/cdm54.html#source_to_concept_map).
It is used to store the source to concept map extended information.

The SOURCE_TO_CONCEPT_MAP_EXTENDED table has the following columns:

| Name                       | Type      | Description                                   | Rules                                     |
|----------------------------|-----------|-----------------------------------------------|-------------------------------------------|
| source_code                | character | Source code for the concept                   | not empty                                 |
| source_vocabulary_id       | character | Source vocabulary the concept was mapped from | not empty, must exist in VOCABULARY table |
| source_code_description    | character | Description of source code                    | not empty                                 |
| target_concept_id          | integer   | Concept ID of the target concept              | not empty, must exist in CONCEPT table    |
| target_vocabulary_id       | character | Target vocabulary the concept was mapped to   | not empty, must exist in VOCABULARY table |
| valid_start_date           | date      | Date when mapping became valid                | not empty, must be before valid_end_date  |
| valid_end_date             | date      | Date when mapping became invalid              | not empty, must be after valid_start_date |
| invalid_reason             | character | Reason why mapping was invalidated            | empty if valid_end_date is 2099-12-31     |
| source_concept_id          | integer   | Source concept ID                             | not empty                                 |
| source_concept_class       | character | Concept class in source vocabulary            | not empty, less than 20 characters        |
| source_domain              | character | Domain in source vocabulary                   | not empty, must exist in DOMAIN table     |
| source_parents_concept_ids | character | Parent concept IDs in source vocabulary       | optional, comma-separated list            |
