# Append Usagi File to Source to Concept Map Table

Reads a Usagi mapping file and appends its contents to a Source to
Concept Map table. The function supports both default and extended
formats for both Usagi files and STCM tables.

## Usage

``` r
appendUsagiFileToSTCMtable(
  vocabularyId,
  pathToUsagiFile,
  connection,
  vocabularyDatabaseSchema,
  sourceToConceptMapTable,
  includeConcepts = c("APPROVED", "UNCHECKED", "INVALID_TARGET"),
  includeMappings = c("APPROVED"),
  skipValidation = TRUE,
  sourceConceptIdOffset = 0
)
```

## Arguments

- vocabularyId:

  String with the vocabulary ID

- pathToUsagiFile:

  Path to the Usagi file

- connection:

  Database connection object

- vocabularyDatabaseSchema:

  Schema name containing vocabulary tables

- sourceToConceptMapTable:

  Name of source to concept map table

- includeConcepts:

  Vector of mapping statuses to include. Must be subset of: "APPROVED",
  "UNCHECKED", "FLAGGED", "INEXACT", "INVALID_TARGET"

- includeMappings:

  Vector of mapping statuses to include. Must be subset of: "APPROVED",
  "UNCHECKED", "FLAGGED", "INVALID_TARGET"

- skipValidation:

  Whether to skip validation of the Usagi file (default: TRUE)

- sourceConceptIdOffset:

  Offset to add to source concept IDs (default: 0)

## Value

NULL invisibly. The function modifies the database table directly.

## Details

Default STCM columns:

- source_code, source_concept_id, source_vocabulary_id,
  source_code_description

- target_concept_id, target_vocabulary_id, valid_start_date,
  valid_end_date

- invalid_reason

Extended STCM columns (includes all default columns plus):

- source_concept_class

- source_domain

- source_parents_concept_ids

Extended Usagi columns:

- ADD_INFO:sourceConceptId

- ADD_INFO:sourceConceptClass

- ADD_INFO:sourceDomain

If formats don't match (e.g., extended Usagi with default STCM), the
extended information will be ignored with a warning.
