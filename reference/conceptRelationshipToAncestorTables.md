# Convert CONCEPT_RELATIONSHIP to CONCEPT_ANCESTOR

Creates and populates the CONCEPT_ANCESTOR table from
CONCEPT_RELATIONSHIP table data.

## Usage

``` r
conceptRelationshipToAncestorTables(
  connection,
  vocabularyDatabaseSchema,
  vocabularyList
)
```

## Arguments

- connection:

  A DatabaseConnector connection object

- vocabularyDatabaseSchema:

  Schema containing the vocabulary and STCM tables

- vocabularyList:

  Vector of vocabulary_ids to include (default: c("ICD10"))

## Value

Invisible TRUE if successful
