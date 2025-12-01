# Convert Source to Concept Map to CDM Tables

Converts the contents of a Source to Concept Map table into standard CDM
vocabulary tables. This includes creating entries in:

- CONCEPT table for source concepts

- CONCEPT_RELATIONSHIP table for mapping relationships

- Additional relationship types if using extended STCM format

## Usage

``` r
STCMToCDMTables(connection, vocabularyDatabaseSchema, sourceToConceptMapTable)
```

## Arguments

- connection:

  A DatabaseConnector connection object

- vocabularyDatabaseSchema:

  Schema name where the vocabulary tables are stored

- sourceToConceptMapTable:

  Name of the source to concept map table
