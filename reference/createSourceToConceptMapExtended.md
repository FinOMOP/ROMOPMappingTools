# Create Extended Source to Concept Map Table

Creates an extended version of the Source to Concept Map table that
includes additional columns for source concept class, domain, and parent
relationships.

## Usage

``` r
createSourceToConceptMapExtended(
  connection,
  vocabularyDatabaseSchema,
  sourceToConceptMapTable,
  overwrite = TRUE
)
```

## Arguments

- connection:

  A DatabaseConnector connection object

- vocabularyDatabaseSchema:

  Schema name where the vocabulary tables are stored

- sourceToConceptMapTable:

  Name of the extended source to concept map table to create

- overwrite:

  Whether to overwrite an existing table. Default is TRUE
