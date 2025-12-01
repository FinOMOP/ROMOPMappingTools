# Export vocabulary tables from a database to CSV files

Export vocabulary tables from a database to CSV files

## Usage

``` r
duckdbToOMOPVocabularyCSVs(
  connection,
  vocabularyDatabaseSchema,
  OMOPVocabularyTableNames,
  pathToOMOPVocabularyCSVsFolder
)
```

## Arguments

- connection:

  DatabaseConnector connection object to the database containing
  vocabulary tables

- vocabularyDatabaseSchema:

  Schema name where vocabulary tables are located

- OMOPVocabularyTableNames:

  Vector of vocabulary table names to export. If NULL, exports standard
  set.

- pathToOMOPVocabularyCSVsFolder:

  Directory path where CSV files will be written

## Value

No return value, called for side effects
