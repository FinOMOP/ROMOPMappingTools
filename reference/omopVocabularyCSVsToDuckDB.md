# Convert OMOP Vocabulary CSVs to Database

This function converts OMOP vocabulary CSV files to a Database.

## Usage

``` r
omopVocabularyCSVsToDuckDB(
  pathToOMOPVocabularyCSVsFolder,
  connection,
  vocabularyDatabaseSchema
)
```

## Arguments

- pathToOMOPVocabularyCSVsFolder:

  Path to folder containing OMOP vocabulary CSV files

- connection:

  A DatabaseConnector connection object

- vocabularyDatabaseSchema:

  Schema name where the vocabulary tables are stored

## Value

Nothing
