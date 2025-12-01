# Get summary table for a vocabulary and database

Get summary table for a vocabulary and database

## Usage

``` r
.getSummaryTableForVocabularyAndDatabase(
  pathToCodeCountsFolder,
  pathToVocabularyFolder,
  connectionDetails,
  vocabularyDatabaseSchema,
  sourceVocabularyId,
  targetVocabularyIds,
  databaseName
)
```

## Arguments

- pathToCodeCountsFolder:

  Path to code counts folder

- pathToVocabularyFolder:

  Path to vocabulary folder

- connectionDetails:

  Connection details

- vocabularyDatabaseSchema:

  Vocabulary database schema

- sourceVocabularyId:

  Source vocabulary ID

- targetVocabularyIds:

  Target vocabulary IDs

- databaseName:

  Database name

## Value

Summary table as a tibble
