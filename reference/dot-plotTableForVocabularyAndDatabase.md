# Plot detailed table for a vocabulary and database

Plot detailed table for a vocabulary and database

## Usage

``` r
.plotTableForVocabularyAndDatabase(
  summaryTableForVocabularyAndDatabase,
  databaseName,
  vocabularyId,
  colors = list(invalid = "#EC6173", unmapped = "#F1AE4A", mapsTo = "#51A350", grey =
    "#AAAAAA"),
  fileIssueRepo = ""
)
```

## Arguments

- summaryTableForVocabularyAndDatabase:

  Tibble with summary

- databaseName:

  Database name

- vocabularyId:

  Vocabulary ID

- colors:

  List of colors

- fileIssueRepo:

  The repository to file issues to

## Value

A reactable table object
