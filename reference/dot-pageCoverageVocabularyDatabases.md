# Render coverage page for a vocabulary and its databases

Render coverage page for a vocabulary and its databases

## Usage

``` r
.pageCoverageVocabularyDatabases(
  summaryTableForVocabularyAndDatabaseList,
  usagiTibble,
  sourceVocabularyId,
  outputFolderPath,
  pathToNewsFile,
  fileIssueRepo = ""
)
```

## Arguments

- summaryTableForVocabularyAndDatabaseList:

  List of summary tables

- usagiTibble:

  Usagi tibble

- sourceVocabularyId:

  Source vocabulary ID

- outputFolderPath:

  Output folder path

- pathToNewsFile:

  Path to the NEWS.md file

- fileIssueRepo:

  The repository to file issues to

## Value

Path to the output HTML file
