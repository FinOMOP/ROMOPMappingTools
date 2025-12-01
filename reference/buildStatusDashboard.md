# Build Status Dashboard

This function generates a status dashboard for mapping status.

## Usage

``` r
buildStatusDashboard(
  pathToCodeCountsFolder,
  pathToVocabularyFolder,
  connectionDetails,
  vocabularyDatabaseSchema,
  outputFolderPath = tempdir(),
  fileIssueRepo = ""
)
```

## Arguments

- pathToCodeCountsFolder:

  Path to folder containing code counts files

- pathToVocabularyFolder:

  Path to folder containing vocabulary files

- connectionDetails:

  DatabaseConnector connection details object

- vocabularyDatabaseSchema:

  Schema containing the vocabulary tables

- outputFolderPath:

  The path to the output folder for HTML output

- fileIssueRepo:

  The repository to file issues to

## Value

Path to the output HTML file
