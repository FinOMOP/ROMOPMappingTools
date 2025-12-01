# Update Vocabulary Folder

Updates all Usagi files in a vocabulary folder with the latest concept
information. The folder must contain a vocabularies.csv file that
describes the vocabularies and their corresponding Usagi files.

## Usage

``` r
updateVocabularyFolder(
  pathToVocabularyFolder,
  connection,
  vocabularyDatabaseSchema,
  updateResultsFolder,
  skipValidation = TRUE
)
```

## Arguments

- pathToVocabularyFolder:

  Path to folder containing vocabulary files

- connection:

  A DatabaseConnector connection object

- vocabularyDatabaseSchema:

  Schema name where the vocabulary tables are stored

- updateResultsFolder:

  Folder where updated files will be saved

- skipValidation:

  Whether to skip validation checks if TRUE. Default is TRUE

## Value

A tibble containing update results for all files
