# Run All Validation and Upload Steps

Runs the complete workflow of validating vocabulary files and uploading
them to CDM tables. It performs the following steps:

1.  Validate the vocabulary folder

2.  If sourceToConceptMapTable is not NULL, create the
    SourceToConceptMap table

3.  Upload the vocabulary.csv and Usagi files to the SourceToConceptMap
    table

4.  Move the SourceToConceptMap table to the CDM table

5.  Validate the CDM tables with DQD

## Usage

``` r
buildVocabulariesAll(
  pathToVocabularyFolder,
  connectionDetails,
  vocabularyDatabaseSchema,
  validationResultsFolder,
  sourceToConceptMapTable = NULL
)
```

## Arguments

- pathToVocabularyFolder:

  Path to folder containing vocabulary files

- connectionDetails:

  DatabaseConnector connection details object

- vocabularyDatabaseSchema:

  Schema containing the vocabulary tables

- validationResultsFolder:

  Folder where validation results will be saved

- sourceToConceptMapTable:

  Optional name of source to concept map table
