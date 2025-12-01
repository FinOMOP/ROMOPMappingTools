# Upload Vocabulary Folder to CDM Tables

Uploads all vocabulary files from a folder to the CDM tables. This
includes:

- Adding vocabularies to the VOCABULARY table

- Adding concept classes to the CONCEPT_CLASS table

- Adding mappings to the Source to Concept Map table

- Creating corresponding CONCEPT entries

## Usage

``` r
vocabularyFolderToSTCMVocabularyConcepClassTables(
  pathToVocabularyFolder,
  connection,
  vocabularyDatabaseSchema,
  sourceToConceptMapTable,
  skipValidation = TRUE,
  pathToValidatedUsagiFolder = NULL
)
```

## Arguments

- pathToVocabularyFolder:

  Path to folder containing vocabulary files

- connection:

  A DatabaseConnector connection object

- vocabularyDatabaseSchema:

  Schema name where the vocabulary tables are stored

- sourceToConceptMapTable:

  Name of the source to concept map table

- skipValidation:

  Whether to skip validation of the Usagi files (default: TRUE)

- pathToValidatedUsagiFolder:

  Path where validated Usagi files will be saved if validation is
  performed

## Value

NULL invisibly. The function modifies the database tables directly.

## Details

The vocabulary folder must contain:

- vocabularies.csv: A file describing the vocabularies with columns:

  - source_vocabulary_id

  - source_vocabulary_name

  - source_concept_id_offset

  - path_to_usagi_file

  - path_to_news_file

  - ignore

- Usagi mapping files as referenced in vocabularies.csv

- NEWS files containing version information as referenced in
  vocabularies.csv
