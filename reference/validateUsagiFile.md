# Validate Usagi File

Reads a usagi file given in 'pathToUsagiFile' and performs the following
checks: Default Usagi columns:

- Check if all default Usagi columns are present:

- Check if sourceCode and conceptId are unique

- Check if sourceCode is not empty

- Check if sourceName is not empty

- Check if sourceName is less than 255 characters If usagi file has C&CR
  columns:

- Check if concept_id is not 0 for APPROVED mappingStatus

- Check codes with mapping to more than one domain are mapped to
  compatible domains

- Check if sourceValidStartDate is before sourceValidEndDate

- Check if ADD_INFO:sourceParents is a valid concept code in the
  ADD_INFO:sourceParentVocabulary

## Usage

``` r
validateUsagiFile(
  pathToUsagiFile,
  connection,
  vocabularyDatabaseSchema,
  pathToValidatedUsagiFile,
  sourceConceptIdOffset
)
```

## Arguments

- pathToUsagiFile:

  Path to the Usagi mapping file to validate

- connection:

  A DatabaseConnector connection object

- vocabularyDatabaseSchema:

  Schema name where the vocabulary tables are stored

- pathToValidatedUsagiFile:

  Path where to save the validated Usagi file

- sourceConceptIdOffset:

  Integer offset to add to source concept IDs. Default is 0
