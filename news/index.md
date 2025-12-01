# Changelog

## ROMOPMappingTools 2.1.2

- Added rule to validate usagi file: sourceConceptCode must be less than
  50 characters
- Fix bug in the DuckDB export: remove QUOTE ’’ from the export, this
  was adding extra characters to the export

## ROMOPMappingTools 2.1.1

- Fixed bug in the conversion of the OMOP vocabulary CSVs to DuckDB

## ROMOPMappingTools 2.1.0

- Fixed concept_ancestor table generation. ICD10fi hierarchy was not
  correct, and duplicated rows were present in the table.
- Added DRUG_STRENGTH table generation. This was missing, and created
  warnings in the DQD validation.
- Fixed errors due to DatabaseConnector v7.0.0.

## ROMOPMappingTools 2.0.5

- Fixed a bug in Usagi to STCM table conversion with respect to source
  parents
  - STCDM to CDM table conversion through SQL has flipped `Subsumes` and
    `Is a` relationships
  - Added the self-reference to the concepts in SQL that converts
    `concept_relationship` to `concept_ancestor`
- Updated test-databasesFromAndToCSV.R to ignore warnings in the DQD
  validation

## ROMOPMappingTools 2.0.4

- Added missing domainId combinations to the usagi file validation

## ROMOPMappingTools 2.0.3

- Added download button to vocabulary and database status tables
- Force `ADD_INFO:validationMessages` and `ADD_INFO:autoUpdatingInfo` to
  be NA if they are empty

## ROMOPMappingTools 2.0.2

- Fix updating reporting

## ROMOPMappingTools v2.0.1

- Refactored the Dashboard build process

## ROMOPMappingTools v2.0.0

- Code refactoring
- Code simplified by working on a duckdb database
- Added new functionality to update Usagi files after a vocabulary
  update

## ROMOPMappingTools v1.0.0

- Initial release
