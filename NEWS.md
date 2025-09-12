# ROMOPMappingTools 2.0.5

- Fixed a bug in Usagi to STCM table conversion with respect to source parents
- STCDM to CDM table conversion through SQL has flipped `Subsumes` and `Is a` relationships
- Added the self-reference to the concepts in SQL that converts `concept_relationship` to `concept_ancestor`

# ROMOPMappingTools 2.0.4

- Added missing domainId combinations to the usagi file validation

# ROMOPMappingTools 2.0.3

- Added download button to vocabulary and database status tables
- Force `ADD_INFO:validationMessages` and `ADD_INFO:autoUpdatingInfo` to be NA if they are empty

# ROMOPMappingTools 2.0.2

- Fix updating reporting

# ROMOPMappingTools v2.0.1

- Refactored the Dashboard build process

# ROMOPMappingTools v2.0.0

- Code refactoring
- Code simplified by working on a duckdb database
- Added new functionality to update Usagi files after a vocabulary update

# ROMOPMappingTools v1.0.0

- Initial release
