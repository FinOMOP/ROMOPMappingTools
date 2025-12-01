# Read Usagi File

Reads a Usagi mapping file with appropriate column type specifications.
Warnings about missing columns are suppressed as the ADD_INFO columns
are optional.

## Usage

``` r
readUsagiFile(pathToUsagiFile)
```

## Arguments

- pathToUsagiFile:

  Path to the Usagi mapping file to read

## Value

A tibble containing the Usagi mapping data with appropriate column types

## Details

Required columns:

- sourceCode (character): Source vocabulary code

- sourceName (character): Description of the source code

- sourceFrequency (integer): Frequency of occurrence

- sourceAutoAssignedConceptIds (integer): Auto-assigned concept IDs

- matchScore (double): Score of the mapping match

- mappingStatus (character): Status of the mapping (APPROVED, UNCHECKED,
  etc.)

- equivalence (character): Type of equivalence

- statusSetBy (character): User who set the status

- statusSetOn (double): Timestamp when status was set

- conceptId (integer): Target concept ID

- conceptName (character): Name of target concept

- domainId (character): Domain of target concept

- mappingType (character): Type of mapping

- comment (character): Additional comments

- createdBy (character): User who created the mapping

- createdOn (double): Timestamp of creation

- assignedReviewer (character): Assigned reviewer

Optional ADD_INFO columns:

- sourceConceptId (double): Source concept ID

- sourceConceptClass (character): Class of source concept

- sourceDomain (character): Domain of source concept

- sourceValidStartDate (date): Start date of validity

- sourceValidEndDate (date): End date of validity

- sourceParents (character): Parent concepts

- sourceParentVocabulary (character): Vocabulary of parent concepts

- validationMessagess (character): Validation messages
