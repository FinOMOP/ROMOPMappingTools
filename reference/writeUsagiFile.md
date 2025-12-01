# Write a Usagi mapping file

Write a Usagi mapping file to disk

## Usage

``` r
writeUsagiFile(usagiTibble, pathToUsagiFile)
```

## Arguments

- usagiTibble:

  A tibble containing the Usagi mapping data

- pathToUsagiFile:

  Path where the Usagi mapping file should be written

## Value

None

## Details

The function writes a Usagi mapping file with the following columns:

- sourceCode (character): Source code

- sourceName (character): Source name

- sourceFrequency (integer): Source frequency

- sourceAutoAssignedConceptIds (integer): Source auto-assigned concept
  IDs

- matchScore (double): Match score

- mappingStatus (character): Mapping status

- equivalence (character): Equivalence

- statusSetBy (character): Status set by

- statusSetOn (double): Status set on

- conceptId (integer): Concept ID

- conceptName (character): Concept name

- domainId (character): Domain ID

- mappingType (character): Mapping type

- comment (character): Comment

- createdBy (character): Created by

- createdOn (double): Created on

- assignedReviewer (character): Assigned reviewer

- ADD_INFO:sourceConceptId (double): Source concept ID

- ADD_INFO:sourceConceptClass (character): Source concept class

- ADD_INFO:sourceDomain (character): Source domain

- ADD_INFO:sourceValidStartDate (date): Source valid start date

- ADD_INFO:sourceValidEndDate (date): Source valid end date

- ADD_INFO:sourceParents (character): Source parents

- ADD_INFO:sourceParentVocabulary (character): Source parent vocabulary

- ADD_INFO:validationMessages (character): Validation messages

- ADD_INFO:autoUpdatingInfo (character): Auto updating info
