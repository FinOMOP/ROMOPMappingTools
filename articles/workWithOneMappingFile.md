# Working with individual mapping files

``` r
library(ROMOPMappingTools)
```

## Intro

This vignette shows how to use some of the functions of the
ROMOPMappingTools package to work with a single Usagi mapping file.
Reading and Usagi file, validating its format, or updating it after a
vocabulary update. For trasforming a single usagi file into C&CR tables
of the OMOP vocabulary, we recommend follow the same steps as in the
[Work with multiple mapping
files](https://finomop.github.io/ROMOPMappingTools/articles/workWithMultipleMappingFiles.md)
vignette. This is because the process need some other information that
is not included in the Usagi file, but n the ‘vocabularies.csv’ file.
For automating all the process in a github repository, please refer to
the [Work as a github
repository](https://finomop.github.io/ROMOPMappingTools/articles/workAsGithubRepo.md)
vignette.

Example files are included in the package. In the `inst/testdata` folder
you can find the files used in this example.

### Reading a Usagi file

For reading the Usagi file, we can use the `readUsagiFile` function,
which returns a tibble with the correct columns formated. It can read a
standard Usagi file or an extended Usagi file, see the [Usagi file
format](https://finomop.github.io/ROMOPMappingTools/articles/usagiFileFormat.md)
vignette for more details. In this example we will read a extended Usagi
file, from the test data. This file contains the mappings for the
ICD10fi vocabulary.

``` r
pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")

usagiTibble <- readUsagiFile(pathToUsagiFile)

usagiTibble |> dplyr::glimpse()
#> Rows: 3,945
#> Columns: 25
#> $ sourceCode                        <chr> "A01.0+G01", "A01.0+I39.8", "A01.0+J…
#> $ sourceName                        <chr> "Meningitis (in) typhoid fever", "En…
#> $ sourceFrequency                   <int> -1, -1, -1, -1, -1, -1, -1, -1, -1, …
#> $ sourceAutoAssignedConceptIds      <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `ADD_INFO:sourceConceptId`        <dbl> 2000500101, 2000500102, 2000500103, …
#> $ `ADD_INFO:sourceName_fi`          <chr> "Lavantautiin liittyvä aivokalvotule…
#> $ `ADD_INFO:sourceConceptClass`     <chr> "ICD10fi Hierarchy", "ICD10fi Hierar…
#> $ `ADD_INFO:sourceDomain`           <chr> "Condition", "Condition", "Condition…
#> $ `ADD_INFO:sourceValidStartDate`   <date> 1900-01-01, 1900-01-01, 1900-01-01,…
#> $ `ADD_INFO:sourceValidEndDate`     <date> 2099-12-31, 2099-12-31, 2099-12-31,…
#> $ `ADD_INFO:sourceParents`          <chr> "A01|A01.0|G01", "A01|A01.0|I39.8", …
#> $ `ADD_INFO:sourceParentVocabulary` <chr> "ICD10|ICD10|ICD10", "ICD10|ICD10|IC…
#> $ matchScore                        <dbl> 0.00, 0.00, 0.00, 0.78, 0.00, 0.00, …
#> $ mappingStatus                     <chr> "APPROVED", "APPROVED", "APPROVED", …
#> $ equivalence                       <chr> "EQUAL", "EQUAL", "EQUAL", "EQUIVALE…
#> $ statusSetBy                       <chr> "PKo", "PKo", "PKo", "PKo", "PKo", "…
#> $ statusSetOn                       <dbl> 1.666794e+12, 1.666794e+12, 1.666794…
#> $ conceptId                         <int> 4100102, 4111401, 4166072, 80316, 43…
#> $ conceptName                       <chr> "Meningitis due to typhoid fever", "…
#> $ domainId                          <chr> "Condition", "Condition", "Condition…
#> $ mappingType                       <chr> "MAPS_TO", "MAPS_TO", "MAPS_TO", "MA…
#> $ comment                           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ createdBy                         <chr> "TAYS", "TAYS", "TAYS", "PKo", "TAYS…
#> $ createdOn                         <dbl> 1.623974e+12, 1.623974e+12, 1.623974…
#> $ assignedReviewer                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
```

### Validating a Usagi file

To validate if all the information in the Usagi file is correct, we can
use the `validateUsagiFile` function. This function takes an Usagi or
Usagi-extended file and and performs a series of validations, see the
function help for more details
[`?validateUsagiFile`](https://finomop.github.io/ROMOPMappingTools/reference/validateUsagiFile.md).
The function also needs a connection to the OMOP vocabulary database and
schema to where the vocabulary tables are stored in order to make some
of the validations. The function also need the number used to offset the
source concept ids in the Usagi file. The function returns a tibble with
a summary of the validations conducted and if error are found, a new
Usagi file with the errors will be created in the specified path.

For this example, we will use the test database in DuckDB format
included in the package. This test database contains only the ICD10
vocabulary with all the keys in other tables (see
`inst/testdata/createTestData.R` for more details).

``` r
pathToOMOPVocabularyDuckDBfile <-  helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()

connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "duckdb",
    server = pathToOMOPVocabularyDuckDBfile
)

connection <- DatabaseConnector::connect(connectionDetails)
#> Connecting using DuckDB driver
vocabularyDatabaseSchema <- "main"

pathToValidatedUsagiFile  <- tempfile(fileext = "usagi_validated.csv")
```

``` r
validationsSummary <- validateUsagiFile(
  pathToUsagiFile,
  connection,
  vocabularyDatabaseSchema,
  pathToValidatedUsagiFile = pathToValidatedUsagiFile, 
  sourceConceptIdOffset = 2000500000
)
```

``` r
knitr::kable(validationsSummary)
```

| type    | step                                                     | message |
|:--------|:---------------------------------------------------------|:--------|
| SUCCESS | Missing default columns                                  |         |
| SUCCESS | SourceCode is empty                                      |         |
| SUCCESS | SourceCode and conceptId are not unique                  |         |
| SUCCESS | SourceCode is more than 50 characters                    |         |
| SUCCESS | SourceName is empty                                      |         |
| SUCCESS | SourceName is more than 255 characters                   |         |
| SUCCESS | SourceFrequency is not empty                             |         |
| SUCCESS | MappingStatus is empty                                   |         |
| SUCCESS | MappingStatus is not valid                               |         |
| SUCCESS | APPROVED mappingStatus conceptId is 0                    |         |
| SUCCESS | APPROVED mappingStatus with concepts outdated            |         |
| SUCCESS | Not APPROVED mappingStatus with concepts outdated        |         |
| SUCCESS | Missing C&CR columns                                     |         |
| SUCCESS | SourceConceptId is empty                                 |         |
| SUCCESS | SourceConceptId is not a number on the range             |         |
| SUCCESS | SourceConceptClass is empty                              |         |
| SUCCESS | SourceConceptClass is more than 20 characters            |         |
| SUCCESS | SourceDomain is empty                                    |         |
| SUCCESS | SourceDomain is not a valid domain                       |         |
| SUCCESS | Not APPROVED mappingStatus with valid domain combination |         |
| SUCCESS | APPROVED mappingStatus with valid domain combination     |         |
| SUCCESS | Missing date columns                                     |         |
| SUCCESS | SourceValidStartDate is after SourceValidEndDate         |         |
| SUCCESS | Missing parent columns                                   |         |
| SUCCESS | Invalid parent concept code                              |         |

In this case the Usagi file is valid and no errors are found. Hence, the
new validated Usagi file remains unchanged.

However, we can see what happens with a usagi file with errors. In this
case we use an other Usagi file with all type of errors, wich is
included in the package for unit testing purposes.

``` r
pathToUsagiFileWithErrors <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_with_errors.usagi.csv", package = "ROMOPMappingTools")

usagiTibbleWithErrors <- readUsagiFile(pathToUsagiFileWithErrors)
```

``` r
validationsSummaryWithErrors <- validateUsagiFile(
  pathToUsagiFileWithErrors,
  connection,
  vocabularyDatabaseSchema,
  pathToValidatedUsagiFile = pathToValidatedUsagiFile,
  sourceConceptIdOffset = 2000500000
)
```

``` r
knitr::kable(validationsSummaryWithErrors)
```

| type    | step                                                     | message                                                                                                                                                      |
|:--------|:---------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| SUCCESS | Missing default columns                                  |                                                                                                                                                              |
| ERROR   | SourceCode is empty                                      | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceCode and conceptId are not unique                  | Number of failed rules: 2                                                                                                                                    |
| ERROR   | SourceCode is more than 50 characters                    | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceName is empty                                      | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceName is more than 255 characters                   | Number of failed rules: 1                                                                                                                                    |
| SUCCESS | SourceFrequency is not empty                             |                                                                                                                                                              |
| SUCCESS | MappingStatus is empty                                   |                                                                                                                                                              |
| SUCCESS | MappingStatus is not valid                               |                                                                                                                                                              |
| ERROR   | APPROVED mappingStatus conceptId is 0                    | Number of failed rules: 1                                                                                                                                    |
| ERROR   | APPROVED mappingStatus with concepts outdated            | 3 conceptIds do not exist on the target vocabularies, 3 conceptNames are outdated, 3 domainIds are outdated, 8 standardConcepts have changed to non-standard |
| WARNING | Not APPROVED mappingStatus with concepts outdated        | 3 conceptIds do not exist on the target vocabularies, 1 conceptNames are outdated, 3 domainIds are outdated, 5 standardConcepts have changed to non-standard |
| SUCCESS | Missing C&CR columns                                     |                                                                                                                                                              |
| ERROR   | SourceConceptId is empty                                 | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceConceptId is not a number on the range             | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceConceptClass is empty                              | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceConceptClass is more than 20 characters            | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceDomain is empty                                    | Number of failed rules: 1                                                                                                                                    |
| ERROR   | SourceDomain is not a valid domain                       | Number of failed rules: 1                                                                                                                                    |
| WARNING | Not APPROVED mappingStatus with valid domain combination | Found 1 codes with invalid domain combinations                                                                                                               |
| ERROR   | APPROVED mappingStatus with valid domain combination     | Found 1 codes with invalid domain combinations                                                                                                               |
| SUCCESS | Missing date columns                                     |                                                                                                                                                              |
| ERROR   | SourceValidStartDate is after SourceValidEndDate         | Number of failed rules: 1                                                                                                                                    |
| SUCCESS | Missing parent columns                                   |                                                                                                                                                              |
| ERROR   | Invalid parent concept code                              | Found 3 codes with invalid parent concept codes                                                                                                              |

In this case, if we open the new validate Usagi with the Usagi software
these mapping with errors will appear as FLAGGED. Additionally, the
`ADD_INFO:validationMessages` column will indicate the exact error or
errors found.

![Usagi with errors](./img/Usagi_with_errors.png)

Usagi with errors

### Updating a Usagi file

If the vocabulary has been updated since the Usagi file was created, it
may happen that some of the mappings are outdated. This will be detected
by the `validateUsagiFile` and show as a “ConceptIds outdated” error.

In this case we will use an other Usagi file with outdated concept ids,
which is included in the package for unit testing purposes.

``` r
pathToOutdatedUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_outdated.usagi.csv", package = "ROMOPMappingTools")

validationsSummaryWithErrors <- validateUsagiFile(
  pathToOutdatedUsagiFile,
  connection,
  vocabularyDatabaseSchema,
  pathToValidatedUsagiFile = pathToValidatedUsagiFile,
  sourceConceptIdOffset = 2000500000
)

knitr::kable(validationsSummaryWithErrors)
```

| type    | step                                                     | message                                                                                                    |
|:--------|:---------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------|
| SUCCESS | Missing default columns                                  |                                                                                                            |
| SUCCESS | SourceCode is empty                                      |                                                                                                            |
| SUCCESS | SourceCode and conceptId are not unique                  |                                                                                                            |
| SUCCESS | SourceCode is more than 50 characters                    |                                                                                                            |
| SUCCESS | SourceName is empty                                      |                                                                                                            |
| SUCCESS | SourceName is more than 255 characters                   |                                                                                                            |
| SUCCESS | SourceFrequency is not empty                             |                                                                                                            |
| SUCCESS | MappingStatus is empty                                   |                                                                                                            |
| SUCCESS | MappingStatus is not valid                               |                                                                                                            |
| SUCCESS | APPROVED mappingStatus conceptId is 0                    |                                                                                                            |
| ERROR   | APPROVED mappingStatus with concepts outdated            | 122 conceptNames are outdated, 92 domainIds are outdated, 24 standardConcepts have changed to non-standard |
| SUCCESS | Not APPROVED mappingStatus with concepts outdated        |                                                                                                            |
| SUCCESS | Missing C&CR columns                                     |                                                                                                            |
| SUCCESS | SourceConceptId is empty                                 |                                                                                                            |
| SUCCESS | SourceConceptId is not a number on the range             |                                                                                                            |
| SUCCESS | SourceConceptClass is empty                              |                                                                                                            |
| SUCCESS | SourceConceptClass is more than 20 characters            |                                                                                                            |
| SUCCESS | SourceDomain is empty                                    |                                                                                                            |
| SUCCESS | SourceDomain is not a valid domain                       |                                                                                                            |
| SUCCESS | Not APPROVED mappingStatus with valid domain combination |                                                                                                            |
| SUCCESS | APPROVED mappingStatus with valid domain combination     |                                                                                                            |
| SUCCESS | Missing date columns                                     |                                                                                                            |
| SUCCESS | SourceValidStartDate is after SourceValidEndDate         |                                                                                                            |
| SUCCESS | Missing parent columns                                   |                                                                                                            |
| SUCCESS | Invalid parent concept code                              |                                                                                                            |

If outdated error are detected, we can attempt to update the Usagi file
automatically using the `updateUsagiFile` function. This function takes
an Usagi or Usagi-extended file, a connection to the database, the
schema with the vocabulary tables and a path to a file where to store
the updated Usagi file.

``` r
pathToUpdatedUsagiFile <- tempfile(fileext = "usagi_updated.csv")

updateSummary <- updateUsagiFile(
    pathToOutdatedUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToUpdatedUsagiFile,
    skipValidation = TRUE
  )
#> Note: method with signature 'DBIConnection#SQL' chosen for function 'dbQuoteIdentifier',
#>  target signature 'DatabaseConnectorDbiConnection#SQL'.
#>  "DatabaseConnectorConnection#character" would also be valid

knitr::kable(updateSummary)
```

| type    | step                  | message                                                            |
|:--------|:----------------------|:-------------------------------------------------------------------|
| INFO    | Updated conceptIds    | Updated 20 conceptIds that don’t need review                       |
| WARNING | Updated conceptIds    | 29 conceptIds could not be updated automatically, remapping needed |
| INFO    | Updated domains       | Updated 38 domains                                                 |
| INFO    | Updated concept names | Updated 100 concept names                                          |

This fuction updates changes in `domain_id`, `concept_name` and if the
mapped `concept_id` point to a non-standard concept it will try to find
a new mapping for it (This is done by looking at the relationship table
for relationships of the old concept_id by “Maps to”, “Concept replaced
by”, “Concept same_as to” and “Concept poss_eq to” in that order). A new
column `ADD_INFO:autoUpdatingInfo` is added to the updated Usagi file to
show the specific changes made to the file.

Some times, like in this case, a new concept_id can not be found, this
is shown as a warning.

The new updates Usagi file can be validated again with the
`validateUsagiFile` function to check if there are any errors.

``` r
validationsSummaryWithErrors <- validateUsagiFile(
  pathToUpdatedUsagiFile,
  connection,
  vocabularyDatabaseSchema,
  pathToValidatedUsagiFile = pathToValidatedUsagiFile,
  sourceConceptIdOffset = 2000500000
)

knitr::kable(validationsSummaryWithErrors)
```

| type    | step                                                     | message                                        |
|:--------|:---------------------------------------------------------|:-----------------------------------------------|
| SUCCESS | Missing default columns                                  |                                                |
| SUCCESS | SourceCode is empty                                      |                                                |
| SUCCESS | SourceCode and conceptId are not unique                  |                                                |
| SUCCESS | SourceCode is more than 50 characters                    |                                                |
| SUCCESS | SourceName is empty                                      |                                                |
| SUCCESS | SourceName is more than 255 characters                   |                                                |
| SUCCESS | SourceFrequency is not empty                             |                                                |
| SUCCESS | MappingStatus is empty                                   |                                                |
| SUCCESS | MappingStatus is not valid                               |                                                |
| SUCCESS | APPROVED mappingStatus conceptId is 0                    |                                                |
| SUCCESS | APPROVED mappingStatus with concepts outdated            |                                                |
| SUCCESS | Not APPROVED mappingStatus with concepts outdated        |                                                |
| SUCCESS | Missing C&CR columns                                     |                                                |
| SUCCESS | SourceConceptId is empty                                 |                                                |
| SUCCESS | SourceConceptId is not a number on the range             |                                                |
| SUCCESS | SourceConceptClass is empty                              |                                                |
| SUCCESS | SourceConceptClass is more than 20 characters            |                                                |
| SUCCESS | SourceDomain is empty                                    |                                                |
| SUCCESS | SourceDomain is not a valid domain                       |                                                |
| SUCCESS | Not APPROVED mappingStatus with valid domain combination |                                                |
| ERROR   | APPROVED mappingStatus with valid domain combination     | Found 6 codes with invalid domain combinations |
| SUCCESS | Missing date columns                                     |                                                |
| SUCCESS | SourceValidStartDate is after SourceValidEndDate         |                                                |
| SUCCESS | Missing parent columns                                   |                                                |
| SUCCESS | Invalid parent concept code                              |                                                |

Unfortunatelly, sometimes the updateUsagiFile is introducing new errors,
in this case updates in the vocabulary have introduced invalid domain
combinations. Moreover, some of the mappings could not be updated
because the new concept_id was not found. This need to be fixed by the
user by reviewing the Usagi file.
