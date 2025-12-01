# Step by step example using individual functions

``` r
library(ROMOPMappingTools)
```

## Intro

This vignette shows how to use some of the functions of the
ROMOPMappingTools package to work with a multiple Usagi mapping files.
Validating their format, updating them after a vocabulary update,
uploading them to the STCM table, and transforming the STCM table to the
CDM tables.

For automating all the process in a github repository, please refer to
the [Work as a github
repository](https://finomop.github.io/ROMOPMappingTools/articles/workAsGithubRepo.md)
vignette. For working with a single Usagi file, please refer to the
[Work with individual mapping
files](https://finomop.github.io/ROMOPMappingTools/articles/workWithOneMappingFile.md)
vignette.

Example files are included in the package. In the `inst/testdata` folder
you can find the files used in this example.

### Setting up the forders structure

To work with multiple Usagi files, we need to have a folder with the
Usagi files and a `vocabularies.csv` file.

The `vocabularies.csv` file is a file that contains the vocabulary
information and the path to the Usagi and NEWS.md files asociated with
each vocabulary, see the [Tables description and
rules](https://finomop.github.io/ROMOPMappingTools/articles/usagiFileFormat.md)
vignette for more details.

We recommend to have a folder structure like the one use in this
example:

    inst/testdata/VOCABULARIES/
    ├── vocabularies.csv
    ├── ICD10fi/
    │   ├── ICD10fi.usagi.csv
    │   └── NEWS.md
    └── UNITfi/
        ├── UNITfi.usagi.csv
        └── NEWS.md

We use a root folder containing the `vocabularies.csv` and
subdirectories named after the vocabulary id. Each subdirectory contains
the Usagi and NEWS.md files. In this case we have two vocabularies:
ICD10fi and UNITfi.

### Target database

For this example we will use the test database in DuckDB format included
in the package. Create a new database by making a copy of the
`inst/testdata/OMOPVocabulary.duckdb` file.

``` r
pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "duckdb",
  server = pathToOMOPVocabularyDuckDBfile
)

connection <- DatabaseConnector::connect(connectionDetails)
#> Connecting using DuckDB driver
#> Warning in connectDuckdb(connectionDetails): The ICU extension of DuckDB is not
#> installed. Attempting to install it.
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.426 secs
vocabularyDatabaseSchema <- "main"
```

### Validate all the Usagi files in the vocabulary folder

This function nees as an input a path to a folder with a
`vocabularies.csv` file, a connection to the database, the schema with
the vocabulary tables and a folder where to store the validated Usagi
files if they are not valid. This function will validate the format of
the `vocabularies.csv` file and all the Usagi files. Details on the
Usagi file validations are described in the [Work with individual
mapping
files](https://finomop.github.io/ROMOPMappingTools/articles/workWithOneMappingFile.md)
vignette.

``` r
pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
pathToValidatedUsagiFolder <- tempdir()

validationsLogTibble <- validateVocabularyFolder(
  pathToVocabularyFolder,
  connection,
  vocabularyDatabaseSchema,
  pathToValidatedUsagiFolder
)
#> Validating Usagi file ./ICD10fi/ICD10fi.usagi.csv
#> Validating Usagi file ./UNITfi/UNITfi.usagi.csv
```

The function will return a tibble with the validations on the
`vocabularies.csv` file and all the Usagi files.

``` r
knitr::kable(validationsLogTibble)
```

| context        | type    | step                                                     | message |
|:---------------|:--------|:---------------------------------------------------------|:--------|
| vocabulary.csv | SUCCESS | source_vocabulary_id is not empty                        |         |
| vocabulary.csv | SUCCESS | source_vocabulary_id is less than 20 characters          |         |
| vocabulary.csv | SUCCESS | source_vocabulary_name is not empty                      |         |
| vocabulary.csv | SUCCESS | source_vocabulary_name is less than 255 characters       |         |
| vocabulary.csv | SUCCESS | source_concept_id_offset is a number over 2 billion      |         |
| vocabulary.csv | SUCCESS | source_concept_id_offset is unique                       |         |
| ICD10fi        | SUCCESS | Missing default columns                                  |         |
| ICD10fi        | SUCCESS | SourceCode is empty                                      |         |
| ICD10fi        | SUCCESS | SourceCode and conceptId are not unique                  |         |
| ICD10fi        | SUCCESS | SourceCode is more than 50 characters                    |         |
| ICD10fi        | SUCCESS | SourceName is empty                                      |         |
| ICD10fi        | SUCCESS | SourceName is more than 255 characters                   |         |
| ICD10fi        | SUCCESS | SourceFrequency is not empty                             |         |
| ICD10fi        | SUCCESS | MappingStatus is empty                                   |         |
| ICD10fi        | SUCCESS | MappingStatus is not valid                               |         |
| ICD10fi        | SUCCESS | APPROVED mappingStatus conceptId is 0                    |         |
| ICD10fi        | SUCCESS | APPROVED mappingStatus with concepts outdated            |         |
| ICD10fi        | SUCCESS | Not APPROVED mappingStatus with concepts outdated        |         |
| ICD10fi        | SUCCESS | Missing C&CR columns                                     |         |
| ICD10fi        | SUCCESS | SourceConceptId is empty                                 |         |
| ICD10fi        | SUCCESS | SourceConceptId is not a number on the range             |         |
| ICD10fi        | SUCCESS | SourceConceptClass is empty                              |         |
| ICD10fi        | SUCCESS | SourceConceptClass is more than 20 characters            |         |
| ICD10fi        | SUCCESS | SourceDomain is empty                                    |         |
| ICD10fi        | SUCCESS | SourceDomain is not a valid domain                       |         |
| ICD10fi        | SUCCESS | Not APPROVED mappingStatus with valid domain combination |         |
| ICD10fi        | SUCCESS | APPROVED mappingStatus with valid domain combination     |         |
| ICD10fi        | SUCCESS | Missing date columns                                     |         |
| ICD10fi        | SUCCESS | SourceValidStartDate is after SourceValidEndDate         |         |
| ICD10fi        | SUCCESS | Missing parent columns                                   |         |
| ICD10fi        | SUCCESS | Invalid parent concept code                              |         |
| UNITfi         | SUCCESS | Missing default columns                                  |         |
| UNITfi         | SUCCESS | SourceCode is empty                                      |         |
| UNITfi         | SUCCESS | SourceCode and conceptId are not unique                  |         |
| UNITfi         | SUCCESS | SourceCode is more than 50 characters                    |         |
| UNITfi         | SUCCESS | SourceName is empty                                      |         |
| UNITfi         | SUCCESS | SourceName is more than 255 characters                   |         |
| UNITfi         | SUCCESS | SourceFrequency is not empty                             |         |
| UNITfi         | SUCCESS | MappingStatus is empty                                   |         |
| UNITfi         | SUCCESS | MappingStatus is not valid                               |         |
| UNITfi         | SUCCESS | APPROVED mappingStatus conceptId is 0                    |         |
| UNITfi         | SUCCESS | APPROVED mappingStatus with concepts outdated            |         |
| UNITfi         | SUCCESS | Not APPROVED mappingStatus with concepts outdated        |         |
| UNITfi         | SUCCESS | Missing C&CR columns                                     |         |
| UNITfi         | SUCCESS | SourceConceptId is empty                                 |         |
| UNITfi         | SUCCESS | SourceConceptId is not a number on the range             |         |
| UNITfi         | SUCCESS | SourceConceptClass is empty                              |         |
| UNITfi         | SUCCESS | SourceConceptClass is more than 20 characters            |         |
| UNITfi         | SUCCESS | SourceDomain is empty                                    |         |
| UNITfi         | SUCCESS | SourceDomain is not a valid domain                       |         |
| UNITfi         | SUCCESS | Not APPROVED mappingStatus with valid domain combination |         |
| UNITfi         | SUCCESS | APPROVED mappingStatus with valid domain combination     |         |
| UNITfi         | SUCCESS | Missing date columns                                     |         |
| UNITfi         | SUCCESS | SourceValidStartDate is after SourceValidEndDate         |         |
| UNITfi         | SUCCESS | Missing parent columns                                   |         |
| UNITfi         | SUCCESS | Invalid parent concept code                              |         |

### Upload all the Usagi files to the STCM table

If all the validations pass, we can use the
`vocabularyFolderToSTCMAndVocabularyTables` function to upload all the
Usagi files to the STCM table. If some of the validations fail, we
recoment fix the errors following the [Work with individual mapping
files](https://finomop.github.io/ROMOPMappingTools/articles/workWithOneMappingFile.md)
vignette.

If we are using the Usagi-extended format, we also need to create the
source_to_concept_map_extended table, see the [Files
format](https://finomop.github.io/ROMOPMappingTools/articles/filesFormat.md)
vignette for more details.

``` r
sourceToConceptMapTable <- "source_to_concept_map_extended"
createSourceToConceptMapExtended(connection, vocabularyDatabaseSchema, sourceToConceptMapTable)
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0101 secs
```

`vocabularyFolderToSTCMVocabularyConcepClassTables` needs the path to
the vocabulary folder, a connection to the database, the schema with the
vocabulary tables and the name of the SourceToConceptMap table.

``` r
vocabularyFolderToSTCMVocabularyConcepClassTables(
  pathToVocabularyFolder,
  connection,
  vocabularyDatabaseSchema,
  sourceToConceptMapTable
)
#> Appending vocabularies.csv to VOCABULARY table
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0071 secs
#> Inserting data took 0.0137 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00647 secs
#> Inserting data took 0.0393 secs
#> Appending info from Usagi files to Concept Class table
#> 
#> Collecting concept classes from Usagi file ./ICD10fi/ICD10fi.usagi.csv
#> 
#> Collecting concept classes from Usagi file ./UNITfi/UNITfi.usagi.csv
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00424 secs
#> Inserting data took 0.00961 secs
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00417 secs
#> Inserting data took 0.0101 secs
#> Appending Usagi files to STCM table
#> 
#> Appending Usagi file ./ICD10fi/ICD10fi.usagi.csv
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00417 secs
#> Inserting data took 0.016 secs
#> Appending Usagi file ./UNITfi/UNITfi.usagi.csv
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00386 secs
#> Inserting data took 0.0116 secs
```

This function will populate the VOCABULARY table with the
`vocabularies.csv` file and the source_to_concept_map_extended table
with the Usagi-extended files.

``` r
dplyr::tbl(connection, "VOCABULARY") |>
  dplyr::collect()
#> # A tibble: 15 × 5
#>    vocabulary_id    vocabulary_name      vocabulary_reference vocabulary_version
#>    <chr>            <chr>                <chr>                <chr>             
#>  1 Domain           OMOP Domain          "OMOP generated"     NA                
#>  2 Vocabulary       OMOP Vocabulary      "OMOP generated"     NA                
#>  3 Concept Class    OMOP Concept Class   "OMOP generated"     NA                
#>  4 Relationship     OMOP Relationship    "OMOP generated"     NA                
#>  5 OMOP Extension   OMOP Extension (OHD… "OMOP generated"     OMOP Extension 20…
#>  6 Nebraska Lexicon Nebraska Lexicon (U… "https://www.unmc.e… Nebraska Lexicon …
#>  7 ICDO3            International Class… "https://seer.cance… ICDO3 SEER Site/H…
#>  8 ICD10            International Class… "http://www.who.int… 2021 Release      
#>  9 LOINC            Logical Observation… "http://loinc.org/d… LOINC 2.77        
#> 10 Cancer Modifier  Diagnostic Modifier… "OMOP generated"     Cancer Modifier 2…
#> 11 SNOMED           Systematic Nomencla… "http://www.nlm.nih… 2024-02-01 SNOMED…
#> 12 None             OMOP Standardized V… "OMOP generated"     v5.0 30-AUG-24    
#> 13 UCUM             Unified Code for Un… "http://aurora.rege… Version 1.8.2     
#> 14 ICD10fi          International Class… ""                   v1.1.2            
#> 15 UNITfi           Finnish Units vocab… ""                   v1.0.1            
#> # ℹ 1 more variable: vocabulary_concept_id <int>
```

We can see on the botton that the ICD10fi and UNITfi vocabularies have
been added to the VOCABULARY table.

``` r
dplyr::tbl(connection, "source_to_concept_map_extended") |>
  dplyr::collect()
#> # A tibble: 4,122 × 12
#>    source_code source_concept_id source_vocabulary_id source_code_description   
#>    <chr>                   <int> <chr>                <chr>                     
#>  1 A01.0+G01          2000500101 ICD10fi              Meningitis (in) typhoid f…
#>  2 A01.0+I39.8        2000500102 ICD10fi              Endocarditis in typhoid f…
#>  3 A01.0+J17.0        2000500103 ICD10fi              Typhoid fever pneumonia   
#>  4 A01.4+M01.3        2000500104 ICD10fi              Arthritis in typhoid or p…
#>  5 A02.2+G01          2000500105 ICD10fi              Salmonella meningitis     
#>  6 A02.2+H22.0        2000500106 ICD10fi              Salmonella iridocyclitis  
#>  7 A02.2+H22.0        2000500106 ICD10fi              Salmonella iridocyclitis  
#>  8 A02.2+J17.0        2000500107 ICD10fi              Salmonella pneumonia      
#>  9 A02.2+M01.3        2000500108 ICD10fi              Salmonella arthritis      
#> 10 A02.2+M90.2        2000500109 ICD10fi              Salmonella osteomyelitis  
#> # ℹ 4,112 more rows
#> # ℹ 8 more variables: target_concept_id <int>, target_vocabulary_id <chr>,
#> #   valid_start_date <date>, valid_end_date <date>, invalid_reason <chr>,
#> #   source_concept_class <chr>, source_domain <chr>,
#> #   source_parents_concept_ids <chr>
```

We can see the source_to_concept_map_extended table has been populated
with the Usagi-extended files.

### Copying the STCM table to the CDM tables

The STCM table can be copied to the CDM tables using the `STCMToCDM`
function. This function needs a connection to the database and schema
where the vocabulary tables are stored, the schema with the vocabulary
tables and the name of the SourceToConceptMap table.

This function solely call to the SQL code in the
`inst/sql/STCMToCDM.sql` file, hence it be also used outside the
package. This SQL code can be translated to any other database supported
by DatabaseConnector, and be applied directly.

``` r
STCMToCDMTables(connection, vocabularyDatabaseSchema, sourceToConceptMapTable)
#>   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0529 secs
```

This populates the CONCEPT table:

``` r
dplyr::tbl(connection, "CONCEPT") |>
  dplyr::filter(vocabulary_id == "ICD10fi") |>
  dplyr::collect()
#> Note: method with signature 'DBIConnection#SQL' chosen for function 'dbQuoteIdentifier',
#>  target signature 'DatabaseConnectorDbiConnection#SQL'.
#>  "DatabaseConnectorConnection#character" would also be valid
#> # A tibble: 3,505 × 10
#>    concept_id concept_name              domain_id vocabulary_id concept_class_id
#>         <int> <chr>                     <chr>     <chr>         <chr>           
#>  1 2000500101 Meningitis (in) typhoid … Condition ICD10fi       ICD10fi Hierarc…
#>  2 2000500102 Endocarditis in typhoid … Condition ICD10fi       ICD10fi Hierarc…
#>  3 2000500103 Typhoid fever pneumonia   Condition ICD10fi       ICD10fi Hierarc…
#>  4 2000500104 Arthritis in typhoid or … Condition ICD10fi       ICD10fi Hierarc…
#>  5 2000500105 Salmonella meningitis     Condition ICD10fi       ICD10fi Hierarc…
#>  6 2000500106 Salmonella iridocyclitis  Condition ICD10fi       ICD10fi Hierarc…
#>  7 2000500107 Salmonella pneumonia      Condition ICD10fi       ICD10fi Hierarc…
#>  8 2000500108 Salmonella arthritis      Condition ICD10fi       ICD10fi Hierarc…
#>  9 2000500109 Salmonella osteomyelitis  Condition ICD10fi       ICD10fi Hierarc…
#> 10 2000500110 Salmonella renal tubulo-… Conditio… ICD10fi       ICD10fi Hierarc…
#> # ℹ 3,495 more rows
#> # ℹ 5 more variables: standard_concept <chr>, concept_code <chr>,
#> #   valid_start_date <date>, valid_end_date <date>, invalid_reason <chr>
```

The CONCEPT_RELATIONSHIP table with

- the ‘Maps to’ relationships:

``` r
dplyr::tbl(connection, "CONCEPT_RELATIONSHIP") |>
  dplyr::filter(relationship_id == "Maps to") |>
  dplyr::filter(concept_id_1 > 2000500101) |>
  dplyr::collect()
#> # A tibble: 4,089 × 6
#>    concept_id_1 concept_id_2 relationship_id valid_start_date valid_end_date
#>           <int>        <int> <chr>           <date>           <date>        
#>  1   2000500104        80316 Maps to         1900-01-01       2099-12-31    
#>  2   2000500111     44811663 Maps to         1900-01-01       2099-12-31    
#>  3   2000500116       440937 Maps to         1900-01-01       2099-12-31    
#>  4   2000500119       434557 Maps to         1900-01-01       2099-12-31    
#>  5   2000500120       434557 Maps to         1900-01-01       2099-12-31    
#>  6   2000500123        81496 Maps to         1900-01-01       2099-12-31    
#>  7   2000500125        80626 Maps to         1900-01-01       2099-12-31    
#>  8   2000500132      4056331 Maps to         1900-01-01       2099-12-31    
#>  9   2000500142       201779 Maps to         1900-01-01       2099-12-31    
#> 10   2000500144       141777 Maps to         1900-01-01       2099-12-31    
#> # ℹ 4,079 more rows
#> # ℹ 1 more variable: invalid_reason <chr>
```

- the ‘Maps from’ relationships:

``` r
dplyr::tbl(connection, "CONCEPT_RELATIONSHIP") |>
  dplyr::filter(relationship_id == "Mapped from") |>
  dplyr::filter(concept_id_2 > 2000500101) |>
  dplyr::collect()
#> # A tibble: 4,089 × 6
#>    concept_id_1 concept_id_2 relationship_id valid_start_date valid_end_date
#>           <int>        <int> <chr>           <date>           <date>        
#>  1      4111401   2000500102 Mapped from     1900-01-01       2099-12-31    
#>  2      4166072   2000500103 Mapped from     1900-01-01       2099-12-31    
#>  3        80316   2000500108 Mapped from     1900-01-01       2099-12-31    
#>  4      4329572   2000500110 Mapped from     1900-01-01       2099-12-31    
#>  5       440937   2000500116 Mapped from     1900-01-01       2099-12-31    
#>  6      4150516   2000500117 Mapped from     1900-01-01       2099-12-31    
#>  7      4098475   2000500122 Mapped from     1900-01-01       2099-12-31    
#>  8        81496   2000500123 Mapped from     1900-01-01       2099-12-31    
#>  9     36713392   2000500133 Mapped from     1900-01-01       2099-12-31    
#> 10       194268   2000500134 Mapped from     1900-01-01       2099-12-31    
#> # ℹ 4,079 more rows
#> # ℹ 1 more variable: invalid_reason <chr>
```

And if the columns `sourceConceptCode` and `sourceConceptVocabularyId`
are present in the STCM table, they will be used to populate the
CONCEPT_RELATIONSHIP table with

- the ‘Is a’ relationships:

``` r
dplyr::tbl(connection, "CONCEPT_RELATIONSHIP") |>
  dplyr::filter(relationship_id == "Is a") |>
  dplyr::filter(concept_id_1 > 2000500101) |>
  dplyr::collect()
#> # A tibble: 5,353 × 6
#>    concept_id_1 concept_id_2 relationship_id valid_start_date valid_end_date
#>           <int>        <int> <chr>           <date>           <date>        
#>  1   2000500263     45600423 Is a            1900-01-01       2099-12-31    
#>  2   2000500264     45552189 Is a            1900-01-01       2099-12-31    
#>  3   2000500265     45552189 Is a            1900-01-01       2099-12-31    
#>  4   2000500277     45547422 Is a            1900-01-01       2099-12-31    
#>  5   2000500279     45547422 Is a            1900-01-01       2099-12-31    
#>  6   2000500281     45581128 Is a            1900-01-01       2099-12-31    
#>  7   2000500283     45581128 Is a            1900-01-01       2099-12-31    
#>  8   2000500288     45595594 Is a            1900-01-01       2099-12-31    
#>  9   2000500294     45595594 Is a            1900-01-01       2099-12-31    
#> 10   2000500296     45595594 Is a            1900-01-01       2099-12-31    
#> # ℹ 5,343 more rows
#> # ℹ 1 more variable: invalid_reason <chr>
```

- the `Subsumes` relationships:

``` r
dplyr::tbl(connection, "CONCEPT_RELATIONSHIP") |>
  dplyr::filter(relationship_id == "Subsumes") |>
  dplyr::filter(concept_id_1 > 2000500101) |>
  dplyr::collect()
#> # A tibble: 21 × 6
#>    concept_id_1 concept_id_2 relationship_id valid_start_date valid_end_date
#>           <int>        <int> <chr>           <date>           <date>        
#>  1   2000503725   2000503728 Subsumes        1997-08-24       2099-12-31    
#>  2   2000503769   2000503739 Subsumes        1900-01-01       2099-12-31    
#>  3   2000503769   2000503742 Subsumes        1900-01-01       2099-12-31    
#>  4   2000503769   2000503755 Subsumes        1900-01-01       2099-12-31    
#>  5   2000500469   2000500471 Subsumes        1900-01-01       2099-12-31    
#>  6   2000503725   2000503729 Subsumes        2010-01-01       2099-12-31    
#>  7   2000503769   2000503737 Subsumes        1900-01-01       2099-12-31    
#>  8   2000503769   2000503748 Subsumes        1900-01-01       2099-12-31    
#>  9   2000500469   2000500470 Subsumes        1900-01-01       2099-12-31    
#> 10   2000503725   2000503727 Subsumes        2010-01-01       2099-12-31    
#> # ℹ 11 more rows
#> # ℹ 1 more variable: invalid_reason <chr>
```

### Populating the CONCEPT_ANCESTOR table

Since we have added the “Is a” and “Subsumes” relationships to the
CONCEPT_RELATIONSHIP table, we can use this information to populate the
CONCEPT_ANCESTOR table. This is done with the
`conceptRelationshipToAncestorTables` function.

This can be applied to any non-standard vocabulary, not only the ones
included in the `vocabularies.csv` file.

``` r
conceptRelationshipToAncestorTables(
  connection,
  vocabularyDatabaseSchema,
  vocabularyList = c("ICD10", "ICD10fi", "UNITfi")
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==============                                                        |  20%  |                                                                              |============================                                          |  40%  |                                                                              |==========================================                            |  60%  |                                                                              |========================================================              |  80%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.0559 secs
```

Close the connection to the database.

``` r
DatabaseConnector::disconnect(connection)
```

### Validate the new CDM tables with DataQualityDashboard

Since we have introduced changes in the OMOP CDM table, we can use the
DataQualityDashboard package to validate that we havent introduced
errors. We include the function `validateCDMtablesWithDQD` in the
package to facilitate this task.

``` r
# Create connectionDetails from the existing connection
validationResultsFolder <- tempdir()

validationLogR6 <- validateCDMtablesWithDQD(connectionDetails, vocabularyDatabaseSchema, validationResultsFolder)
#> Connecting using DuckDB driver
#> Connecting using DuckDB driver
#> Currently in a tryCatch or withCallingHandlers block, so unable to add global calling handlers. ParallelLogger will not capture R messages, errors, and warnings, only explicit calls to ParallelLogger. (This message will not be shown again this R session)
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   checkLevel = col_character(),
#>   checkName = col_character(),
#>   checkDescription = col_character(),
#>   kahnContext = col_character(),
#>   kahnCategory = col_character(),
#>   kahnSubcategory = col_character(),
#>   sqlFile = col_character(),
#>   evaluationFilter = col_character(),
#>   severity = col_character()
#> )
#> CDM Tables skipped: CARE_SITE, CDM_SOURCE, COHORT, COHORT_DEFINITION, CONDITION_ERA, CONDITION_OCCURRENCE, COST, DEATH, DEVICE_EXPOSURE, DOSE_ERA, DQDASHBOARD_RESULTS, DRUG_ERA, DRUG_EXPOSURE, EPISODE, EPISODE_EVENT, FACT_RELATIONSHIP, LOCATION, MAIN.DQDASHBOARD_RESULTS, MEASUREMENT, METADATA, NOTE, NOTE_NLP, OBSERVATION, OBSERVATION_PERIOD, PAYER_PLAN_PERIOD, PERSON, PROCEDURE_OCCURRENCE, PROVIDER, SOURCE_TO_CONCEPT_MAP, SOURCE_TO_CONCEPT_MAP_EXTENDED, SPECIMEN, VISIT_DETAIL, VISIT_OCCURRENCE
#> Warning in DataQualityDashboard::executeDqChecks(connectionDetails =
#> connectionDetails, : DEPRECATION WARNING - The plausibleDuringLife check has
#> been reimplemented with the plausibleBeforeDeath check.
#> Warning in DataQualityDashboard::executeDqChecks(connectionDetails =
#> connectionDetails, : DEPRECATION WARNING - The plausibleTemporalAfter check has
#> been reimplemented with the plausibleAfterBirth and plausibleStartBeforeEnd
#> checks.
#> Warning in DataQualityDashboard::executeDqChecks(connectionDetails =
#> connectionDetails, : DEPRECATION WARNING - The plausibleGender check has been
#> reimplemented with the plausibleGenderUseDescendants check.
#> Connecting using DuckDB driver
#> Processing check description: cdmTable
#> Processing check description: measurePersonCompleteness
#> Processing check description: measureConditionEraCompleteness
#> Processing check description: measureObservationPeriodOverlap
#> Processing check description: cdmField
#> Processing check description: isRequired
#> Processing check description: cdmDatatype
#> Processing check description: isPrimaryKey
#> Processing check description: isForeignKey
#> Processing check description: fkDomain
#> Processing check description: fkClass
#> Processing check description: isStandardValidConcept
#> Processing check description: measureValueCompleteness
#> Processing check description: standardConceptRecordCompleteness
#> Processing check description: sourceConceptRecordCompleteness
#> Processing check description: sourceValueCompleteness
#> Processing check description: plausibleValueLow
#> Processing check description: plausibleValueHigh
#> Processing check description: plausibleTemporalAfter
#> Processing check description: plausibleDuringLife
#> Processing check description: withinVisitDates
#> Processing check description: plausibleAfterBirth
#> Processing check description: plausibleBeforeDeath
#> Processing check description: plausibleStartBeforeEnd
#> Processing check description: plausibleGender
#> Processing check description: plausibleGenderUseDescendants
#> Processing check description: plausibleUnitConceptIds
#> Writing results to file: /tmp/RtmphaWyHx/tmp_vocab_table-20251201090445.json
#> Execution Complete
#> Connecting using DuckDB driver
#> Writing results to table main.dqdashboard_results
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Executing SQL took 0.00658 secs
#> Warning in value[[3L]](cond): Writing table failed: Invalid Error: Conversion Error: Could not convert string 'Table CONCEPT_SYNONYM is empty.' to INT32 when casting from source column not_applicable_reason
#> ℹ Context: rapi_execute
#> ℹ Error type: INVALID
#> ℹ Raw message: Conversion Error: Could not convert string 'Table CONCEPT_SYNONYM is empty.' to INT32 when casting from source column not_applicable_reason
```

``` r
knitr::kable(validationLogR6)
```

| context | type    | step                                                                                                                                                                                                                                       | message                        |
|:--------|:--------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------|
| DQD     | SUCCESS | CONCEPT: A yes or no value indicating if CONCEPT table is present as expected based on the specification.                                                                                                                                  | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY: A yes or no value indicating if VOCABULARY table is present as expected based on the specification.                                                                                                                            | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN: A yes or no value indicating if DOMAIN table is present as expected based on the specification.                                                                                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS: A yes or no value indicating if CONCEPT_CLASS table is present as expected based on the specification.                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP: A yes or no value indicating if CONCEPT_RELATIONSHIP table is present as expected based on the specification.                                                                                                        | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP: A yes or no value indicating if RELATIONSHIP table is present as expected based on the specification.                                                                                                                        | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_SYNONYM: A yes or no value indicating if CONCEPT_SYNONYM table is present as expected based on the specification.                                                                                                                  | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR: A yes or no value indicating if CONCEPT_ANCESTOR table is present as expected based on the specification.                                                                                                                | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH: A yes or no value indicating if DRUG_STRENGTH table is present as expected based on the specification.                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_CLASS_ID: A yes or no value indicating if CONCEPT_CLASS_ID is present in the CONCEPT table as expected based on the specification.                                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_CODE: A yes or no value indicating if CONCEPT_CODE is present in the CONCEPT table as expected based on the specification.                                                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_ID: A yes or no value indicating if CONCEPT_ID is present in the CONCEPT table as expected based on the specification.                                                                                                     | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_NAME: A yes or no value indicating if CONCEPT_NAME is present in the CONCEPT table as expected based on the specification.                                                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.DOMAIN_ID: A yes or no value indicating if DOMAIN_ID is present in the CONCEPT table as expected based on the specification.                                                                                                       | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.INVALID_REASON: A yes or no value indicating if INVALID_REASON is present in the CONCEPT table as expected based on the specification.                                                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.STANDARD_CONCEPT: A yes or no value indicating if STANDARD_CONCEPT is present in the CONCEPT table as expected based on the specification.                                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VALID_END_DATE: A yes or no value indicating if VALID_END_DATE is present in the CONCEPT table as expected based on the specification.                                                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VALID_START_DATE: A yes or no value indicating if VALID_START_DATE is present in the CONCEPT table as expected based on the specification.                                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VOCABULARY_ID: A yes or no value indicating if VOCABULARY_ID is present in the CONCEPT table as expected based on the specification.                                                                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.ANCESTOR_CONCEPT_ID: A yes or no value indicating if ANCESTOR_CONCEPT_ID is present in the CONCEPT_ANCESTOR table as expected based on the specification.                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.DESCENDANT_CONCEPT_ID: A yes or no value indicating if DESCENDANT_CONCEPT_ID is present in the CONCEPT_ANCESTOR table as expected based on the specification.                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MAX_LEVELS_OF_SEPARATION: A yes or no value indicating if MAX_LEVELS_OF_SEPARATION is present in the CONCEPT_ANCESTOR table as expected based on the specification.                                                       | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MIN_LEVELS_OF_SEPARATION: A yes or no value indicating if MIN_LEVELS_OF_SEPARATION is present in the CONCEPT_ANCESTOR table as expected based on the specification.                                                       | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_CONCEPT_ID: A yes or no value indicating if CONCEPT_CLASS_CONCEPT_ID is present in the CONCEPT_CLASS table as expected based on the specification.                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_ID: A yes or no value indicating if CONCEPT_CLASS_ID is present in the CONCEPT_CLASS table as expected based on the specification.                                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_NAME: A yes or no value indicating if CONCEPT_CLASS_NAME is present in the CONCEPT_CLASS table as expected based on the specification.                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_1: A yes or no value indicating if CONCEPT_ID_1 is present in the CONCEPT_RELATIONSHIP table as expected based on the specification.                                                                       | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_2: A yes or no value indicating if CONCEPT_ID_2 is present in the CONCEPT_RELATIONSHIP table as expected based on the specification.                                                                       | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.INVALID_REASON: A yes or no value indicating if INVALID_REASON is present in the CONCEPT_RELATIONSHIP table as expected based on the specification.                                                                   | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.RELATIONSHIP_ID: A yes or no value indicating if RELATIONSHIP_ID is present in the CONCEPT_RELATIONSHIP table as expected based on the specification.                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_END_DATE: A yes or no value indicating if VALID_END_DATE is present in the CONCEPT_RELATIONSHIP table as expected based on the specification.                                                                   | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_START_DATE: A yes or no value indicating if VALID_START_DATE is present in the CONCEPT_RELATIONSHIP table as expected based on the specification.                                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_SYNONYM.CONCEPT_ID: A yes or no value indicating if CONCEPT_ID is present in the CONCEPT_SYNONYM table as expected based on the specification.                                                                                     | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_SYNONYM.CONCEPT_SYNONYM_NAME: A yes or no value indicating if CONCEPT_SYNONYM_NAME is present in the CONCEPT_SYNONYM table as expected based on the specification.                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_SYNONYM.LANGUAGE_CONCEPT_ID: A yes or no value indicating if LANGUAGE_CONCEPT_ID is present in the CONCEPT_SYNONYM table as expected based on the specification.                                                                   | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_CONCEPT_ID: A yes or no value indicating if DOMAIN_CONCEPT_ID is present in the DOMAIN table as expected based on the specification.                                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_ID: A yes or no value indicating if DOMAIN_ID is present in the DOMAIN table as expected based on the specification.                                                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_NAME: A yes or no value indicating if DOMAIN_NAME is present in the DOMAIN table as expected based on the specification.                                                                                                     | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.AMOUNT_UNIT_CONCEPT_ID: A yes or no value indicating if AMOUNT_UNIT_CONCEPT_ID is present in the DRUG_STRENGTH table as expected based on the specification.                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.AMOUNT_VALUE: A yes or no value indicating if AMOUNT_VALUE is present in the DRUG_STRENGTH table as expected based on the specification.                                                                                     | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.BOX_SIZE: A yes or no value indicating if BOX_SIZE is present in the DRUG_STRENGTH table as expected based on the specification.                                                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.DENOMINATOR_UNIT_CONCEPT_ID: A yes or no value indicating if DENOMINATOR_UNIT_CONCEPT_ID is present in the DRUG_STRENGTH table as expected based on the specification.                                                       | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.DENOMINATOR_VALUE: A yes or no value indicating if DENOMINATOR_VALUE is present in the DRUG_STRENGTH table as expected based on the specification.                                                                           | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.DRUG_CONCEPT_ID: A yes or no value indicating if DRUG_CONCEPT_ID is present in the DRUG_STRENGTH table as expected based on the specification.                                                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.INGREDIENT_CONCEPT_ID: A yes or no value indicating if INGREDIENT_CONCEPT_ID is present in the DRUG_STRENGTH table as expected based on the specification.                                                                   | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.INVALID_REASON: A yes or no value indicating if INVALID_REASON is present in the DRUG_STRENGTH table as expected based on the specification.                                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.NUMERATOR_UNIT_CONCEPT_ID: A yes or no value indicating if NUMERATOR_UNIT_CONCEPT_ID is present in the DRUG_STRENGTH table as expected based on the specification.                                                           | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.NUMERATOR_VALUE: A yes or no value indicating if NUMERATOR_VALUE is present in the DRUG_STRENGTH table as expected based on the specification.                                                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.VALID_END_DATE: A yes or no value indicating if VALID_END_DATE is present in the DRUG_STRENGTH table as expected based on the specification.                                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | DRUG_STRENGTH.VALID_START_DATE: A yes or no value indicating if VALID_START_DATE is present in the DRUG_STRENGTH table as expected based on the specification.                                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.DEFINES_ANCESTRY: A yes or no value indicating if DEFINES_ANCESTRY is present in the RELATIONSHIP table as expected based on the specification.                                                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.IS_HIERARCHICAL: A yes or no value indicating if IS_HIERARCHICAL is present in the RELATIONSHIP table as expected based on the specification.                                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_CONCEPT_ID: A yes or no value indicating if RELATIONSHIP_CONCEPT_ID is present in the RELATIONSHIP table as expected based on the specification.                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_ID: A yes or no value indicating if RELATIONSHIP_ID is present in the RELATIONSHIP table as expected based on the specification.                                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_NAME: A yes or no value indicating if RELATIONSHIP_NAME is present in the RELATIONSHIP table as expected based on the specification.                                                                             | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.REVERSE_RELATIONSHIP_ID: A yes or no value indicating if REVERSE_RELATIONSHIP_ID is present in the RELATIONSHIP table as expected based on the specification.                                                                 | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_CONCEPT_ID: A yes or no value indicating if VOCABULARY_CONCEPT_ID is present in the VOCABULARY table as expected based on the specification.                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_ID: A yes or no value indicating if VOCABULARY_ID is present in the VOCABULARY table as expected based on the specification.                                                                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_NAME: A yes or no value indicating if VOCABULARY_NAME is present in the VOCABULARY table as expected based on the specification.                                                                                     | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_REFERENCE: A yes or no value indicating if VOCABULARY_REFERENCE is present in the VOCABULARY table as expected based on the specification.                                                                           | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_VERSION: A yes or no value indicating if VOCABULARY_VERSION is present in the VOCABULARY table as expected based on the specification.                                                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_CLASS_ID: The number and percent of records with a NULL value in the CONCEPT_CLASS_ID of the CONCEPT that is considered not nullable.                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_CODE: The number and percent of records with a NULL value in the CONCEPT_CODE of the CONCEPT that is considered not nullable.                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_ID: The number and percent of records with a NULL value in the CONCEPT_ID of the CONCEPT that is considered not nullable.                                                                                                  | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_NAME: The number and percent of records with a NULL value in the CONCEPT_NAME of the CONCEPT that is considered not nullable.                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.DOMAIN_ID: The number and percent of records with a NULL value in the DOMAIN_ID of the CONCEPT that is considered not nullable.                                                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VALID_END_DATE: The number and percent of records with a NULL value in the VALID_END_DATE of the CONCEPT that is considered not nullable.                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VALID_START_DATE: The number and percent of records with a NULL value in the VALID_START_DATE of the CONCEPT that is considered not nullable.                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VOCABULARY_ID: The number and percent of records with a NULL value in the VOCABULARY_ID of the CONCEPT that is considered not nullable.                                                                                            | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.ANCESTOR_CONCEPT_ID: The number and percent of records with a NULL value in the ANCESTOR_CONCEPT_ID of the CONCEPT_ANCESTOR that is considered not nullable.                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.DESCENDANT_CONCEPT_ID: The number and percent of records with a NULL value in the DESCENDANT_CONCEPT_ID of the CONCEPT_ANCESTOR that is considered not nullable.                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MAX_LEVELS_OF_SEPARATION: The number and percent of records with a NULL value in the MAX_LEVELS_OF_SEPARATION of the CONCEPT_ANCESTOR that is considered not nullable.                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MIN_LEVELS_OF_SEPARATION: The number and percent of records with a NULL value in the MIN_LEVELS_OF_SEPARATION of the CONCEPT_ANCESTOR that is considered not nullable.                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_CONCEPT_ID: The number and percent of records with a NULL value in the CONCEPT_CLASS_CONCEPT_ID of the CONCEPT_CLASS that is considered not nullable.                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_ID: The number and percent of records with a NULL value in the CONCEPT_CLASS_ID of the CONCEPT_CLASS that is considered not nullable.                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_NAME: The number and percent of records with a NULL value in the CONCEPT_CLASS_NAME of the CONCEPT_CLASS that is considered not nullable.                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_1: The number and percent of records with a NULL value in the CONCEPT_ID_1 of the CONCEPT_RELATIONSHIP that is considered not nullable.                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_2: The number and percent of records with a NULL value in the CONCEPT_ID_2 of the CONCEPT_RELATIONSHIP that is considered not nullable.                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.RELATIONSHIP_ID: The number and percent of records with a NULL value in the RELATIONSHIP_ID of the CONCEPT_RELATIONSHIP that is considered not nullable.                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_END_DATE: The number and percent of records with a NULL value in the VALID_END_DATE of the CONCEPT_RELATIONSHIP that is considered not nullable.                                                                | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_START_DATE: The number and percent of records with a NULL value in the VALID_START_DATE of the CONCEPT_RELATIONSHIP that is considered not nullable.                                                            | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.CONCEPT_ID: The number and percent of records with a NULL value in the CONCEPT_ID of the CONCEPT_SYNONYM that is considered not nullable.                                                                                  | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.CONCEPT_SYNONYM_NAME: The number and percent of records with a NULL value in the CONCEPT_SYNONYM_NAME of the CONCEPT_SYNONYM that is considered not nullable.                                                              | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.LANGUAGE_CONCEPT_ID: The number and percent of records with a NULL value in the LANGUAGE_CONCEPT_ID of the CONCEPT_SYNONYM that is considered not nullable.                                                                | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_CONCEPT_ID: The number and percent of records with a NULL value in the DOMAIN_CONCEPT_ID of the DOMAIN that is considered not nullable.                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_ID: The number and percent of records with a NULL value in the DOMAIN_ID of the DOMAIN that is considered not nullable.                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_NAME: The number and percent of records with a NULL value in the DOMAIN_NAME of the DOMAIN that is considered not nullable.                                                                                                  | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DRUG_CONCEPT_ID: The number and percent of records with a NULL value in the DRUG_CONCEPT_ID of the DRUG_STRENGTH that is considered not nullable.                                                                            | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.INGREDIENT_CONCEPT_ID: The number and percent of records with a NULL value in the INGREDIENT_CONCEPT_ID of the DRUG_STRENGTH that is considered not nullable.                                                                | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.VALID_END_DATE: The number and percent of records with a NULL value in the VALID_END_DATE of the DRUG_STRENGTH that is considered not nullable.                                                                              | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.VALID_START_DATE: The number and percent of records with a NULL value in the VALID_START_DATE of the DRUG_STRENGTH that is considered not nullable.                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.DEFINES_ANCESTRY: The number and percent of records with a NULL value in the DEFINES_ANCESTRY of the RELATIONSHIP that is considered not nullable.                                                                            | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.IS_HIERARCHICAL: The number and percent of records with a NULL value in the IS_HIERARCHICAL of the RELATIONSHIP that is considered not nullable.                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_CONCEPT_ID: The number and percent of records with a NULL value in the RELATIONSHIP_CONCEPT_ID of the RELATIONSHIP that is considered not nullable.                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_ID: The number and percent of records with a NULL value in the RELATIONSHIP_ID of the RELATIONSHIP that is considered not nullable.                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_NAME: The number and percent of records with a NULL value in the RELATIONSHIP_NAME of the RELATIONSHIP that is considered not nullable.                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.REVERSE_RELATIONSHIP_ID: The number and percent of records with a NULL value in the REVERSE_RELATIONSHIP_ID of the RELATIONSHIP that is considered not nullable.                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_CONCEPT_ID: The number and percent of records with a NULL value in the VOCABULARY_CONCEPT_ID of the VOCABULARY that is considered not nullable.                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_ID: The number and percent of records with a NULL value in the VOCABULARY_ID of the VOCABULARY that is considered not nullable.                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_NAME: The number and percent of records with a NULL value in the VOCABULARY_NAME of the VOCABULARY that is considered not nullable.                                                                                  | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_ID: A yes or no value indicating if the CONCEPT_ID in the CONCEPT is the expected data type based on the specification. Only checks integer fields.                                                                        | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.ANCESTOR_CONCEPT_ID: A yes or no value indicating if the ANCESTOR_CONCEPT_ID in the CONCEPT_ANCESTOR is the expected data type based on the specification. Only checks integer fields.                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.DESCENDANT_CONCEPT_ID: A yes or no value indicating if the DESCENDANT_CONCEPT_ID in the CONCEPT_ANCESTOR is the expected data type based on the specification. Only checks integer fields.                                | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MAX_LEVELS_OF_SEPARATION: A yes or no value indicating if the MAX_LEVELS_OF_SEPARATION in the CONCEPT_ANCESTOR is the expected data type based on the specification. Only checks integer fields.                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MIN_LEVELS_OF_SEPARATION: A yes or no value indicating if the MIN_LEVELS_OF_SEPARATION in the CONCEPT_ANCESTOR is the expected data type based on the specification. Only checks integer fields.                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_CONCEPT_ID: A yes or no value indicating if the CONCEPT_CLASS_CONCEPT_ID in the CONCEPT_CLASS is the expected data type based on the specification. Only checks integer fields.                                | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_1: A yes or no value indicating if the CONCEPT_ID_1 in the CONCEPT_RELATIONSHIP is the expected data type based on the specification. Only checks integer fields.                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_2: A yes or no value indicating if the CONCEPT_ID_2 in the CONCEPT_RELATIONSHIP is the expected data type based on the specification. Only checks integer fields.                                          | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.CONCEPT_ID: A yes or no value indicating if the CONCEPT_ID in the CONCEPT_SYNONYM is the expected data type based on the specification. Only checks integer fields.                                                        | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.LANGUAGE_CONCEPT_ID: A yes or no value indicating if the LANGUAGE_CONCEPT_ID in the CONCEPT_SYNONYM is the expected data type based on the specification. Only checks integer fields.                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_CONCEPT_ID: A yes or no value indicating if the DOMAIN_CONCEPT_ID in the DOMAIN is the expected data type based on the specification. Only checks integer fields.                                                            | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.AMOUNT_UNIT_CONCEPT_ID: A yes or no value indicating if the AMOUNT_UNIT_CONCEPT_ID in the DRUG_STRENGTH is the expected data type based on the specification. Only checks integer fields.                                    | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.BOX_SIZE: A yes or no value indicating if the BOX_SIZE in the DRUG_STRENGTH is the expected data type based on the specification. Only checks integer fields.                                                                | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DENOMINATOR_UNIT_CONCEPT_ID: A yes or no value indicating if the DENOMINATOR_UNIT_CONCEPT_ID in the DRUG_STRENGTH is the expected data type based on the specification. Only checks integer fields.                          | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DRUG_CONCEPT_ID: A yes or no value indicating if the DRUG_CONCEPT_ID in the DRUG_STRENGTH is the expected data type based on the specification. Only checks integer fields.                                                  | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.INGREDIENT_CONCEPT_ID: A yes or no value indicating if the INGREDIENT_CONCEPT_ID in the DRUG_STRENGTH is the expected data type based on the specification. Only checks integer fields.                                      | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.NUMERATOR_UNIT_CONCEPT_ID: A yes or no value indicating if the NUMERATOR_UNIT_CONCEPT_ID in the DRUG_STRENGTH is the expected data type based on the specification. Only checks integer fields.                              | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_CONCEPT_ID: A yes or no value indicating if the RELATIONSHIP_CONCEPT_ID in the RELATIONSHIP is the expected data type based on the specification. Only checks integer fields.                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_CONCEPT_ID: A yes or no value indicating if the VOCABULARY_CONCEPT_ID in the VOCABULARY is the expected data type based on the specification. Only checks integer fields.                                            | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_ID: The number and percent of records that have a duplicate value in the CONCEPT_ID field of the CONCEPT.                                                                                                                  | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_ID: The number and percent of records that have a duplicate value in the CONCEPT_CLASS_ID field of the CONCEPT_CLASS.                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_ID: The number and percent of records that have a duplicate value in the DOMAIN_ID field of the DOMAIN.                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_ID: The number and percent of records that have a duplicate value in the RELATIONSHIP_ID field of the RELATIONSHIP.                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_ID: The number and percent of records that have a duplicate value in the VOCABULARY_ID field of the VOCABULARY.                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_CLASS_ID: The number and percent of records that have a value in the CONCEPT_CLASS_ID field in the CONCEPT table that does not exist in the CONCEPT_CLASS table.                                                           | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.DOMAIN_ID: The number and percent of records that have a value in the DOMAIN_ID field in the CONCEPT table that does not exist in the DOMAIN table.                                                                                | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VOCABULARY_ID: The number and percent of records that have a value in the VOCABULARY_ID field in the CONCEPT table that does not exist in the VOCABULARY table.                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.ANCESTOR_CONCEPT_ID: The number and percent of records that have a value in the ANCESTOR_CONCEPT_ID field in the CONCEPT_ANCESTOR table that does not exist in the CONCEPT table.                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.DESCENDANT_CONCEPT_ID: The number and percent of records that have a value in the DESCENDANT_CONCEPT_ID field in the CONCEPT_ANCESTOR table that does not exist in the CONCEPT table.                                     | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_CONCEPT_ID: The number and percent of records that have a value in the CONCEPT_CLASS_CONCEPT_ID field in the CONCEPT_CLASS table that does not exist in the CONCEPT table.                                     | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_1: The number and percent of records that have a value in the CONCEPT_ID_1 field in the CONCEPT_RELATIONSHIP table that does not exist in the CONCEPT table.                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_2: The number and percent of records that have a value in the CONCEPT_ID_2 field in the CONCEPT_RELATIONSHIP table that does not exist in the CONCEPT table.                                               | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.RELATIONSHIP_ID: The number and percent of records that have a value in the RELATIONSHIP_ID field in the CONCEPT_RELATIONSHIP table that does not exist in the RELATIONSHIP table.                                    | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.CONCEPT_ID: The number and percent of records that have a value in the CONCEPT_ID field in the CONCEPT_SYNONYM table that does not exist in the CONCEPT table.                                                             | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.LANGUAGE_CONCEPT_ID: The number and percent of records that have a value in the LANGUAGE_CONCEPT_ID field in the CONCEPT_SYNONYM table that does not exist in the CONCEPT table.                                           | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_CONCEPT_ID: The number and percent of records that have a value in the DOMAIN_CONCEPT_ID field in the DOMAIN table that does not exist in the CONCEPT table.                                                                 | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.AMOUNT_UNIT_CONCEPT_ID: The number and percent of records that have a value in the AMOUNT_UNIT_CONCEPT_ID field in the DRUG_STRENGTH table that does not exist in the CONCEPT table.                                         | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DENOMINATOR_UNIT_CONCEPT_ID: The number and percent of records that have a value in the DENOMINATOR_UNIT_CONCEPT_ID field in the DRUG_STRENGTH table that does not exist in the CONCEPT table.                               | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DRUG_CONCEPT_ID: The number and percent of records that have a value in the DRUG_CONCEPT_ID field in the DRUG_STRENGTH table that does not exist in the CONCEPT table.                                                       | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.INGREDIENT_CONCEPT_ID: The number and percent of records that have a value in the INGREDIENT_CONCEPT_ID field in the DRUG_STRENGTH table that does not exist in the CONCEPT table.                                           | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.NUMERATOR_UNIT_CONCEPT_ID: The number and percent of records that have a value in the NUMERATOR_UNIT_CONCEPT_ID field in the DRUG_STRENGTH table that does not exist in the CONCEPT table.                                   | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_CONCEPT_ID: The number and percent of records that have a value in the RELATIONSHIP_CONCEPT_ID field in the RELATIONSHIP table that does not exist in the CONCEPT table.                                         | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_CONCEPT_ID: The number and percent of records that have a value in the VOCABULARY_CONCEPT_ID field in the VOCABULARY table that does not exist in the CONCEPT table.                                                 | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DRUG_CONCEPT_ID: The number and percent of records that have a value in the DRUG_CONCEPT_ID field in the DRUG_STRENGTH table that do not conform to the DRUG domain.                                                         | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.INGREDIENT_CONCEPT_ID: The number and percent of records that have a value in the INGREDIENT_CONCEPT_ID field in the DRUG_STRENGTH table that do not conform to the INGREDIENT class.                                        | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_CLASS_ID: The number and percent of records with a NULL value in the CONCEPT_CLASS_ID of the CONCEPT.                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_CODE: The number and percent of records with a NULL value in the CONCEPT_CODE of the CONCEPT.                                                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_ID: The number and percent of records with a NULL value in the CONCEPT_ID of the CONCEPT.                                                                                                                                  | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.CONCEPT_NAME: The number and percent of records with a NULL value in the CONCEPT_NAME of the CONCEPT.                                                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.DOMAIN_ID: The number and percent of records with a NULL value in the DOMAIN_ID of the CONCEPT.                                                                                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.INVALID_REASON: The number and percent of records with a NULL value in the INVALID_REASON of the CONCEPT.                                                                                                                          | Number of violated rows: 30785 |
| DQD     | SUCCESS | CONCEPT.STANDARD_CONCEPT: The number and percent of records with a NULL value in the STANDARD_CONCEPT of the CONCEPT.                                                                                                                      | Number of violated rows: 20551 |
| DQD     | SUCCESS | CONCEPT.VALID_END_DATE: The number and percent of records with a NULL value in the VALID_END_DATE of the CONCEPT.                                                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VALID_START_DATE: The number and percent of records with a NULL value in the VALID_START_DATE of the CONCEPT.                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VOCABULARY_ID: The number and percent of records with a NULL value in the VOCABULARY_ID of the CONCEPT.                                                                                                                            | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.ANCESTOR_CONCEPT_ID: The number and percent of records with a NULL value in the ANCESTOR_CONCEPT_ID of the CONCEPT_ANCESTOR.                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.DESCENDANT_CONCEPT_ID: The number and percent of records with a NULL value in the DESCENDANT_CONCEPT_ID of the CONCEPT_ANCESTOR.                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MAX_LEVELS_OF_SEPARATION: The number and percent of records with a NULL value in the MAX_LEVELS_OF_SEPARATION of the CONCEPT_ANCESTOR.                                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_ANCESTOR.MIN_LEVELS_OF_SEPARATION: The number and percent of records with a NULL value in the MIN_LEVELS_OF_SEPARATION of the CONCEPT_ANCESTOR.                                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_CONCEPT_ID: The number and percent of records with a NULL value in the CONCEPT_CLASS_CONCEPT_ID of the CONCEPT_CLASS.                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_ID: The number and percent of records with a NULL value in the CONCEPT_CLASS_ID of the CONCEPT_CLASS.                                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_CLASS.CONCEPT_CLASS_NAME: The number and percent of records with a NULL value in the CONCEPT_CLASS_NAME of the CONCEPT_CLASS.                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_1: The number and percent of records with a NULL value in the CONCEPT_ID_1 of the CONCEPT_RELATIONSHIP.                                                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.CONCEPT_ID_2: The number and percent of records with a NULL value in the CONCEPT_ID_2 of the CONCEPT_RELATIONSHIP.                                                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.INVALID_REASON: The number and percent of records with a NULL value in the INVALID_REASON of the CONCEPT_RELATIONSHIP.                                                                                                | Number of violated rows: 57725 |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.RELATIONSHIP_ID: The number and percent of records with a NULL value in the RELATIONSHIP_ID of the CONCEPT_RELATIONSHIP.                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_END_DATE: The number and percent of records with a NULL value in the VALID_END_DATE of the CONCEPT_RELATIONSHIP.                                                                                                | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_START_DATE: The number and percent of records with a NULL value in the VALID_START_DATE of the CONCEPT_RELATIONSHIP.                                                                                            | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.CONCEPT_ID: The number and percent of records with a NULL value in the CONCEPT_ID of the CONCEPT_SYNONYM.                                                                                                                  | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.CONCEPT_SYNONYM_NAME: The number and percent of records with a NULL value in the CONCEPT_SYNONYM_NAME of the CONCEPT_SYNONYM.                                                                                              | Number of violated rows: 0     |
| DQD     | WARNING | CONCEPT_SYNONYM.LANGUAGE_CONCEPT_ID: The number and percent of records with a NULL value in the LANGUAGE_CONCEPT_ID of the CONCEPT_SYNONYM.                                                                                                | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_CONCEPT_ID: The number and percent of records with a NULL value in the DOMAIN_CONCEPT_ID of the DOMAIN.                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_ID: The number and percent of records with a NULL value in the DOMAIN_ID of the DOMAIN.                                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | DOMAIN.DOMAIN_NAME: The number and percent of records with a NULL value in the DOMAIN_NAME of the DOMAIN.                                                                                                                                  | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.AMOUNT_UNIT_CONCEPT_ID: The number and percent of records with a NULL value in the AMOUNT_UNIT_CONCEPT_ID of the DRUG_STRENGTH.                                                                                              | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.AMOUNT_VALUE: The number and percent of records with a NULL value in the AMOUNT_VALUE of the DRUG_STRENGTH.                                                                                                                  | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.BOX_SIZE: The number and percent of records with a NULL value in the BOX_SIZE of the DRUG_STRENGTH.                                                                                                                          | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DENOMINATOR_UNIT_CONCEPT_ID: The number and percent of records with a NULL value in the DENOMINATOR_UNIT_CONCEPT_ID of the DRUG_STRENGTH.                                                                                    | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DENOMINATOR_VALUE: The number and percent of records with a NULL value in the DENOMINATOR_VALUE of the DRUG_STRENGTH.                                                                                                        | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.DRUG_CONCEPT_ID: The number and percent of records with a NULL value in the DRUG_CONCEPT_ID of the DRUG_STRENGTH.                                                                                                            | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.INGREDIENT_CONCEPT_ID: The number and percent of records with a NULL value in the INGREDIENT_CONCEPT_ID of the DRUG_STRENGTH.                                                                                                | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.INVALID_REASON: The number and percent of records with a NULL value in the INVALID_REASON of the DRUG_STRENGTH.                                                                                                              | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.NUMERATOR_UNIT_CONCEPT_ID: The number and percent of records with a NULL value in the NUMERATOR_UNIT_CONCEPT_ID of the DRUG_STRENGTH.                                                                                        | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.NUMERATOR_VALUE: The number and percent of records with a NULL value in the NUMERATOR_VALUE of the DRUG_STRENGTH.                                                                                                            | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.VALID_END_DATE: The number and percent of records with a NULL value in the VALID_END_DATE of the DRUG_STRENGTH.                                                                                                              | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.VALID_START_DATE: The number and percent of records with a NULL value in the VALID_START_DATE of the DRUG_STRENGTH.                                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.DEFINES_ANCESTRY: The number and percent of records with a NULL value in the DEFINES_ANCESTRY of the RELATIONSHIP.                                                                                                            | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.IS_HIERARCHICAL: The number and percent of records with a NULL value in the IS_HIERARCHICAL of the RELATIONSHIP.                                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_CONCEPT_ID: The number and percent of records with a NULL value in the RELATIONSHIP_CONCEPT_ID of the RELATIONSHIP.                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_ID: The number and percent of records with a NULL value in the RELATIONSHIP_ID of the RELATIONSHIP.                                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.RELATIONSHIP_NAME: The number and percent of records with a NULL value in the RELATIONSHIP_NAME of the RELATIONSHIP.                                                                                                          | Number of violated rows: 0     |
| DQD     | SUCCESS | RELATIONSHIP.REVERSE_RELATIONSHIP_ID: The number and percent of records with a NULL value in the REVERSE_RELATIONSHIP_ID of the RELATIONSHIP.                                                                                              | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_CONCEPT_ID: The number and percent of records with a NULL value in the VOCABULARY_CONCEPT_ID of the VOCABULARY.                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_ID: The number and percent of records with a NULL value in the VOCABULARY_ID of the VOCABULARY.                                                                                                                      | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_NAME: The number and percent of records with a NULL value in the VOCABULARY_NAME of the VOCABULARY.                                                                                                                  | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_REFERENCE: The number and percent of records with a NULL value in the VOCABULARY_REFERENCE of the VOCABULARY.                                                                                                        | Number of violated rows: 0     |
| DQD     | SUCCESS | VOCABULARY.VOCABULARY_VERSION: The number and percent of records with a NULL value in the VOCABULARY_VERSION of the VOCABULARY.                                                                                                            | Number of violated rows: 4     |
| DQD     | SUCCESS | CONCEPT.VALID_END_DATE: The number and percent of records with a value in the VALID_END_DATE field of the CONCEPT that occurs prior to the date in the VALID_START_DATE field of the CONCEPT table.                                        | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_END_DATE: The number and percent of records with a value in the VALID_END_DATE field of the CONCEPT_RELATIONSHIP that occurs prior to the date in the VALID_START_DATE field of the CONCEPT_RELATIONSHIP table. | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.VALID_END_DATE: The number and percent of records with a value in the VALID_END_DATE field of the DRUG_STRENGTH that occurs prior to the date in the VALID_START_DATE field of the DRUG_STRENGTH table.                      | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT.VALID_START_DATE: The number and percent of records with a value in the VALID_START_DATE field of the CONCEPT that occurs after the date in the VALID_END_DATE.                                                                    | Number of violated rows: 0     |
| DQD     | SUCCESS | CONCEPT_RELATIONSHIP.VALID_START_DATE: The number and percent of records with a value in the VALID_START_DATE field of the CONCEPT_RELATIONSHIP that occurs after the date in the VALID_END_DATE.                                          | Number of violated rows: 0     |
| DQD     | WARNING | DRUG_STRENGTH.VALID_START_DATE: The number and percent of records with a value in the VALID_START_DATE field of the DRUG_STRENGTH that occurs after the date in the VALID_END_DATE.                                                        | Number of violated rows: 0     |

We can see that there are no errors.

``` r
validationLogR6 |>
  dplyr::filter(type == "ERROR") |>
  knitr::kable()
```

| context | type | step | message |
|---------|------|------|---------|
