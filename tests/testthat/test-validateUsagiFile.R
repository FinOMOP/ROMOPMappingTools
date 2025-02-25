test_that("test validateUsagiFile returns no errors with a valid usagi file", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- testthatSetup_pathToOMOPVocabularyDuckDBfile
  withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
  
  vocabularyDatabaseSchema = "main"
  sourceConceptIdOffset = 2000500000
  pathToValidatedUsagiFile <- tempfile(fileext = ".csv")

  # Create connection to test database
  connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
  on.exit(DatabaseConnector::disconnect(connection))

  validationsSummary <- validateUsagiFile(
    pathToUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToValidatedUsagiFile,
    sourceConceptIdOffset
  )

  # all validations must be successful
  validationsSummary |> dplyr::filter(type != "SUCCESS") |> nrow() |> expect_equal(0)

  # Usagi file has not changed 
  validatedUsagiFile <- readUsagiFile(pathToValidatedUsagiFile)

  validatedUsagiFile |> dplyr::filter(!is.na(`ADD_INFO:validationMessages`)) |> nrow() |> expect_equal(0)
})

test_that("test validateUsagiFile returns errors with the errored usagi file", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_with_errors.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- testthatSetup_pathToOMOPVocabularyDuckDBfile
  pathToValidatedUsagiFile <- tempfile(fileext = ".csv")
  vocabularyDatabaseSchema = "main"
  sourceConceptIdOffset = 2000500000
  # Create connection to test database
  connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
  on.exit(DatabaseConnector::disconnect(connection))

  validationsSummary <- validateUsagiFile(
    pathToUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToValidatedUsagiFile,
    sourceConceptIdOffset
  )
  
  validatedUsagiFile <- readUsagiFile(pathToValidatedUsagiFile)

  # Missing default columns
  # not implemented as Usagi wont open this file

  # SourceCode is empty
  validationsSummary |> dplyr::filter(step == "SourceCode is empty") |> nrow() |> expect_equal(1)

  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode is empty"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode is empty")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceCode is empty")

  # SourceCode and conceptId are not unique
  validationsSummary |> dplyr::filter(step == "SourceCode and conceptId are not unique") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode and conceptId are not unique"))  |> nrow() |> expect_equal(2)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode and conceptId are not unique")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(rep("ERROR: SourceCode and conceptId are not unique", 2))

  # SourceName is empty
  validationsSummary |> dplyr::filter(step == "SourceName is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(is.na(sourceName))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(is.na(sourceName)) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceName is empty")

  # SourceName is more than 255 characters
  validationsSummary |> dplyr::filter(step == "SourceName is more than 255 characters") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceName is more than 255 characters"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceName is more than 255 characters")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceName is more than 255 characters")

  # SourceFrequency is empty
  # not implemented as Usagi wont open this file

  # MappingStatus is empty
  # not implemented as Usagi wont open this file

  # MappingStatus is not one of the following
  # not implemented as Usagi wont open this file

  # Concept_id is 0 for APPROVED mappingStatus
  validationsSummary |> dplyr::filter(step == "Concept_id is 0 for APPROVED mappingStatus") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Concept_id is 0 for APPROVED mappingStatus"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Concept_id is 0 for APPROVED mappingStatus")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: Concept_id is 0 for APPROVED mappingStatus")

  # Missing C&CR columns
  # 

  # SourceConceptId is empty
  validationsSummary |> dplyr::filter(step == "SourceConceptId is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is empty"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is empty")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptId is empty")

  # SourceConceptId is not a number on the range
  validationsSummary |> dplyr::filter(step == "SourceConceptId is not a number on the range") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not a number on the range"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not a number on the range")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptId is not a number on the range")

  # SourceConceptClass is empty
  validationsSummary |> dplyr::filter(step == "SourceConceptClass is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is empty"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is empty")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptClass is empty")

  # SourceConceptClass is more than 20 characters
  validationsSummary |> dplyr::filter(step == "SourceConceptClass is more than 20 characters") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is more than 20 characters"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is more than 20 characters")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptClass is more than 20 characters")

  # SourceDomain is empty
  validationsSummary |> dplyr::filter(step == "SourceDomain is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is empty"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is empty")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceDomain is empty")

  # SourceDomain is not a valid domain
  validationsSummary |> dplyr::filter(step == "SourceDomain is not a valid domain") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is not a valid domain"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is not a valid domain")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceDomain is not a valid domain")

  # Invalid domain combination
  validationsSummary |> dplyr::filter(step == "Invalid domain combination") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid domain combination"))  |> nrow() |> expect_equal(2)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid domain combination")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(rep("ERROR: this code is mapped to more than one domains that are not compatible: Condition noDomain", 2))

  # Missing date columns

  # SourceValidStartDate is after SourceValidEndDate
  validationsSummary |> dplyr::filter(step == "SourceValidStartDate is after SourceValidEndDate") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceValidStartDate is after SourceValidEndDate"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceValidStartDate is after SourceValidEndDate")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceValidStartDate is after SourceValidEndDate")

  # Invalid parent concept code
  validationsSummary |> dplyr::filter(step == "Invalid parent concept code") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code]"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code]")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: WWW is not a valid concept in this vocabulary | ERROR: OOO is not a valid concept in this vocabulary")

  # Invalid parent concept code in other vocabulary
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code in other vocabulary"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code in other vocabulary")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: WWW is not a valid concept code in vocabulary ICD10 | ERROR: OOO is not a valid concept code in vocabulary ICD10")

  # Missing parent columns

  # Invalid parent vocabulary
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent vocabulary"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent vocabulary")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: A06 is not a valid concept code in vocabulary INVENT | ERROR: A06.8 is not a valid concept code in vocabulary INVENT")

  # it opens with Usagi
  #file.copy(pathToValidatedUsagiFile, "~/Downloads/ICD10fi.usagi.csv", overwrite = TRUE)
})


test_that("test validateUsagiFile detects if the mappings are out of date", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_outdated.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- testthatSetup_pathToOMOPVocabularyDuckDBfile
  pathToValidatedUsagiFile <- tempfile(fileext = ".csv")
  vocabularyDatabaseSchema = "main"
  sourceConceptIdOffset = 2000500000
  # Create connection to test database
  connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
  on.exit(DatabaseConnector::disconnect(connection))

  validationsSummary <- validateUsagiFile(
    pathToUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToValidatedUsagiFile,
    sourceConceptIdOffset
  )
  
  validatedUsagiFile <- readUsagiFile(pathToValidatedUsagiFile)

  # Outdated concepts
  validationsSummary |> dplyr::filter(step == "Outdated concepts") |> nrow() |> expect_equal(1)

  validatedUsagiFile |> dplyr::filter(stringr::str_detect(`ADD_INFO:validationMessages`, "OUTDATED"))  |> nrow() |> expect_equal(177)

})
















