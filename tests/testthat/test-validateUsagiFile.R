test_that("test validateUsagiFile returns no errors with a valid usagi file", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
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

  # Usagi file has not changed apart from mappingStatus and ADD_INFO:validationMessages
  usagiTibble <- readUsagiFile(pathToUsagiFile)
  validatedUsagiTibble <- readUsagiFile(pathToValidatedUsagiFile)

  byNames <- usagiTibble |> names() |> setdiff(c("mappingStatus", "ADD_INFO:validationMessages"))
  expect_equal(usagiTibble |> dplyr::select(dplyr::all_of(byNames)), validatedUsagiTibble |> dplyr::select(dplyr::all_of(byNames)))

})

test_that("test validateUsagiFile returns errors with the errored usagi file", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_with_errors.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
  withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
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
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode is empty")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")

  # SourceCode and conceptId are not unique
  validationsSummary |> dplyr::filter(step == "SourceCode and conceptId are not unique") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode and conceptId are not unique"))  |> nrow() |> expect_equal(2)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode and conceptId are not unique")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(rep("ERROR: SourceCode and conceptId are not unique", 2))
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode and conceptId are not unique")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(rep("FLAGGED", 2))

  # SourceName is empty
  validationsSummary |> dplyr::filter(step == "SourceName is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(is.na(sourceName))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(is.na(sourceName)) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceName is empty")
  validatedUsagiFile |> dplyr::filter(is.na(sourceName)) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")
  
  # SourceCode is more than 50 characters
  validationsSummary |> dplyr::filter(step == "SourceCode is more than 50 characters") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode is more than 50 characters"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode is more than 50 characters")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceCode is more than 50 characters")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceCode is more than 50 characters")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")
  
  # SourceName is more than 255 characters
  validationsSummary |> dplyr::filter(step == "SourceName is more than 255 characters") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceName is more than 255 characters"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceName is more than 255 characters")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceName is more than 255 characters")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceName is more than 255 characters")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")

  # SourceFrequency is empty
  # not implemented as Usagi wont open this file

  # MappingStatus is empty
  # not implemented as Usagi wont open this file

  # MappingStatus is not one of the following
  # not implemented as Usagi wont open this file

  # Concept_id is 0 for APPROVED mappingStatus
  validationsSummary |> dplyr::filter(step == "APPROVED mappingStatus conceptId is 0") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "APPROVED mappingStatus conceptId is 0"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "APPROVED mappingStatus conceptId is 0")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: APPROVED mappingStatus conceptId is 0")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "APPROVED mappingStatus conceptId is 0")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")

  # APPROVEDConceptIds not in vocabularies
  validationsSummary |> dplyr::filter(step == "APPROVED mappingStatus with concepts outdated") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED mappingStatus with concepts outdated"))  |> nrow() |> expect_equal(3)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED mappingStatus with concepts outdated")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(rep("ERROR OUTDATED conceptId: conceptId 1234 does not exist on the target vocabularies", 3))
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED mappingStatus with concepts outdated")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(rep("APPROVED", 3))

  # Not APPROVED ConceptIds not in vocabularies
  validationsSummary |> dplyr::filter(step == "Not APPROVED mappingStatus with concepts outdated") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Not APPROVED mappingStatus with concepts outdated"))  |> nrow() |> expect_equal(3)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Not APPROVED mappingStatus with concepts outdated")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(rep("WARNING OUTDATED conceptId: conceptId 1234 does not exist on the target vocabularies", 3))
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Not APPROVED mappingStatus with concepts outdated")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(rep("UNCHECKED", 3))

  #APPROVED ConceptIds outdated
  validationsSummary |> dplyr::filter(step == "APPROVED mappingStatus with concepts outdated") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED ConceptIds outdated\\]"))  |> nrow() |> expect_equal(7)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED ConceptIds outdated\\]"))|> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(
    c("ERROR OUTDATED domainId: domainId for conceptId 4150516 is different in the target vocabularies",
      "ERROR OUTDATED conceptName: conceptName for conceptId 4130374 is different in the target vocabularies", 
      "ERROR OUTDATED standardConcept: standardConcept for conceptId 320073 has changed to non-standard",
      "ERROR OUTDATED standardConcept: standardConcept for conceptId 320073 has changed to non-standard",
      "ERROR OUTDATED standardConcept: standardConcept for conceptId 3085666 has changed to non-standard",
      "ERROR OUTDATED standardConcept: standardConcept for conceptId 3079174 has changed to non-standard", 
      "ERROR OUTDATED standardConcept: standardConcept for conceptId 30258 has changed to non-standard"
    )
  )
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED ConceptIds outdated\\]")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(rep("APPROVED", 7))

  #Not APPROVED ConceptIds outdated
  validationsSummary |> dplyr::filter(step == "Not APPROVED mappingStatus with concepts outdated") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Not APPROVED ConceptIds outdated"))  |> nrow() |> expect_equal(7)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Not APPROVED ConceptIds outdated"))|> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(
    c("WARNING OUTDATED domainId: domainId for conceptId 4150516 is different in the target vocabularies",
      "WARNING OUTDATED conceptName: conceptName for conceptId 4130374 is different in the target vocabularies", 
      "WARNING OUTDATED standardConcept: standardConcept for conceptId 320073 has changed to non-standard",
      "WARNING OUTDATED standardConcept: standardConcept for conceptId 320073 has changed to non-standard",
      "WARNING OUTDATED standardConcept: standardConcept for conceptId 3085666 has changed to non-standard",
      "WARNING OUTDATED standardConcept: standardConcept for conceptId 3079174 has changed to non-standard", 
      "WARNING OUTDATED standardConcept: standardConcept for conceptId 30258 has changed to non-standard"
    )
  )
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Not APPROVED ConceptIds outdated")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(rep("UNCHECKED", 7))

  # Missing C&CR columns
  # 

  # SourceConceptId is empty
  validationsSummary |> dplyr::filter(step == "SourceConceptId is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is empty"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is empty")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptId is empty")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is empty")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")

  # SourceConceptId is not a number on the range
  validationsSummary |> dplyr::filter(step == "SourceConceptId is not a number on the range") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not a number on the range"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not a number on the range")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptId is not a number on the range")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not a number on the range")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")

  # SourceConceptId is not unique
  validationsSummary |> dplyr::filter(step == "SourceConceptId is not unique") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not unique"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not unique")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptId is not unique")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptId is not unique")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")

  # SourceConceptClass is empty
  validationsSummary |> dplyr::filter(step == "SourceConceptClass is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is empty"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is empty")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptClass is empty")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is empty")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")
  
  # SourceConceptClass is more than 20 characters
  validationsSummary |> dplyr::filter(step == "SourceConceptClass is more than 20 characters") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is more than 20 characters"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is more than 20 characters")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceConceptClass is more than 20 characters")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceConceptClass is more than 20 characters")) |> dplyr::pull(mappingStatus) |> 
  expect_equal("FLAGGED")

  # SourceDomain is empty
  validationsSummary |> dplyr::filter(step == "SourceDomain is empty") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is empty"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is empty")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceDomain is empty")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is empty")) |> dplyr::pull(mappingStatus) |>  
  expect_equal("FLAGGED")

  # SourceDomain is not a valid domain
  validationsSummary |> dplyr::filter(step == "SourceDomain is not a valid domain") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is not a valid domain"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is not a valid domain")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceDomain is not a valid domain")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceDomain is not a valid domain")) |> dplyr::pull(mappingStatus) |>  
  expect_equal("FLAGGED")
  
  # APPROVED Invalid domain combination
  validationsSummary |> dplyr::filter(step == "APPROVED mappingStatus with valid domain combination") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED Invalid domain combination"))  |> nrow() |> expect_equal(2)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED Invalid domain combination")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(rep("ERROR OUTDATED domainId: domainId for conceptId 4109415 is different in the target vocabularies | ERROR: this code is mapped to more than one domains that are not compatible: Condition noDomain", 2))
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[APPROVED Invalid domain combination")) |> dplyr::pull(mappingStatus) |>  
  expect_equal(rep("FLAGGED", 2))

  # Not APPROVED Invalid domain combination
  validationsSummary |> dplyr::filter(step == "Not APPROVED mappingStatus with valid domain combination") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[Not APPROVED Invalid domain combination"))  |> nrow() |> expect_equal(2)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[Not APPROVED Invalid domain combination")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal(rep("WARNING OUTDATED domainId: domainId for conceptId 4109415 is different in the target vocabularies | WARNING: this code is mapped to more than one domains that are not compatible: Condition noDomain", 2))
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "\\[Not APPROVED Invalid domain combination")) |> dplyr::pull(mappingStatus) |>  
  expect_equal(rep("UNCHECKED", 2))

  # Missing date columns

  # SourceValidStartDate is after SourceValidEndDate
  validationsSummary |> dplyr::filter(step == "SourceValidStartDate is after SourceValidEndDate") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceValidStartDate is after SourceValidEndDate"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceValidStartDate is after SourceValidEndDate")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: SourceValidStartDate is after SourceValidEndDate")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "SourceValidStartDate is after SourceValidEndDate")) |> dplyr::pull(mappingStatus) |>  
  expect_equal("FLAGGED")

  # Invalid parent concept code
  validationsSummary |> dplyr::filter(step == "Invalid parent concept code") |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code]"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code]")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: WWW is not a valid concept in this vocabulary | ERROR: OOO is not a valid concept in this vocabulary")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code]")) |> dplyr::pull(mappingStatus) |>  
  expect_equal("FLAGGED")

  # Invalid parent concept code in other vocabulary
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code in other vocabulary"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code in other vocabulary")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: WWW is not a valid concept code in vocabulary ICD10 | ERROR: OOO is not a valid concept code in vocabulary ICD10")
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent concept code in other vocabulary")) |> dplyr::pull(mappingStatus) |>  
  expect_equal("FLAGGED")

  # Missing parent columns

  # Invalid parent vocabulary
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent vocabulary"))  |> nrow() |> expect_equal(1)
  validatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Invalid parent vocabulary")) |> dplyr::pull(`ADD_INFO:validationMessages`) |> 
  expect_equal("ERROR: A06 is not a valid concept code in vocabulary INVENT | ERROR: A06.8 is not a valid concept code in vocabulary INVENT")

  # it opens with Usagi
  #file.copy(pathToValidatedUsagiFile, "~/Downloads/ICD10fi.usagi.csv", overwrite = TRUE)


  # Usagi file has not changed apart from mappingStatus and ADD_INFO:validationMessages
  usagiTibble <- readUsagiFile(pathToUsagiFile)
  validatedUsagiTibble <- readUsagiFile(pathToValidatedUsagiFile)

  byNames <- usagiTibble |> names() |> setdiff(c("mappingStatus", "ADD_INFO:validationMessages"))
  expect_equal(usagiTibble |> dplyr::select(dplyr::all_of(byNames)), validatedUsagiTibble |> dplyr::select(dplyr::all_of(byNames)))
})


















test_that("test validateUsagiFile validates ADD_INFO:sourceName_fi", {

  pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
  withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
  pathToValidatedUsagiFile <- tempfile(fileext = ".csv")
  vocabularyDatabaseSchema = "main"
  sourceConceptIdOffset = 2000500000

  # make a copy of a valid usagi file with errors in ADD_INFO:sourceName_fi
  usagiTibble <- readUsagiFile(system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools"))
  usagiTibble <- usagiTibble |>
    dplyr::mutate(
      `ADD_INFO:sourceName_fi` = dplyr::case_when(
        # too long
        sourceCode == "A01.0+G01" ~ strrep("a", 1001),
        # empty
        sourceCode == "A01.0+I39.8" ~ NA_character_,
        # different within a multi-mapped sourceCode
        sourceCode == "A02.2+H22.0" & dplyr::row_number() == min(which(sourceCode == "A02.2+H22.0")) ~ "Eri nimi",
        TRUE ~ `ADD_INFO:sourceName_fi`
      )
    )
  pathToUsagiFile <- tempfile(fileext = ".csv")
  writeUsagiFile(usagiTibble, pathToUsagiFile)

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

  # only the sourceName_fi validations fail
  validationsSummary |> dplyr::filter(type != "SUCCESS") |> dplyr::pull(step) |>
    expect_setequal(c(
      "SourceNameFi is more than 1000 characters",
      "SourceNameFi is not the same for all rows of the sourceCode",
      "SourceNameFi is empty"
    ))

  # SourceNameFi is more than 1000 characters
  validationsSummary |> dplyr::filter(step == "SourceNameFi is more than 1000 characters") |> dplyr::pull(type) |> as.character() |> expect_equal("ERROR")
  validatedUsagiFile |> dplyr::filter(sourceCode == "A01.0+G01") |> dplyr::pull(`ADD_INFO:validationMessages`) |>
    expect_equal("ERROR: SourceNameFi is more than 1000 characters")
  validatedUsagiFile |> dplyr::filter(sourceCode == "A01.0+G01") |> dplyr::pull(mappingStatus) |> expect_equal("FLAGGED")

  # SourceNameFi is not the same for all rows of the sourceCode
  validationsSummary |> dplyr::filter(step == "SourceNameFi is not the same for all rows of the sourceCode") |> dplyr::pull(type) |> as.character() |> expect_equal("ERROR")
  validatedUsagiFile |> dplyr::filter(sourceCode == "A02.2+H22.0") |> dplyr::pull(`ADD_INFO:validationMessages`) |>
    expect_equal(rep("ERROR: SourceNameFi is not the same for all rows of the sourceCode", 2))
  validatedUsagiFile |> dplyr::filter(sourceCode == "A02.2+H22.0") |> dplyr::pull(mappingStatus) |> expect_equal(rep("FLAGGED", 2))

  # SourceNameFi is empty, only a warning, mappingStatus is not changed
  validationsSummary |> dplyr::filter(step == "SourceNameFi is empty") |> dplyr::pull(type) |> as.character() |> expect_equal("WARNING")
  validatedUsagiFile |> dplyr::filter(sourceCode == "A01.0+I39.8") |> dplyr::pull(`ADD_INFO:validationMessages`) |>
    expect_equal("WARNING: SourceNameFi is empty")
  validatedUsagiFile |> dplyr::filter(sourceCode == "A01.0+I39.8") |> dplyr::pull(mappingStatus) |> expect_equal("APPROVED")
})

test_that("test validateUsagiFile requires C&CR columns when ADD_INFO:sourceName_fi is present", {

  pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
  withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))

  usagiTibble <- readUsagiFile(system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")) |>
    dplyr::select(-dplyr::starts_with("ADD_INFO:"), `ADD_INFO:sourceName_fi`)
  pathToUsagiFile <- tempfile(fileext = ".csv")
  writeUsagiFile(usagiTibble, pathToUsagiFile)

  connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
  on.exit(DatabaseConnector::disconnect(connection))

  validationsSummary <- validateUsagiFile(pathToUsagiFile, connection, "main", tempfile(fileext = ".csv"), 2000500000)

  validationsSummary |> dplyr::filter(step == "Missing C&CR columns") |> dplyr::pull(type) |> as.character() |> expect_equal("ERROR")
})
