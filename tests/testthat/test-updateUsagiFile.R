test_that("test updateUsagiFile returns no errors with a valid usagi file", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_outdated.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
  withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
  
  vocabularyDatabaseSchema = "main"
  pathToUpdatedUsagiFile <- tempfile(fileext = ".csv")

  # Create connection to test database
  connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
  on.exit(DatabaseConnector::disconnect(connection))

  updateSummary <- updateUsagiFile(
    pathToUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToUpdatedUsagiFile,
    skipValidation = TRUE
  )

  # no new lines introduced
  usagiFile  <- readUsagiFile(pathToUsagiFile)
  updatedUsagiFile <- readUsagiFile(pathToUpdatedUsagiFile)
  usagiFile |> nrow() |> expect_equal(updatedUsagiFile |> nrow())

  # only updates have been introduced
  pathToValidatedUsagiFileBeforeUpdate <- tempfile(fileext = ".csv")
  validationSummaryBeforeUpdate <- validateUsagiFile(pathToUsagiFile, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFileBeforeUpdate, sourceConceptIdOffset = 2000500000)
  pathToValidatedUsagiFileAfterUpdate <- tempfile(fileext = ".csv")
  validationSummaryAfterUpdate <- validateUsagiFile(pathToUpdatedUsagiFile, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFileAfterUpdate, sourceConceptIdOffset = 2000500000)
  
  validationSummaryBeforeUpdateNoErrors   <- validationSummaryBeforeUpdate |> 
    dplyr::filter(!stringr::str_detect(step, "ConceptIds outdated")) |> 
    dplyr::filter(!stringr::str_detect(step, "Invalid domain combination")) 
  validationSummaryAfterUpdateNoErrors    <- validationSummaryAfterUpdate |> 
    dplyr::filter(!stringr::str_detect(step, "ConceptIds outdated")) |> 
    dplyr::filter(!stringr::str_detect(step, "Invalid domain combination")) 

  validationSummaryBeforeUpdateNoErrors  |> expect_equal(validationSummaryAfterUpdateNoErrors)


  # same number in summary than in the usagi file
  nSourceCodesAffected <- validationSummaryBeforeUpdate |> dplyr::filter(type != "SUCCESS") |> dplyr::pull(message) |> 
    stringr::str_extract("\\d+") |> 
    as.numeric()

  pathToValidatedUsagiFileBeforeUpdate |> readUsagiFile() |> filter(!is.na(`ADD_INFO:validationMessages`)) |> distinct(sourceCode, .keep_all = TRUE) |> nrow() |> expect_equal(nSourceCodesAffected)


  pathToUpdatedUsagiFile |> readUsagiFile() |> filter(!is.na(`ADD_INFO:autoUpdatingInfo`))  |> distinct(sourceCode) |> nrow() 
  pathToValidatedUsagiFileAfterUpdate |> readUsagiFile() |> filter(!is.na(`ADD_INFO:validationMessages`))  |> distinct(sourceCode, .keep_all = TRUE) |> nrow() 

  # if run again no change 
  pathToUpdatedUsagiFile2 <- tempfile(fileext = ".csv")
  updateSummary2 <- updateUsagiFile(
    pathToUpdatedUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToUpdatedUsagiFile2,
    skipValidation = TRUE,
    appendOrClearAutoUpdatingInfo = "clear"
  )
  
  updateSummary2 |> dplyr::filter(type == "WARNING") |> expect_equal(updateSummary |> dplyr::filter(type == "WARNING"))
  # copy updated file to ICD10fi.usagi.csv
  # file.copy(pathToUpdatedUsagiFile, system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools"), overwrite = TRUE)
})

test_that("test updateUsagiFile detects if the mappings are out of date", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_with_errors.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
  withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
  pathToUpdatedUsagiFile <- tempfile(fileext = ".csv")
  vocabularyDatabaseSchema = "main"
  
  # Create connection to test database
  connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
  on.exit(DatabaseConnector::disconnect(connection))

  updateSummary <- updateUsagiFile(
    pathToUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToUpdatedUsagiFile,
    skipValidation = TRUE
  )
  updatedUsagiFile <- readUsagiFile(pathToUpdatedUsagiFile)

  # no new lines introduced, only for Invalid codes 
  usagiFile  <- readUsagiFile(pathToUsagiFile)
  updatedUsagiFile <- readUsagiFile(pathToUpdatedUsagiFile)
  usagiFile |> nrow() |> expect_equal(updatedUsagiFile |> nrow() -2)

  # only updates have been introduced
  pathToValidatedUsagiFileBeforeUpdate <- tempfile(fileext = ".csv")
  validationSummaryBeforeUpdate <- validateUsagiFile(pathToUsagiFile, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFileBeforeUpdate, sourceConceptIdOffset = 2000500000)
  pathToValidatedUsagiFileAfterUpdate <- tempfile(fileext = ".csv")
  validationSummaryAfterUpdate <- validateUsagiFile(pathToUpdatedUsagiFile, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFileAfterUpdate, sourceConceptIdOffset = 2000500000)
  
  validationSummaryBeforeUpdate   <- validationSummaryBeforeUpdate |> 
    dplyr::filter(!stringr::str_detect(step, "ConceptIds outdated")) |> 
    dplyr::filter(!stringr::str_detect(step, "Invalid domain combination")) 
  validationSummaryAfterUpdate    <- validationSummaryAfterUpdate |> 
    dplyr::filter(!stringr::str_detect(step, "ConceptIds outdated")) |> 
    dplyr::filter(!stringr::str_detect(step, "Invalid domain combination")) 

  validationSummaryBeforeUpdate  |> expect_equal(validationSummaryAfterUpdate)

  # Updated domains
  updateSummary |> dplyr::filter(step == "Updated domains") |> nrow() |> expect_equal(1)
  updateSummary |> dplyr::filter(step == "Updated domains") |> dplyr::pull(message) |> expect_equal("Updated 2 domains")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated domain"))  |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated domain")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("domainId updated from Observation to Condition")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated domain")) |> dplyr::pull(domainId) |> 
  expect_equal(c("Condition"))

  # Updated concept names
  updateSummary |> dplyr::filter(step == "Updated concept names") |> nrow() |> expect_equal(1)
  updateSummary |> dplyr::filter(step == "Updated concept names") |> dplyr::pull(message) |> expect_equal("Updated 1 concept names")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated concept name"))  |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated concept name")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptName changed from Tuberculosis of new name to Tuberculosis of brain")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated concept name")) |> dplyr::pull(conceptName) |> 
  expect_equal(c("Tuberculosis of brain"))

  # Updated conceptIds
  updateSummary |> dplyr::filter(message == "Updated 2 conceptIds that don't need review") |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds maps to"))  |> nrow() |> expect_equal(2)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds maps to")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  unique() |> expect_match("conceptId changed from 320073 to 4148615 based on relationship :Maps to, does not need reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds maps to")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(4148615, 4174262))

  # Updated conceptIds that need review
  updateSummary |> dplyr::filter(message == "Updated 7 conceptIds that need review") |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept replaced by")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId changed from 3085666 to 3428638 based on relationship :Concept replaced by, needs reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept replaced by")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(3428638))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept replaced by")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept same_as to")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId changed from 3079174 to 3245108 based on relationship :Concept same_as to, needs reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept same_as to")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(3245108))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept same_as to")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept poss_eq to")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId changed from 30258 to 618787 based on relationship :Concept poss_eq to, needs reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept poss_eq to")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(618787))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept poss_eq to")) |> dplyr::pull(mappingStatus) |>  
  expect_equal(c("UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept poss_eq to")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(618787))

  # Updated conceptIds that could not be updated automatically
  updateSummary |> dplyr::filter(message == "2 conceptIds could not be updated automatically, remapping needed") |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId 4071477 could not be updated automatically, remapping needed")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("INVALID_TARGET"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found")) |> dplyr::pull(conceptId)  |> 
  expect_equal(0)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found")) |> dplyr::pull(conceptName)  |> 
  expect_equal(c("Unmapped"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found")) |> dplyr::pull(comment)  |> 
  expect_equal(c("Invalid existing target: 4071477"))

  # Updated by usagi
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId 4071477 could not be updated automatically, remapping needed")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(0))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("INVALID_TARGET"))

  # Updated by usagi 2 one invalid
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> nrow() |> expect_equal(2)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  unique() |> length() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(4148615,3245108))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED", "UNCHECKED"))

  # Updated by usagi 2 2 invalid
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 2 invalid")) |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 2 invalid")) |> dplyr::pull(conceptId) |> 
  expect_equal(3245108)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 2 invalid")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED"))

  #Updated by usagi 3 1 invalid
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 3 1 invalid")) |> nrow() |> expect_equal(4)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 3 1 invalid")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(4054068, 4331284, 4310238, 3245108))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 3 1 invalid")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED", "UNCHECKED", "UNCHECKED", "UNCHECKED"))

  #Updated by usagi 1 reamap to mapped
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 1 reamap to mapped")) |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 1 reamap to mapped")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(320741))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 1 reamap to mapped")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED"))
})
















