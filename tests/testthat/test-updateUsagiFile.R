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

  ignoreSteps <- c("APPROVED mappingStatus with concepts outdated", "APPROVED mappingStatus with valid domain combination")
  validationSummaryBeforeUpdateNoErrors   <- validationSummaryBeforeUpdate |> 
    dplyr::filter(!step %in% ignoreSteps) 
  validationSummaryAfterUpdateNoErrors    <- validationSummaryAfterUpdate |> 
    dplyr::filter(!step %in% ignoreSteps) 

  validationSummaryBeforeUpdateNoErrors  |> expect_equal(validationSummaryAfterUpdateNoErrors)


  # same number in summary than in the usagi fil

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

  # only updates have been introduced
  pathToValidatedUsagiFileBeforeUpdate <- tempfile(fileext = ".csv")
  validationSummaryBeforeUpdate <- validateUsagiFile(pathToUsagiFile, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFileBeforeUpdate, sourceConceptIdOffset = 2000500000)
  pathToValidatedUsagiFileAfterUpdate <- tempfile(fileext = ".csv")
  validationSummaryAfterUpdate <- validateUsagiFile(pathToUpdatedUsagiFile, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFileAfterUpdate, sourceConceptIdOffset = 2000500000)
  
  ignoreSteps <- c(
    "APPROVED mappingStatus with concepts outdated", 
    "Not APPROVED mappingStatus with concepts outdated", 
    "APPROVED mappingStatus with valid domain combination", 
    "Not APPROVED mappingStatus with valid domain combination"
  )
  validationSummaryBeforeUpdateNoErrors   <- validationSummaryBeforeUpdate |> 
    dplyr::filter(!step %in% ignoreSteps) 
  validationSummaryAfterUpdateNoErrors    <- validationSummaryAfterUpdate |> 
    dplyr::filter(!step %in% ignoreSteps) 

  validationSummaryBeforeUpdateNoErrors  |> expect_equal(validationSummaryAfterUpdateNoErrors)

  # Updated domains
  updateSummary |> dplyr::filter(step == "Updated domains") |> nrow() |> expect_equal(1)
  updateSummary |> dplyr::filter(step == "Updated domains") |> dplyr::pull(message) |> expect_equal("Updated 4 domains")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated domain"))  |> nrow() |> expect_equal(2)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated domain")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("domainId updated from Observation to Condition")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated domain")) |> dplyr::pull(domainId) |> 
  expect_equal(c("Condition", "Condition"))

  # Updated concept names
  updateSummary |> dplyr::filter(step == "Updated concept names") |> nrow() |> expect_equal(1)
  updateSummary |> dplyr::filter(step == "Updated concept names") |> dplyr::pull(message) |> expect_equal("Updated 3 concept names")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated concept name"))  |> nrow() |> expect_equal(2)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated concept name")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptName changed from Tuberculosis of new name to Tuberculosis of brain")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Outdated concept name")) |> dplyr::pull(conceptName) |> 
  expect_equal(c("Tuberculosis of brain", "Tuberculosis of brain"))

  # Updated conceptIds
  updateSummary |> dplyr::filter(message == "Updated 2 conceptIds that don't need review") |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds maps to"))  |> nrow() |> expect_equal(4)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds maps to")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  unique() |> expect_match("conceptId changed from 320073 to 4148615 based on relationship :Maps to, does not need reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds maps to")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(4148615, 4174262, 4148615, 4174262))

  # Updated conceptIds that need review
  updateSummary |> dplyr::filter(message == "Updated 9 conceptIds that need review") |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept replaced by")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId changed from 3085666 to 3428638 based on relationship :Concept replaced by, needs reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept replaced by")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(3428638, 3428638))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept replaced by")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED", "UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept same_as to")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId changed from 3079174 to 3245108 based on relationship :Concept same_as to, needs reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept same_as to")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(3245108, 3245108))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept same_as to")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED", "UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept poss_eq to")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId changed from 30258 to 618787 based on relationship :Concept poss_eq to, needs reviewing")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept poss_eq to")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(618787, 618787))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept poss_eq to")) |> dplyr::pull(mappingStatus) |>  
  expect_equal(c("UNCHECKED", "UNCHECKED"))
  #
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds Concept")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(rep("Automatic update", 6))

  # Updated conceptIds that could not be updated automatically
  updateSummary |> dplyr::filter(message == "3 conceptIds could not be updated automatically, remapping needed") |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found\\]")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId 4071477 could not be updated automatically, remapping needed")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found\\]")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("INVALID_TARGET"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found\\]")) |> dplyr::pull(conceptId)  |> 
  expect_equal(0)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found\\]")) |> dplyr::pull(conceptName)  |> 
  expect_equal(c("Unmapped"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found\\]")) |> dplyr::pull(comment)  |> 
  expect_equal(c("Invalid existing target: 4071477"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found\\]")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(NA_character_)

  # Updated conceptIds that could not be updated automatically 2, it has 2 rows with 2 invalid conceptIds
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found 2\\]")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId 4071477 could not be updated automatically, remapping needed")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found 2\\]")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("INVALID_TARGET"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found 2\\]")) |> dplyr::pull(conceptId)  |> 
  expect_equal(c(0))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found 2\\]")) |> dplyr::pull(conceptName)  |> 
  expect_equal(c("Unmapped"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found 2\\]")) |> dplyr::pull(comment)  |> 
  expect_equal(c("Invalid existing target: 4071477"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated conceptIds not found 2\\]")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(NA_character_)

  # Updated by usagi
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_match("conceptId 4071477 could not be updated automatically, remapping needed")
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(0))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("INVALID_TARGET"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi]")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(NA_character_)

  # Updated by usagi 2 one invalid
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> nrow() |> expect_equal(2)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  unique() |> length() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(4121541,3245108))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED", "UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 one invalid")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(c("Automatic update", "Automatic update"))

  # Updated by usagi 2 2 invalid
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 2 invalid")) |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 2 invalid")) |> dplyr::pull(conceptId) |> 
  expect_equal(3245108)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 2 invalid")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 2 2 invalid")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(c("Automatic update"))

  #Updated by usagi 3 1 invalid
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 3 1 invalid")) |> nrow() |> expect_equal(4)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 3 1 invalid")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(4054068, 4331284, 4310238, 3245108))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 3 1 invalid")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED", "UNCHECKED", "UNCHECKED", "UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 3 1 invalid")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(c("Automatic update", "Automatic update", "Automatic update", "Automatic update"))
  
  #Updated by usagi 1 reamap to mapped
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 1 reamap to mapped")) |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 1 reamap to mapped")) |> dplyr::pull(conceptId) |> 
  expect_equal(c(320741))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 1 reamap to mapped")) |> dplyr::pull(mappingStatus) |> 
  expect_equal(c("UNCHECKED"))
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated by usagi 1 reamap to mapped")) |> dplyr::pull(statusSetBy) |> 
  expect_equal(c("Automatic update"))

  #Updated in the pass
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated in the pass")) |> nrow() |> expect_equal(1)
  updatedUsagiFile |> dplyr::filter(stringr::str_detect(sourceName, "Updated in the pass")) |> dplyr::pull(`ADD_INFO:autoUpdatingInfo`) |> 
  expect_equal("2025-03-27 | conceptName changed from Aortography to Angiography of aorta")
})
















