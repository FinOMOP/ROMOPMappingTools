
test_that("test updateUsagiFile detects if the mappings are out of date", {
  
  pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_outdated.usagi.csv", package = "ROMOPMappingTools")
  pathToOMOPVocabularyDuckDBfile <- testthatSetup_pathToOMOPVocabularyDuckDBfile
  pathToUpdatedUsagiFile <- tempfile(fileext = ".csv")
  vocabularyDatabaseSchema = "main"
  sourceConceptIdOffset = 2000500000
  # Create connection to test database
  connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
  on.exit(DatabaseConnector::disconnect(connection))

  updatedUsagiFile <- updateUsagiFile(
    pathToUsagiFile, 
    connection,
    vocabularyDatabaseSchema,
    pathToUpdatedUsagiFile,
    skipValidation = TRUE
  )
  
  updatedUsagiFile <- readUsagiFile(pathToUpdatedUsagiFile)

  # Outdated concepts
  validationsSummary |> dplyr::filter(step == "Outdated concepts") |> nrow() |> expect_equal(1)

  validatedUsagiFile |> dplyr::filter(stringr::str_detect(`ADD_INFO:validationMessages`, "OUTDATED"))  |> nrow() |> expect_equal(177)

})
















