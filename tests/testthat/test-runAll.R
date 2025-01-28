test_that("runAll works", {
    # Set up test data
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
    pathToOMOPVocabularyDuckDBfile <-createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    vocabularyDatabaseSchema <- "main"
    validationResultsFolder <- file.path(tempdir(), "validationResults")
    dir.create(validationResultsFolder, showWarnings = FALSE, recursive = TRUE)
    withr::defer(unlink(validationResultsFolder, recursive = TRUE))
    
    # Create connection details for test database
    connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    
    # Run function
    validationLogTibble <- runAll(
        pathToVocabularyFolder = pathToVocabularyFolder,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        validationResultsFolder = validationResultsFolder
    )
    
    # Check results
    # validationLogTibble  |> dplyr::filter(type == "ERROR") |> View()
    validationLogTibble |> expect_s3_class("tbl_df")
    validationLogTibble |> dplyr::filter(type == "ERROR") |> nrow() |> expect_equal(0)

    # check the validation results folder
    expect_true(file.exists(file.path(validationResultsFolder, "validationLogTibble.csv")))
    expect_true(file.exists(file.path(validationResultsFolder, "resultsDQD.json")))
})  
