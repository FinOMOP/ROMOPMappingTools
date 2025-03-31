test_that("updateVocabularyFolder returns all SUCCESS for valid vocabulary folder", {
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
    pathToUpdatedVocabularyFolder <- file.path(tempdir(), "updated_vocabularies")
    dir.create(pathToUpdatedVocabularyFolder, showWarnings = FALSE)
    withr::defer(unlink(pathToUpdatedVocabularyFolder, recursive = TRUE))
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    vocabularyDatabaseSchema <- "main"

    # Create connection to test database
    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    # Run validation
    updateLogTibble <- updateVocabularyFolder(
        pathToVocabularyFolder, 
        connection, 
        vocabularyDatabaseSchema, 
        pathToUpdatedVocabularyFolder
    )

    # Check validation results
    updateLogTibble |> dplyr::pull(type) |> as.character() |> expect_equal(c("SUCCESS", "SUCCESS"))

    # Check that validated files were created
    expect_true(dir.exists(pathToUpdatedVocabularyFolder))
    expect_true(file.exists(file.path(pathToUpdatedVocabularyFolder, "ICD10fi", "ICD10fi.usagi.csv")))
    expect_true(file.exists(file.path(pathToUpdatedVocabularyFolder, "UNITfi", "UNITfi.usagi.csv")))
})
