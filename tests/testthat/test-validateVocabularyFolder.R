test_that("validateVocabularyFolder returns all SUCCESS for valid vocabulary folder", {
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
    pathToValidatedUsagiFolder <- file.path(tempdir(), "validated_vocabularies")
    dir.create(pathToValidatedUsagiFolder, showWarnings = FALSE)
    pathToOMOPVocabularyDuckDBfile <- system.file("testdata/OMOPVocabularyICD10only/OMOPVocabularyICD10only.duckdb", package = "ROMOPMappingTools")
    vocabularyDatabaseSchema <- "main"

    # Create connection to test database
    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    # Run validation
    validationsLogTibble <- validateVocabularyFolder(
        pathToVocabularyFolder, 
        connection, 
        vocabularyDatabaseSchema, 
        pathToValidatedUsagiFolder
    )

    # Check validation results
    validationsLogTibble |> dplyr::filter(type != "SUCCESS") |> nrow() |> expect_equal(0)

    # Check that validated files were created
    expect_true(dir.exists(pathToValidatedUsagiFolder))
    expect_true(file.exists(file.path(pathToValidatedUsagiFolder, "ICD10fi", "ICD10fi.usagi.csv")))
    expect_true(file.exists(file.path(pathToValidatedUsagiFolder, "UNITfi", "UNITfi.usagi.csv")))
})
