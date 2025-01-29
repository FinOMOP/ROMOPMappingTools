test_that("validateVocabularyFolder returns all SUCCESS for valid vocabulary folder", {
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
    pathToValidatedVocabularyFolder <- file.path(tempdir(), "validated_vocabularies")
    dir.create(pathToValidatedVocabularyFolder, showWarnings = FALSE)
    withr::defer(unlink(pathToValidatedVocabularyFolder, recursive = TRUE))
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
    validationsLogTibble <- validateVocabularyFolder(
        pathToVocabularyFolder, 
        connection, 
        vocabularyDatabaseSchema, 
        pathToValidatedVocabularyFolder
    )

    # Check validation results
    validationsLogTibble |> dplyr::filter(type != "SUCCESS") |> nrow() |> expect_equal(0)

    # Check that validated files were created
    expect_true(dir.exists(pathToValidatedVocabularyFolder))
    expect_true(file.exists(file.path(pathToValidatedVocabularyFolder, "ICD10fi", "ICD10fi.usagi.csv")))
    expect_true(file.exists(file.path(pathToValidatedVocabularyFolder, "UNITfi", "UNITfi.usagi.csv")))
})

test_that("validateVocabularyFolder returns all ERRORS for invalid vocabulary folder", {
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIESwithErrors", package = "ROMOPMappingTools")
    pathToValidatedVocabularyFolder <- file.path(tempdir(), "validated_vocabularies_with_errors")
    dir.create(pathToValidatedVocabularyFolder, showWarnings = FALSE)
    withr::defer(unlink(pathToValidatedVocabularyFolder, recursive = TRUE))
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
        pathToValidatedVocabularyFolder
    )

    # Check validation results
    validationsLogTibble |> dplyr::filter(type != "SUCCESS") |> nrow() |> expect_equal(1)
    validationsLogTibble |> dplyr::filter(type == "ERROR") |> dplyr::pull(message) |> expect_equal("Number of failed rules:  1")

    # Check that validated files were created
    expect_true(dir.exists(pathToValidatedVocabularyFolder))
    readr::read_csv(file.path(pathToValidatedVocabularyFolder, "vocabularies.csv"), show_col_types = FALSE)  |> 
        dplyr::filter(tmpvalidationMessages != "") |> 
        nrow() |> 
        expect_equal(1)

    expect_false(file.exists(file.path(pathToValidatedVocabularyFolder, "ICD10fi", "ICD10fi.usagi.csv")))
    expect_false(file.exists(file.path(pathToValidatedVocabularyFolder, "UNITfi", "UNITfi.usagi.csv")))
})
