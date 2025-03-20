test_that("buildStatusDashboard works", {
    # Set up test data
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
    pathToCodeCountsFolder <- system.file("testdata/CODE_COUNTS", package = "ROMOPMappingTools")
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
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
    suppressWarnings(
    validationLogTibble <- buildVocabulariesAll(
        pathToVocabularyFolder = pathToVocabularyFolder,
        pathToCodeCountsFolder = pathToCodeCountsFolder,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        validationResultsFolder = validationResultsFolder
        )
    )

    output_file_html <- file.path(validationResultsFolder, "MappingStatusDashboard.html")

    validationLogTibble <- buildStatusDashboard(
        pathToCodeCountsFolder = pathToCodeCountsFolder,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        output_file_html = output_file_html
    )

    # Check results
    validationLogTibble |> dplyr::filter(type == "ERROR") |> nrow() |> expect_equal(0)

    # check the validation results folder
    expect_true(file.exists(output_file_html))
})  


