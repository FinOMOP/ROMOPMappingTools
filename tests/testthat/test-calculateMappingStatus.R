test_that("calculateMappingStatus returns a tibble", {
    pathToCodeCountsFolder <- system.file("testdata/CODE_COUNTS", package = "ROMOPMappingTools")
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))

     # Create connection details for test database
    connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    vocabularyDatabaseSchema <- "main"

    mappingStatus <- calculateMappingStatus(
        pathToCodeCountsFolder = pathToCodeCountsFolder,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        includeCountForAllDatabases = TRUE,
        skipValidation = TRUE
    )

    mappingStatus |> checkmate::expect_list()
    mappingStatus$concepts_to_match  |> checkmate::expect_tibble()
    mappingStatus$code_counts_matched  |> checkmate::expect_tibble()

})
