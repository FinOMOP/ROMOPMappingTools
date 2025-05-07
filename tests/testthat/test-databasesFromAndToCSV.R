testthat::test_that("duckdbToOMOPVocabularyCSVs", {
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    pathToOMOPVocabularyCSVsFolder <- tempdir()
    withr::defer(unlink(pathToOMOPVocabularyCSVsFolder))

    # Create connection to test database
    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    duckdbToOMOPVocabularyCSVs(
        connection = connection,
        vocabularyDatabaseSchema = "main",
        OMOPVocabularyTableNames = NULL,
        pathToOMOPVocabularyCSVsFolder = pathToOMOPVocabularyCSVsFolder
    )

    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "CONCEPT.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "CONCEPT_ANCESTOR.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "CONCEPT_CLASS.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "CONCEPT_RELATIONSHIP.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "CONCEPT_SYNONYM.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "DRUG_STRENGTH.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "DOMAIN.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "RELATIONSHIP.csv")))
    expect_true(file.exists(file.path(pathToOMOPVocabularyCSVsFolder, "VOCABULARY.csv")))

    # load again to check the dqd
    newDuckdb <- tempfile()

    connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "duckdb",
        server = newDuckdb
    )

    connection <- DatabaseConnector::connect(connectionDetails)

    omopVocabularyCSVsToDuckDB(
        pathToOMOPVocabularyCSVsFolder = pathToOMOPVocabularyCSVsFolder,
        connection = connection,
        vocabularyDatabaseSchema = "main"
    )

    suppressWarnings(
        validation <- validateCDMtablesWithDQD(
            connectionDetails = connectionDetails,
            vocabularyDatabaseSchema = "main",
            validationResultsFolder = tempdir()
        )
    )

    all(validation$type == "SUCCESS") |> expect_true()
})
