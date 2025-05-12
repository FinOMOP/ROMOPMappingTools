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
            connectionDetails = connectionDetails,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            validationResultsFolder = validationResultsFolder
        )
    )

    output_file_html <- file.path(validationResultsFolder, "MappingStatusDashboard.html")


    # TMP warning
    suppressWarnings(
        outputFileHtmlPath <- buildStatusDashboard(
            pathToCodeCountsFolder = pathToCodeCountsFolder,
            pathToVocabularyFolder = pathToVocabularyFolder,
            connectionDetails = connectionDetails,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            outputFolderPath = validationResultsFolder
        )
    )


    # check the validation results folder
    expect_true(file.exists(outputFileHtmlPath))
})


test_that(".pageSummaryAllVocabularies works", {
    outputFolderPath <- tempdir()

    summaryAllVocabularies <- tibble::tribble(
        ~vocabularyId, ~mantainedBy, ~strMapped, ~FinnGenDF10, ~HUS,
        "ATC", "OMOP", NA, "100-500-10", "0-15033-15033",
        "ICD10fi", "OMOP+FinnOMOP", "3494-11", "0-12758-12758", "0-15033-15033",
        "UNITfi", "FinnOMOP", "173-4", NA, "173-1512-14937"
    )

    plotTable <- .plotTableSummaryAllVocabularies(summaryAllVocabularies)
    expect_s3_class(plotTable, "reactable")

    outputFileHtmlPath <- .pageSummaryAllVocabularies(summaryAllVocabularies, outputFolderPath)
    expect_true(file.exists(outputFileHtmlPath))
})


test_that("internal functions for .pageCoverageVocabularyDatabase works", {
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
            connectionDetails = connectionDetails,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            validationResultsFolder = validationResultsFolder
        )
    )

    sourceVocabularyId <- c("ICD10fi")
    targetVocabularyIds <- c("ICD10", "ICD10fi")
    vocabularyDatabaseSchema <- "main"
    databaseName <- "FinnGenDF10"
    pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")
    pathToNewsFile <- system.file("testdata/VOCABULARIES/ICD10fi/NEWS.md", package = "ROMOPMappingTools")
    #
    # - getDatabaseSummaryForVocabulary
    #
    databaseSummary <- .getDatabaseSummaryForVocabulary(
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        targetVocabularyIds = targetVocabularyIds
    )

    # Check that all columns in databaseSummary have values
    expect_true(all(colnames(databaseSummary) |>
        purrr::map_lgl(~ !all(is.na(databaseSummary[[.x]])))))

    #
    # - getCodeCountsForVocabulary
    #
    codeCounts <- .getCodeCountsForVocabularyAndDatabase(
        pathToCodeCountsFolder = pathToCodeCountsFolder,
        sourceVocabularyId = sourceVocabularyId,
        databaseName = databaseName
    )

    # Check that all columns in codeCounts have values
    expect_true(all(colnames(codeCounts) |>
        purrr::map_lgl(~ !all(is.na(codeCounts[[.x]])))))

    #
    # - getUsagiSummaryForVocabulary
    #
    usagiSummary <- .getUsagiSummaryForVocabulary(
        pathToVocabularyFolder = pathToVocabularyFolder,
        sourceVocabularyId = sourceVocabularyId
    )

    # Check that all columns in usagiSummary have values
    expect_true(all(colnames(usagiSummary) |>
        purrr::map_lgl(~ !all(is.na(usagiSummary[[.x]])))))


    #
    # - getSummaryTableForVocabularyAndDatabase
    #
    summaryTableForVocabularyAndDatabase <- .getSummaryTableForVocabularyAndDatabase(
        pathToCodeCountsFolder = pathToCodeCountsFolder,
        pathToVocabularyFolder = pathToVocabularyFolder,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceVocabularyId = sourceVocabularyId,
        targetVocabularyIds = targetVocabularyIds,
        databaseName = databaseName
    )

    #
    # - .plotTableForUsagiFile
    #
    usagiTibble <- readUsagiFile(pathToUsagiFile)
    plotTable <- .plotTableForUsagiFile(usagiTibble)
    expect_s3_class(plotTable, "reactable")

    plotSummaryTable <- .plotSummaryTableForUsagiFile(usagiTibble)
    expect_s3_class(plotSummaryTable, "reactable")

    #
    # - .pageSummaryTableForVocabularyAndDatabase
    #
    summaryTableForVocabularyAndDatabaseList <- list(
        "FinnGenDF10" = summaryTableForVocabularyAndDatabase
    )

    outputCoverageVocabularyDatabaseHtmlPath <- .pageCoverageVocabularyDatabases(
        summaryTableForVocabularyAndDatabaseList = summaryTableForVocabularyAndDatabaseList,
        usagiTibble = usagiTibble,
        sourceVocabularyId = sourceVocabularyId,
        pathToNewsFile = pathToNewsFile,
        outputFolderPath = validationResultsFolder
    )

    expect_true(file.exists(outputCoverageVocabularyDatabaseHtmlPath))
})
