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

    # TODO: there is a warning that needs to be fixed
    suppressWarnings(
        validationLogTibble <- buildStatusDashboard(
            pathToCodeCountsFolder = pathToCodeCountsFolder,
            connectionDetails = connectionDetails,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            output_file_html = output_file_html
        )
    )

    # Check results
    validationLogTibble |>
        dplyr::filter(type == "ERROR") |>
        nrow() |>
        expect_equal(0)

    # check the validation results folder
    expect_true(file.exists(output_file_html))
})



test_that("internal functions works", {
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

    outputCoverageVocabularyDatabaseHtmlPath <- .pageCoverageVocabularyDatabase(
        summaryTableForVocabularyAndDatabaseList = summaryTableForVocabularyAndDatabaseList,
        usagiTibble = usagiTibble,
        sourceVocabularyId = sourceVocabularyId
    )

    expect_true(file.exists(outputCoverageVocabularyDatabaseHtml))

    
})


test_that(".plotTableForUsagiFile works", {

    pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")

    usagiTibble <- readUsagiFile(pathToUsagiFile)
    plotTable <- .plotTableForUsagiFile(usagiTibble)

    expect_s3_class(plotTable, "reactable")

    plotSummaryTable <- .plotSummaryTableForUsagiFile(usagiTibble)
    expect_s3_class(plotSummaryTable, "reactable")


})

for (database in names(summaryTableForVocabularyAndDatabaseList)) {
  cat(paste0( database, "\n"))
  cat(paste0("=====================================\n"))

    summaryTableForVocabularyAndDatabase <- summaryTableForVocabularyAndDatabaseList[[database]]

  cat("Column {.tabset}\n")
  cat("-----------------------------------------------------------------------\n")

  cat("### Summary\n")
  .plotSummaryTableForVocabularyAndDatabase(summaryTableForVocabularyAndDatabase)

  cat("### Coverage\n")
  .plotTableForVocabularyAndDatabase(summaryTableForVocabularyAndDatabase)

  
}
