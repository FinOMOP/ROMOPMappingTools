test_that("test appendUsagiFileToSTCMTable appends the usagi file to the sourceToConceptMapTable for a non-extended sourceToConceptMapTable", {
    pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")
    nrowUsagiFile <- readUsagiFile(pathToUsagiFile)  |> nrow()
    nrowUsagiFileMapped <- readUsagiFile(pathToUsagiFile)  |> dplyr::filter(mappingStatus == "APPROVED") |> nrow()
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    vocabularyDatabaseSchema <- "main"
    sourceToConceptMapTable <- "source_to_concept_map"

    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    appendUsagiFileToSTCMtable(
        vocabularyId = "ICD10fi",
        pathToUsagiFile = pathToUsagiFile,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable
    ) |>
        expect_warning("The usagi file is extended but the sourceToConceptMapTable is not extended.")

    stcmTable <- DBI::dbReadTable(connection, sourceToConceptMapTable) |> tibble::as_tibble()
    stcmTable |>
        nrow() |>
        expect_equal(nrowUsagiFile)

    stcmTable |>
        dplyr::filter(TARGET_CONCEPT_ID != 0L) |>
        dplyr::count() |> dplyr::pull(n) |>
        expect_equal(nrowUsagiFileMapped)

    stcmTable |>
        names() |>
        stringr::str_to_lower() |>
        expect_equal(c(
            "source_code", "source_concept_id", "source_vocabulary_id", "source_code_description", "target_concept_id",
             "target_vocabulary_id", "valid_start_date", "valid_end_date", "invalid_reason"))
})

test_that("test appendUsagiFileToSTCMTable appends the usagi file to the sourceToConceptMapTable for an extended sourceToConceptMapTable", {
    pathToUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi.usagi.csv", package = "ROMOPMappingTools")
    nrowUsagiFile <- readUsagiFile(pathToUsagiFile)  |> nrow()
    nrowUsagiFileMapped <- readUsagiFile(pathToUsagiFile)  |> dplyr::filter(mappingStatus == "APPROVED") |> nrow()
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    vocabularyDatabaseSchema <- "main"

    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    # create an extended sourceToConceptMapTable
    sourceToConceptMapTable <- "source_to_concept_map_extended"
    createSourceToConceptMapExtended(connection, vocabularyDatabaseSchema, sourceToConceptMapTable)

    appendUsagiFileToSTCMtable(
        vocabularyId = "ICD10fi",
        pathToUsagiFile = pathToUsagiFile,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable
    ) 

    stcmTable <- DBI::dbReadTable(connection, sourceToConceptMapTable) |> tibble::as_tibble()
    stcmTable |>
        nrow() |>
        expect_equal(nrowUsagiFile)

    stcmTable |>
        dplyr::filter(TARGET_CONCEPT_ID != 0L) |>
        dplyr::count() |> dplyr::pull(n) |>
        expect_equal(nrowUsagiFileMapped)
        
    stcmTable |>
        names() |> 
        stringr::str_to_lower() |>
        expect_equal(c(
            "source_code", "source_concept_id", "source_vocabulary_id", "source_code_description", "target_concept_id",
             "target_vocabulary_id", "valid_start_date", "valid_end_date", "invalid_reason", "source_concept_class", 
             "source_domain", "source_parents_concept_ids"))

    stcmTable |>
        dplyr::filter(is.na(SOURCE_PARENTS_CONCEPT_IDS)) |>
        nrow() |>
        expect_equal(21)

})
