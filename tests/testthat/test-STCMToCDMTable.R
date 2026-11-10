test_that("STCMToCDMTables creates CONCEPT entries from STCM Extended with correct domain recalculation", {
    # Setup test paths and parameters
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    vocabularyDatabaseSchema <- "main"
    sourceToConceptMapTable <- "source_to_concept_map_extended"

    # Create test data
    testData <- tibble::tribble(
        ~source_code, ~source_concept_id, ~source_vocabulary_id, ~source_code_description, ~target_concept_id, ~target_vocabulary_id, ~valid_start_date, ~valid_end_date, ~invalid_reason, ~source_concept_class, ~source_domain, ~source_parents_concept_ids,
        # domain recalculation
        "code1", 2000000001, "TestVocab", "Test Code 1 ConditionCondition", 141797, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        "code1", 2000000001, "TestVocab", "Test Code 1 ConditionCondition", 141797, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        "code2", 2000000002, "TestVocab", "Test Code 2 ConditionObservation", 141797, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        "code2", 2000000002, "TestVocab", "Test Code 2 ConditionObservation", 36713461, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        "code3", 2000000003, "TestVocab", "Test Code 3 ObservationCondition", 36713461, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        "code3", 2000000003, "TestVocab", "Test Code 3 ObservationCondition", 141797, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        "code4", 2000000004, "TestVocab", "Test Code 4 Observation", 36713461, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        "code5", 2000000005, "TestVocab", "Test Code 5 unmapped", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_,
        # no dates
        "code6", 2000000006, "TestVocab", "Test Code 6 no start date", 0, "SNOMED", as.Date(NA), as.Date("2099-12-31"), NA_character_, "Test concept class 2", "Condition", NA_character_,
        "code7", 2000000007, "TestVocab", "Test Code 7 no end date", 0, "SNOMED", as.Date("2023-01-01"), as.Date(NA), NA_character_, "Test concept class 2", "Condition", NA_character_,
        # parent concept ids
        "code8", 2000000008, "TestVocab", "Test Code 8 parent concept ids", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class 3", "Condition", "2000000001",
        "code9", 2000000009, "TestVocab", "Test Code 9 parent concept ids", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class 3", "Condition", "2000000001|2000000002",
        "code10", 2000000010, "TestVocab", "Test Code 10 parent concept ids", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class 3", "Condition", "2000000001|2000000002|2000000003"
    )
    testData <- testData |> dplyr::mutate(source_code_description_fi = NA_character_)

    # Create connection to test database
    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    # Create and populate STCM table
    createSourceToConceptMapExtended(
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable,
        overwrite = TRUE
    )

    DatabaseConnector::insertTable(
        connection = connection,
        databaseSchema = vocabularyDatabaseSchema,
        tableName = sourceToConceptMapTable,
        data = testData
    )

    # Execute the SQL
    STCMToCDMTables(
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable
    )

    # CONCEPT
    res <- dplyr::tbl(connection, "CONCEPT") |>
        dplyr::filter(vocabulary_id == "TestVocab") |>
        dplyr::arrange(concept_id) |>
        dplyr::collect()

    # general
    res  |> nrow() |> expect_equal(10)
    res  |> dplyr::pull(concept_id) |> expect_equal(c(2000000001, 2000000002, 2000000003, 2000000004, 2000000005, 2000000006, 2000000007, 2000000008, 2000000009, 2000000010))
    # domain_id
    res  |> dplyr::pull(domain_id) |> expect_equal(c("Condition", "Condition/Obs", "Condition/Obs", "Observation", "Condition", "Condition", "Condition", "Condition", "Condition", "Condition"))
    # dates
    res  |> dplyr::pull(valid_start_date) |> expect_equal(c(as.Date("2023-01-01"), as.Date("2023-01-01"), as.Date("2023-01-01"), as.Date("2023-01-01"), as.Date("2023-01-01"), as.Date('1970-01-01'), as.Date("2023-01-01"), as.Date("2023-01-01"), as.Date("2023-01-01"), as.Date("2023-01-01")))
    res  |> dplyr::pull(valid_end_date) |> expect_equal(c(as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31")))

    # CONCEPT_RELATIONSHIP
    # maps to
    res <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP")  |>
        dplyr::filter(relationship_id == "Maps to") |>
        dplyr::filter(concept_id_1 > 2000000000) |>
        dplyr::arrange(concept_id_1, concept_id_2) |>
        dplyr::collect()
    res |> nrow() |> expect_equal(6)
    res  |> dplyr::pull(relationship_id) |> expect_equal(rep("Maps to", 6))
    res  |> dplyr::pull(concept_id_2) |> expect_equal(c( 141797, 141797, 36713461, 141797, 36713461, 36713461))

    # maps from
    res <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP")  |>
        dplyr::filter(relationship_id == "Mapped from") |>
        dplyr::filter(concept_id_2 > 2000000000) |>
        dplyr::arrange(concept_id_2, concept_id_1) |>
        dplyr::collect()
    res |> nrow() |> expect_equal(6)
    res  |> dplyr::pull(relationship_id) |> expect_equal(rep("Mapped from", 6))
    res  |> dplyr::pull(concept_id_1) |> expect_equal(c( 141797, 141797, 36713461, 141797, 36713461, 36713461))

    # subsumes
    res <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP")  |>
        dplyr::filter(relationship_id == "Subsumes") |>
        dplyr::filter(concept_id_1 > 2000000000) |>
        dplyr::arrange(concept_id_1, concept_id_2) |>
        dplyr::collect()
    res |> nrow() |> expect_equal(6)
    res  |> dplyr::pull(concept_id_1) |> expect_equal(c(2000000001, 2000000001, 2000000002, 2000000001, 2000000002, 2000000003))
    res  |> dplyr::pull(concept_id_2) |> expect_equal(c(2000000008, 2000000009, 2000000009, 2000000010, 2000000010, 2000000010))


    # is a
    res <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP")  |>
        dplyr::filter(relationship_id == "Is a") |>
        dplyr::filter(concept_id_1 > 2000000000) |>
        dplyr::arrange(concept_id_1, concept_id_2) |>
        dplyr::collect()
    res |> nrow() |> expect_equal(6)
    res  |> dplyr::pull(concept_id_1) |> expect_equal(c(2000000008, 2000000009, 2000000009, 2000000010, 2000000010, 2000000010))
    res  |> dplyr::pull(concept_id_2) |> expect_equal(c(2000000001, 2000000001, 2000000002, 2000000001, 2000000002, 2000000003))

})

test_that("STCMToCDMTables creates Finnish CONCEPT_SYNONYM entries from source_code_description_fi", {
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    vocabularyDatabaseSchema <- "main"
    sourceToConceptMapTable <- "source_to_concept_map_extended"

    testData <- tibble::tribble(
        ~source_code, ~source_concept_id, ~source_vocabulary_id, ~source_code_description, ~target_concept_id, ~target_vocabulary_id, ~valid_start_date, ~valid_end_date, ~invalid_reason, ~source_concept_class, ~source_domain, ~source_parents_concept_ids, ~source_code_description_fi,
        # multi-mapped code, one synonym
        "code1", 2000000001, "TestVocab", "Test Code 1", 141797, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_, "Testikoodi 1",
        "code1", 2000000001, "TestVocab", "Test Code 1", 36713461, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_, "Testikoodi 1",
        # unmapped code also gets synonym
        "code2", 2000000002, "TestVocab", "Test Code 2", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_, "Testikoodi 2",
        # same name as concept_name is not added as synonym
        "code3", 2000000003, "TestVocab", "Test Code 3", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_, "Test Code 3",
        # no synonym
        "code4", 2000000004, "TestVocab", "Test Code 4 no fi", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_, NA_character_,
        "code5", 2000000005, "TestVocab", "Test Code 5 blank fi", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_, "  ",
        # source_concept_id 0 is skipped
        "code6", 0, "TestVocab", "Test Code 6 no source concept id", 0, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test concept class", "Condition", NA_character_, "Testikoodi 6"
    )

    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    createSourceToConceptMapExtended(
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable,
        overwrite = TRUE
    )
    DatabaseConnector::insertTable(
        connection = connection,
        databaseSchema = vocabularyDatabaseSchema,
        tableName = sourceToConceptMapTable,
        data = testData
    )

    # existing synonyms: an old Finnish one to be replaced and an English one to be kept
    DatabaseConnector::insertTable(
        connection = connection,
        databaseSchema = vocabularyDatabaseSchema,
        tableName = "CONCEPT_SYNONYM",
        data = tibble::tribble(
            ~concept_id, ~concept_synonym_name, ~language_concept_id,
            2000000001, "Vanha nimi", 4181730,
            2000000001, "Old English synonym", 4180186
        ),
        dropTableIfExists = FALSE,
        createTable = FALSE
    )

    # run twice to check it is idempotent
    for (i in 1:2) {
        STCMToCDMTables(
            connection = connection,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            sourceToConceptMapTable = sourceToConceptMapTable
        )
    }

    res <- dplyr::tbl(connection, "CONCEPT_SYNONYM") |>
        dplyr::filter(concept_id >= 2000000000 | concept_id == 0) |>
        dplyr::arrange(concept_id, language_concept_id) |>
        dplyr::collect()

    finnish <- res |> dplyr::filter(language_concept_id == 4181730)
    finnish |> dplyr::pull(concept_id) |> expect_equal(c(2000000001, 2000000002))
    finnish |> dplyr::pull(concept_synonym_name) |> expect_equal(c("Testikoodi 1", "Testikoodi 2"))

    # other languages are not touched
    res |> dplyr::filter(language_concept_id != 4181730) |> dplyr::pull(concept_synonym_name) |> expect_equal("Old English synonym")
})
