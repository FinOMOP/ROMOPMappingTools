test_that("conceptRelationshipToAncestorTables creates CONCEPT_ANCESTOR table from CONCEPT_RELATIONSHIP table from ICD10", {
    # Setup test paths and parameters
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    vocabularyDatabaseSchema <- "main"

    # Create connection to test database
    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    # conceptRelationshipToAncestorTables
    conceptRelationshipToAncestorTables(
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        vocabularyList = c("ICD10")
    )

    # Check if CONCEPT_ANCESTOR table exists
    ancestor <- dplyr::tbl(connection, "CONCEPT_ANCESTOR") |>
        dplyr::collect()

    ancestor |> nrow() |> expect_gt(0)

    # check icd10 for asthma  45596282
    asthmaChildren <- ancestor |>
        dplyr::filter(ancestor_concept_id == 45596282) |>
        dplyr::collect()

    asthmaChildren |> nrow() |> expect_equal(5)
    asthmaChildren |> dplyr::pull(descendant_concept_id) |> expect_setequal(c(45548118, 45557624, 45557625, 45562456, 45596282))

    # check descendant Chronic lower respiratory diseases 40475107
    crdDescendant <- ancestor |>
        dplyr::filter(ancestor_concept_id == 40475107) |>
        dplyr::collect()

    crdDescendant |> nrow() |> expect_equal(25)
    crdDescendant |> dplyr::count(min_levels_of_separation, max_levels_of_separation)  |>
    dplyr::pull(n) |>
    expect_equal(c(1,8,16))

})
