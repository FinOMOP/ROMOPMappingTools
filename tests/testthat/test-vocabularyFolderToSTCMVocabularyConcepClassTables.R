test_that("vocabularyFolderToSTCMVocabularyConcepClassTables creates STCM and vocabulary and concept class tables from valid vocabulary folder", {
    # Setup test paths
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
    pathToOMOPVocabularyDuckDBfile <- helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB()
    withr::defer(unlink(pathToOMOPVocabularyDuckDBfile))
    vocabularyDatabaseSchema <- "main"
    sourceToConceptMapTable <- "source_to_concept_map_extended"

    # Create connection to test database
    connection <- DatabaseConnector::connect(
        dbms = "duckdb",
        server = pathToOMOPVocabularyDuckDBfile
    )
    on.exit(DatabaseConnector::disconnect(connection))

    # Create SOURCE_TO_CONCEPT_MAP_EXTENDED table
    createSourceToConceptMapExtended(
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable,
        overwrite = TRUE
    )

    # Run the function
    vocabularyFolderToSTCMVocabularyConcepClassTables(
        pathToVocabularyFolder = pathToVocabularyFolder,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable
    )

    # Check that data was inserted correctly
    vocabularyId_ICD10fi <- 2000500000
    vocabularyId_UNITfi <- 2001800000
    
    stcmData <- connection |> dplyr::tbl(sourceToConceptMapTable) |> dplyr::collect()
    vocabulary <- connection |> dplyr::tbl("VOCABULARY")|> dplyr::collect()
    concept <- connection |> dplyr::tbl("CONCEPT")|> dplyr::collect()
    conceptClass <- connection |> dplyr::tbl("CONCEPT_CLASS")|> dplyr::collect()

    # vocabulary 
    vocabulary |> dplyr::filter(vocabulary_concept_id == vocabularyId_ICD10fi) |> dplyr::pull(vocabulary_version) |> expect_equal("v1.1.2")
    vocabulary |> dplyr::filter(vocabulary_concept_id == vocabularyId_UNITfi) |> dplyr::pull(vocabulary_version) |> expect_equal("v1.0.1")

    concept |> dplyr::filter(concept_id == vocabularyId_ICD10fi)  |> 
    dplyr::pull(concept_name) |> expect_equal("International Classification of Diseases 10th Revision Finnish extension")
    concept |> dplyr::filter(concept_id == vocabularyId_UNITfi)  |> 
    dplyr::pull(concept_name) |> expect_equal("Finnish Units vocabulary")

    # concept class
    conceptClass |> dplyr::filter(concept_class_concept_id > vocabularyId_ICD10fi & concept_class_concept_id < vocabularyId_ICD10fi+1000) |> 
    dplyr::arrange(concept_class_concept_id) |> 
    dplyr::pull(concept_class_name) |> expect_equal(c("ICD10fi Hierarchy", "ICD10fi 1 Code", "ICD10fi SubChapter"))
    conceptClass |> dplyr::filter(concept_class_concept_id > vocabularyId_UNITfi & concept_class_concept_id < vocabularyId_UNITfi+1000) |> 
    dplyr::arrange(concept_class_concept_id) |> 
    dplyr::pull(concept_class_name) |> expect_equal("UNITfi Level 0")

    concept |> dplyr::filter(concept_id > vocabularyId_ICD10fi & concept_id < vocabularyId_ICD10fi+1000) |> 
    dplyr::arrange(concept_id) |> 
    dplyr::pull(concept_name) |> expect_equal(c("ICD10fi Hierarchy", "ICD10fi 1 Code", "ICD10fi SubChapter"))
    concept |> dplyr::filter(concept_id > vocabularyId_UNITfi & concept_id < vocabularyId_UNITfi+1000) |> 
    dplyr::arrange(concept_id) |> 
    dplyr::pull(concept_name) |> expect_equal("UNITfi Level 0")


    # stcm
    expect_true(all(!is.na(stcmData$source_code)))
    expect_true(all(!is.na(stcmData$source_vocabulary_id)))
    expect_true(all(!is.na(stcmData$target_concept_id)))
    expect_true(all(!is.na(stcmData$valid_start_date)))
    expect_true(all(!is.na(stcmData$valid_end_date)))
    
}) 
