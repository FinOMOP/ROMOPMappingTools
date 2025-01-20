test_that("vocabularyFolderToSTCM creates STCM table from valid vocabulary folder", {
    # Setup test paths
    pathToVocabularyFolder <- system.file("testdata/VOCABULARIES", package = "ROMOPMappingTools")
    pathToOMOPVocabularyDuckDBfile <- system.file("testdata/OMOPVocabularyICD10only/OMOPVocabularyICD10only.duckdb", package = "ROMOPMappingTools")
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
    vocabularyFolderToSTCMtable(
        pathToVocabularyFolder = pathToVocabularyFolder,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable
    )

    # Check that data was inserted correctly
    stcmData <- DatabaseConnector::dbGetQuery(
        connection,
        sprintf("SELECT * FROM %s.%s", vocabularyDatabaseSchema, sourceToConceptMapTable)
    )

    # Verify table has data
    expect_true(nrow(stcmData) > 0)

    # Check that expected vocabularies are present
    vocabularies <- unique(stcmData$source_vocabulary_id)
    expect_true(all(c("ICD10fi", "UNITfi") %in% vocabularies))

    # Check that required columns have no NULL values
    expect_true(all(!is.na(stcmData$source_code)))
    expect_true(all(!is.na(stcmData$source_vocabulary_id)))
    expect_true(all(!is.na(stcmData$target_concept_id)))
    expect_true(all(!is.na(stcmData$valid_start_date)))
    expect_true(all(!is.na(stcmData$valid_end_date)))

    
}) 
