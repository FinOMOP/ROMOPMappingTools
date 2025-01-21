test_that("STCMToCDMTables creates vocabulary entries from STCM", {
    # Setup test paths and parameters
    pathToOMOPVocabularyDuckDBfile <- system.file(
        "testdata/OMOPVocabularyICD10only/OMOPVocabularyICD10only.duckdb", 
        package = "ROMOPMappingTools"
    )
    vocabularyDatabaseSchema <- "main"
    sourceToConceptMapTable <- "source_to_concept_map_extended"

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

    # Add test data to STCM
    testData <- data.frame(
        source_code = c("TEST1", "TEST2"),
        source_concept_id = c(2000000001, 2000000002),
        source_vocabulary_id = c("TestVocab", "TestVocab"),
        source_code_description = c("Test Code 1", "Test Code 2"),
        target_concept_id = c(4000001, 4000002),
        target_vocabulary_id = c("SNOMED", "SNOMED"),
        valid_start_date = as.Date(c("2023-01-01", "2023-01-01")),
        valid_end_date = as.Date(c("2099-12-31", "2099-12-31")),
        invalid_reason = NA_character_,
        source_concept_class = c("Test", "Test"),
        source_domain = c("Condition", "Condition"),
        source_parents_concept_ids = NA_character_
    )
    
    DatabaseConnector::insertTable(
        connection = connection,
        tableName = sourceToConceptMapTable,
        databaseSchema = vocabularyDatabaseSchema,
        data = testData,
        dropTableIfExists = FALSE,
        createTable = FALSE,
        tempTable = FALSE
    )

    on.exit(DatabaseConnector::dbRemoveTable(connection, sourceToConceptMapTable))

    # Run function
    STCMToCDMTables(
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        sourceToConceptMapTable = sourceToConceptMapTable
    )
 
    # Check VOCABULARY table
    vocabularyData <- DatabaseConnector::dbGetQuery(
        connection,
        sprintf("SELECT * FROM %s.VOCABULARY WHERE vocabulary_id = 'TestVocab'", 
                vocabularyDatabaseSchema)
    )

    expect_equal(nrow(vocabularyData), 1)
    expect_equal(vocabularyData$vocabulary_id, "TestVocab")
    expect_equal(vocabularyData$vocabulary_name, "TestVocab")
    expect_equal(vocabularyData$vocabulary_concept_id, 0)

    # Check that old vocabulary entries were removed
    oldVocabCount <- DatabaseConnector::dbGetQuery(
        connection,
        sprintf("
            SELECT COUNT(*) as count 
            FROM %s.VOCABULARY 
            WHERE vocabulary_id = 'TestVocab' 
            AND vocabulary_concept_id != 0", 
            vocabularyDatabaseSchema)
    )
    expect_equal(oldVocabCount$count, 0)

    # Check CONCEPT table
    conceptData <- DatabaseConnector::dbGetQuery(
        connection,
        sprintf("SELECT * FROM %s.CONCEPT WHERE vocabulary_id = 'TestVocab'", 
                vocabularyDatabaseSchema)
    )
})

test_that("STCMToCDMTables creates CONCEPT entries from STCM Extended with correct domain recalculation", {
  # Create test data
    testData <- tibble::tribble(
        ~source_code, ~source_concept_id, ~source_vocabulary_id, ~source_code_description, ~target_concept_id, ~target_vocabulary_id, ~valid_start_date, ~valid_end_date, ~invalid_reason, ~source_concept_class, ~domain_id, ~source_parents_concept_ids,
        "code1", 2000000001, "TestVocab", "Test Code 1 ConditionCondition", 257581, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test", "Condition", NA_character_,
        "code1", 2000000001, "TestVocab", "Test Code 1 ConditionCondition", 257581, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test", "Condition", NA_character_,
        "code2", 2000000002, "TestVocab", "Test Code 2 ConditionObservation", 257581, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test", "Condition", NA_character_,
        "code2", 2000000002, "TestVocab", "Test Code 2 ConditionObservation", 4235703 465, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test", "Condition", NA_character_,
        "code3", 2000000003, "TestVocab", "Test Code 3", 4000005, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test", "Condition", NA_character_,
        "code3", 2000000003, "TestVocab", "Test Code 3", 4000006, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test", "Condition", NA_character_,
        "code4", 2000000004, "TestVocab", "Test Code 4", 4000007, "SNOMED", as.Date("2023-01-01"), as.Date("2099-12-31"), NA_character_, "Test", "Condition", NA_character_
    )
  
  # Write test data to temp database
  DBI::dbWriteTable(connection, "#temp_stcm", testData, temporary = TRUE)
  
  # Execute the SQL
  DatabaseConnector::executeSql(connection, SqlRender::render(
    readSql("inst/sql/sql_server/STCMExtendedToCDM.sql"),
    vocabularyDatabaseSchema = "vocab",
    sourceToConceptMapTable = "#temp_stcm"
  ))
  
  # Get results
  results <- DBI::dbGetQuery(connection, "
    SELECT source_code, final_domain_id 
    FROM #temp_results 
    ORDER BY source_code
  ")
  
  # Check expectations
  expected_results <- tibble::tribble(
    ~source_code, ~final_domain_id,
    "code1", "Condition/Device",
    "code2", "Condition/Meas",
    "code3", "Drug/Procedure",
    "code4", "Observation"
  )
  
  expect_equal(results, expected_results)
}) 
