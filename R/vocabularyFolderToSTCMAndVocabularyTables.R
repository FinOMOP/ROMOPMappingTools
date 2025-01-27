vocabularyFolderToSTCMAndVocabularyTables <- function(pathToVocabularyFolder, connection, vocabularyDatabaseSchema, sourceToConceptMapTable, skipValidation = TRUE) {
    if (!skipValidation) {
        validationsLogTibble <- validateVocabularyFolder(pathToVocabularyFolder, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFolder)
        if (nrow(validationsLogTibble |> dplyr::filter(type != "SUCCESS")) > 0) {
            stop("Validation failed, use validateVocabularyFolder to see the errors")
        }
    }

    message("Appending vocabularies.csv to VOCABULARY table")
    vocabulariesTibble <- readr::read_csv(file.path(pathToVocabularyFolder, "vocabularies.csv"), show_col_types = FALSE)

    # get all the vocabulary references from the NEWS file
    vocabularyTableToInsert <- vocabulariesTibble |>
        dplyr::mutate(
            vocabulary_version = purrr::map_chr(path_to_news_file, ~ {
                readLines(file.path(pathToVocabularyFolder, .x), n = 1) |>
                    stringr::str_extract("v[0-9]+\\.[0-9]+\\.[0-9]+")
            })
        ) |>
        dplyr::transmute(
            vocabulary_id = source_vocabulary_id,
            vocabulary_name = source_vocabulary_name,
            vocabulary_reference = '',
            vocabulary_version = vocabulary_version,
            vocabulary_concept_id = 0L
        )

    # delete rows if vocabulary id exists
    DatabaseConnector::renderTranslateExecuteSql(
        connection,
        "DELETE FROM @vocabulary_database_schema.VOCABULARY WHERE vocabulary_id IN (@vocabulary_ids)",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        vocabulary_ids = paste("'", vocabularyTableToInsert |> dplyr::pull(vocabulary_id), "'")
    )

    # insert the STCMTableToInsert into the sourceToConceptMapTable
    DatabaseConnector::dbWriteTable(connection, "VOCABULARY", vocabularyTableToInsert, append = TRUE)

    for (i in 1:nrow(vocabulariesTibble)) {
        message(paste0("Appending Usagi file ", vocabulariesTibble$path_to_usagi_file[i]))
        vocabularyId <- vocabulariesTibble$source_vocabulary_id[i]
        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        appendUsagiFileToSTCMtable(vocabularyId, pathToUsagiFile, connection, vocabularyDatabaseSchema, sourceToConceptMapTable, skipValidation = TRUE)
    }
}
