#' Upload Vocabulary Folder to CDM Tables
#'
#' Uploads all vocabulary files from a folder to the CDM tables. This includes:
#' - Adding vocabularies to the VOCABULARY table
#' - Adding concept classes to the CONCEPT_CLASS table
#' - Adding mappings to the Source to Concept Map table
#' - Creating corresponding CONCEPT entries
#'
#' The vocabulary folder must contain:
#' - vocabularies.csv: A file describing the vocabularies with columns:
#'   - source_vocabulary_id
#'   - source_vocabulary_name
#'   - source_concept_id_offset
#'   - path_to_usagi_file
#'   - path_to_news_file
#'   - ignore
#' - Usagi mapping files as referenced in vocabularies.csv
#' - NEWS files containing version information as referenced in vocabularies.csv
#'
#' @param pathToVocabularyFolder Path to folder containing vocabulary files
#' @param connection A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @param sourceToConceptMapTable Name of the source to concept map table
#' @param skipValidation Whether to skip validation of the Usagi files (default: TRUE)
#' @param pathToValidatedUsagiFolder Path where validated Usagi files will be saved if validation is performed
#'
#' @return NULL invisibly. The function modifies the database tables directly.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate transmute pull bind_rows distinct select
#' @importFrom purrr map_chr
#' @importFrom stringr str_extract
#' @importFrom lubridate ymd
#' @importFrom DatabaseConnector renderTranslateExecuteSql dbWriteTable
#' @importFrom tibble tibble
#'
#' @export
vocabularyFolderToSTCMVocabularyConcepClassTables <- function(
    pathToVocabularyFolder,
    connection,
    vocabularyDatabaseSchema,
    sourceToConceptMapTable,
    skipValidation = TRUE,
    pathToValidatedUsagiFolder = NULL
) {
    if (!skipValidation) {
        validationsLogTibble <- validateVocabularyFolder(pathToVocabularyFolder, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFolder)
        if (nrow(validationsLogTibble |> dplyr::filter(type != "SUCCESS")) > 0) {
            stop("Validation failed, use validateVocabularyFolder to see the errors")
        }
    }

    #
    # Vocabulary table
    #
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
            vocabulary_reference = "",
            vocabulary_version = vocabulary_version,
            vocabulary_concept_id = source_concept_id_offset
        )

    # delete rows if vocabulary id exists
    DatabaseConnector::renderTranslateExecuteSql(
        connection,
        "DELETE FROM @vocabulary_database_schema.VOCABULARY WHERE vocabulary_id IN (@vocabulary_ids)",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        vocabulary_ids = paste0("'", vocabularyTableToInsert |> dplyr::pull(vocabulary_id), "'")
    )

    # append the vocabularyTableToInsert into the vocabularyTable
    DatabaseConnector::dbWriteTable(connection, "VOCABULARY", vocabularyTableToInsert, append = TRUE)

    # create concept table for vocabulary entries
    conceptTableToInsert <- vocabularyTableToInsert |>
    dplyr::transmute(
        concept_id = vocabulary_concept_id,
        concept_name = vocabulary_name,
        domain_id = "Metadata",
        vocabulary_id = "Vocabulary",
        concept_class_id = "Vocabulary",
        standard_concept = NA_character_,
        concept_code = "FinOMOP generated",
        valid_start_date = lubridate::ymd("1900-01-01"),
        valid_end_date = lubridate::ymd("2099-12-31")
    )

    # delete rows from CONCEPT table where concept_id in vocabularyTableToInsert
    DatabaseConnector::renderTranslateExecuteSql(
        connection,
        "DELETE FROM @vocabulary_database_schema.CONCEPT WHERE concept_id IN (@concept_ids)",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_ids = paste0("'", conceptTableToInsert |> dplyr::pull(concept_id), "'")
    )

    # append the conceptTableToInsert into the conceptTable
    DatabaseConnector::dbWriteTable(connection, "CONCEPT", conceptTableToInsert, append = TRUE)
    

    #
    # Concept Class table
    #
    message("Appending info from Usagi files to Concept Class table")

    # collect all the concept classes from the Usagi files
    conceptClasses <- tibble::tibble()
    for (i in 1:nrow(vocabulariesTibble)) {
        message(paste0("Collecting concept classes from Usagi file ", vocabulariesTibble$path_to_usagi_file[i]))
        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        usagiTibble <- readUsagiFile(pathToUsagiFile)
        conceptClasse <- usagiTibble |>
            dplyr::select(`ADD_INFO:sourceConceptClass`) |>
            dplyr::distinct() |>
            dplyr::mutate(source_concept_id_offset = vocabulariesTibble$source_concept_id_offset[i])
        conceptClasses <- conceptClasse |> dplyr::bind_rows(conceptClasses)
    }

    conceptClassTableToInsert <- conceptClasses |>
        dplyr::transmute(
            concept_class_id = `ADD_INFO:sourceConceptClass`,
            concept_class_name = `ADD_INFO:sourceConceptClass`,
            concept_class_concept_id = source_concept_id_offset 
        ) |> 
        dplyr::group_by(concept_class_concept_id) |>
        dplyr::mutate(concept_class_concept_id = concept_class_concept_id + dplyr::row_number()) |>
        dplyr::ungroup()

    # delete rows if concept class id exists
    DatabaseConnector::renderTranslateExecuteSql(
        connection,
        "DELETE FROM @vocabulary_database_schema.CONCEPT_CLASS WHERE concept_class_id IN (@concept_class_ids)",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_class_ids = paste0("'", conceptClassTableToInsert |> dplyr::pull(concept_class_id), "'")
    )

    # append the conceptClassTableToInsert into the conceptClassTable
    DatabaseConnector::dbWriteTable(connection, "CONCEPT_CLASS", conceptClassTableToInsert, append = TRUE)

    # create concept table for concept classes
    conceptTableToInsert <- conceptClassTableToInsert |>
        dplyr::transmute(
            concept_id = concept_class_concept_id,
            concept_name = concept_class_name,
            domain_id = "Metadata",
            vocabulary_id = "Concept Class",
            concept_class_id = "Concept Class",
            standard_concept = NA_character_,
            concept_code = "FinOMOP generated",
            valid_start_date = lubridate::ymd("1900-01-01"),
            valid_end_date = lubridate::ymd("2099-12-31")
        )

    # delete rows from CONCEPT table where concept_id in conceptClassTableToInsert
    DatabaseConnector::renderTranslateExecuteSql(
        connection,
        "DELETE FROM @vocabulary_database_schema.CONCEPT WHERE concept_id IN (@concept_ids)",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_ids = paste0("'", conceptTableToInsert |> dplyr::pull(concept_id), "'")
    )

    # append the conceptClassTableToInsert into the conceptClassTable
    DatabaseConnector::dbWriteTable(connection, "CONCEPT", conceptTableToInsert, append = TRUE)

    #
    # STCM table
    #
    message("Appending Usagi files to STCM table")

    for (i in 1:nrow(vocabulariesTibble)) {
        message(paste0("Appending Usagi file ", vocabulariesTibble$path_to_usagi_file[i]))
        vocabularyId <- vocabulariesTibble$source_vocabulary_id[i]
        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        appendUsagiFileToSTCMtable(vocabularyId, pathToUsagiFile, connection, vocabularyDatabaseSchema, sourceToConceptMapTable, skipValidation = TRUE)
    }
}
