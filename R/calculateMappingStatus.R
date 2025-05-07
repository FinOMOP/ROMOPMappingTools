#' calculateMappingStatus
#'
#' Calculates the mapping status for code counts in different databases and vocabularies.
#'
#' This function calculates the mapping status for code counts in different databases and vocabularies. The calculation involves the following steps:
#'
#' 1. Validating the input parameters to ensure they meet the required format.
#' 2. Reading and validating the provided vocabularies coverage file.
#' 3. Creating a table called `concepts_to_match` that collects concepts in OMOP with mapping relationships and synonyms.
#' 4. Creating a table called `all_code_counts` that contains all code counts in the vocabularies coverage, optionally ignoring databases with failed rules.
#' 5. Creating a new database called "all_databases" with normalized event counts across all databases.
#' 6. Calculating the percentage of codes per database and vocabulary.
#' 7. Calculating the mapping status for code counts based on whether they have mappings and codes.
#' 8. Returning the mapping status information in the form of two tables: `concepts_to_match` and `code_counts_matched`.
#'
#' @param pathToCodeCountsFolder The path to the vocabularies coverage file.
#' @param connectionDetails The connection details for the OMOP tables.
#' @param vocabularyDatabaseSchema The database schema for the OMOP vocabulary.
#' @param includeCountForAllDatabases Logical value indicating whether to include code counts for all databases (default is TRUE).
#' @param skipValidation Logical value indicating whether to skip validation of the input parameters (default is TRUE).
#' @return A list containing two tables: `concepts_to_match` and `code_counts_matched`, representing the mapping status for code counts.
#'
#' @importFrom dplyr bind_rows case_when filter group_by left_join mutate rename select semi_join summarize ungroup
#' @importFrom stringr str_c
#' @importFrom SqlRender render
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @importFrom DatabaseConnector connect dbGetQuery disconnect
#'
#' @export
calculateMappingStatus <- function(
    pathToCodeCountsFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    includeCountForAllDatabases = TRUE,
    skipValidation = TRUE) {
    ###
    ## validate input parameters
    ###
    includeCountForAllDatabases |> checkmate::assertLogical()
    skipValidation |> checkmate::assertLogical()

    if (skipValidation == FALSE) {
        validationLogTibble <- pathToCodeCountsFolder |> validateCodeCountsFolder()
        if (validationLogTibble |> dplyr::filter(type != "SUCCESS") |> nrow() > 0) {
            stop("Validation errors in the code counts folder")
        }
    }

    connectionDetails |> checkmate::assertClass("ConnectionDetails")
    vocabularyDatabaseSchema |> checkmate::assertString()
    ###
    ## function
    ###
    
    databasesCoverageTibble <- readr::read_csv(pathToCodeCountsFolder |> file.path("databases_coverage.csv"), show_col_types = FALSE) |>
        dplyr::filter(ignore == FALSE)

    vocabulariesCoverageTibble <- readr::read_csv(pathToCodeCountsFolder |> file.path("vocabularies_coverage.csv"), show_col_types = FALSE) |>
        dplyr::filter(ignore == FALSE) |>
        dplyr::mutate(target_vocabulary_ids = purrr::map(target_vocabulary_ids, ~ str_split(.x, "\\|") |>
            unlist() |>
            unique())) |>
        tidyr::unnest(target_vocabulary_ids)

    #
    # - Get info of the mappings from the OMOP vocabulary
    #



    concepts_to_match <- vocabulariesCoverageTibble |>
        dplyr::mutate(vocabulary_id = target_vocabulary_ids) |>
        dplyr::left_join(
            vocabulariesDBInfo |>
                dplyr::group_by(vocabulary_id, concept_code, concept_name, concept_name_fi, concept_id) |>
                dplyr::summarise(standard_concept_ids = stringr::str_c(concept_id_2, collapse = ", "), .groups = "drop"),
            by = "vocabulary_id",
            multiple = "all"
        ) |>
        dplyr::mutate(has_mapping = dplyr::if_else(is.na(standard_concept_ids), FALSE, TRUE))


    #
    # - Create a table with all code counts in vocabulariesCoverageTibble
    #
    databasesCountsTibble <- tibble::tibble()
    for (i in 1:nrow(databasesCoverageTibble)) {
        pathToCodeCountsFile <- file.path(pathToCodeCountsFolder, databasesCoverageTibble$path_to_code_counts_file[i])

        databaseCountsTibble <- readr::read_csv(pathToCodeCountsFile, show_col_types = FALSE)
        databasesCountsTibble <- databasesCountsTibble |>
            dplyr::bind_rows(databaseCountsTibble |>
                dplyr::mutate(
                    database_name = databasesCoverageTibble$database_name[i]
                ))
    }
    databasesCountsTibble <- databasesCountsTibble |>
        dplyr::select(database_name, source_vocabulary_id, source_code, n_events)

    # create a new database called "all_databases" with normalised counts to per 1 000 000
    if (includeCountForAllDatabases == TRUE) {
        databasesCountsTibble <- dplyr::bind_rows(
            databasesCountsTibble,
            databasesCountsTibble |>
                dplyr::group_by(database_name, source_vocabulary_id) |>
                dplyr::mutate(n_total_events = sum(n_events)) |>
                # normalised counts to per 1 000 000
                dplyr::mutate(n_events_norm = round(n_events / n_total_events * 1000000)) |>
                # average
                dplyr::group_by(source_vocabulary_id, source_code) |>
                dplyr::summarise(n_events = mean(n_events_norm), .groups = "drop") |>
                # add database name
                dplyr::mutate(database_name = "all_databases")
        )
    }

    # calcualte percentage of codes per database and vocab
    databasesCountsTibble <- databasesCountsTibble |>
        dplyr::group_by(database_name, source_vocabulary_id) |>
        dplyr::mutate(per_events = n_events / sum(n_events) * 100) |>
        dplyr::ungroup()



    #
    # - Calculate mapping status
    #
    code_counts_matched <- databasesCountsTibble |>
        dplyr::left_join(
            concepts_to_match |> dplyr::rename(source_code = concept_code),
            by = c("source_vocabulary_id", "source_code")
        ) |>
        dplyr::mutate(has_code = ifelse(is.na(concept_id), FALSE, TRUE)) |>
        dplyr::mutate(mapping_status = dplyr::case_when(
            has_code & has_mapping ~ "mapped",
            has_code & !has_mapping ~ "no_mapping",
            !has_code ~ "no_code"
        )) |>
        dplyr::select(
            database_name, source_vocabulary_id, target_vocabulary_ids, vocabulary_id, mantained_by,
            source_code, concept_name, concept_name_fi,
            n_events, per_events,
            mapping_status, standard_concept_ids
        )

    mapping_status <- list(
        concepts_to_match = concepts_to_match,
        code_counts_matched = code_counts_matched
    )

    return(mapping_status)
}
