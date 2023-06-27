
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
#' @param path_to_vocabularies_coverage_file The path to the vocabularies coverage file.
#' @param connection_details_omop_tables The connection details for the OMOP tables.
#' @param databases_code_counts_tables The tables containing the code counts for different databases.
#' @param ignore_failed_rules Logical value indicating whether to ignore databases with failed rules (default is FALSE).
#' @param calculate_all_databases Logical value indicating whether to calculate code counts for all databases (default is TRUE).
#' @return A list containing two tables: `concepts_to_match` and `code_counts_matched`, representing the mapping status for code counts.
#'
#' @importFrom checkmate assertFileExists assertTibble
#' @importFrom dplyr bind_rows case_when filter group_by left_join mutate rename select semi_join summarize ungroup
#' @importFrom stringr str_c
#' @importFrom SqlRender render
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @importFrom DatabaseConnector connect dbGetQuery disconnect
#'
#' @export
calculateMappingStatus <- function(
    path_to_vocabularies_coverage_file,
    connection_details_omop_tables,
    databases_code_counts_tables,
    ignore_failed_rules = FALSE,
    calculate_all_databases = TRUE) {


  ###
  ## validate input parameters
  ###
  path_to_input_vocabularies_coverage_file |>  checkmate::assertFileExists()
  vocabs_coverage <- readTable(path_to_vocabularies_coverage_file, table_type = "VocabulariesCoverage")

  vocabs_coverage_validation <- validateTable(vocabs_coverage, table_type = "VocabulariesCoverage")

  .stopIfValidationErrors(vocabs_coverage_validation, "VocabulariesCoverage", path_to_vocabularies_coverage_file)

  databases_code_counts_tables |>  checkmate::assertTibble()
  databases_code_counts_tables |> names() |> checkmate::assertSubset(c("name", "table", "validation_summary", "failed_rules_table", "n_failed_rules" ))

  ###
  ## function
  ###


  # CREATE TABLE concepts_to_match
  # Collect concepts in OMOP with mapping relationship and synonyms

  connection <- DatabaseConnector::connect(connection_details_omop_tables)

  sql <- "
      SELECT
        c.vocabulary_id AS vocabulary_id,
        c.concept_code AS concept_code,
        c.concept_name AS concept_name,
        cs.concept_synonym_name AS concept_name_fi,
        c.concept_id AS concept_id,
        cr.concept_id_2 AS concept_id_2
      FROM (
        SELECT * FROM @cdmDatabaseSchema.CONCEPT
        WHERE vocabulary_id IN (@list_vocabulary_ids)
      ) AS c
      LEFT JOIN (
        SELECT concept_id_1, concept_id_2  FROM @cdmDatabaseSchema.CONCEPT_RELATIONSHIP
        WHERE relationship_id = 'Maps to'
      ) AS cr
        ON c.concept_id = cr.concept_id_1
      LEFT JOIN (
        SELECT concept_id, concept_synonym_name  FROM @cdmDatabaseSchema.CONCEPT_SYNONYM
        WHERE language_concept_id == 4181730
      ) AS cs
        ON c.concept_id = cs.concept_id

  "

  sql <- SqlRender::render(
    sql = sql,
    cdmDatabaseSchema = "main",
    list_vocabulary_ids = vocabs_coverage |> dplyr::pull(target_vocabulary_ids) |> sQuote(q=FALSE) |> paste0(collapse = ", ")
  )

  vocabs_info <- DatabaseConnector::dbGetQuery(connection, sql) |>  tibble::as_tibble()

  DatabaseConnector::disconnect(connection)


  concepts_to_match <- vocabs_coverage |>
    dplyr::mutate(vocabulary_id = target_vocabulary_ids ) |>
    dplyr::left_join(
      vocabs_info |>
        dplyr::group_by(vocabulary_id, concept_code, concept_name, concept_name_fi, concept_id) |>
        dplyr::summarise(standard_concept_ids = stringr::str_c(concept_id_2, collapse = ", "), .groups="drop"),
      by = "vocabulary_id",
      multiple = "all"
    ) |>
    dplyr::mutate( has_mapping = dplyr::if_else(is.na(standard_concept_ids), FALSE, TRUE))


  # CREATE TABLE all_code_counts
  # one table with all code counts in vocabs_coverage
  if(ignore_failed_rules==FALSE){
    # remove database with import errors
    databases_code_counts_tables <- databases_code_counts_tables |>
      dplyr::filter(n_failed_rules==0)
  }
  all_code_counts <- databases_code_counts_tables |>
    dplyr::select(database_name = name, table) |>
    tidyr::unnest(table) |>
    dplyr::semi_join(vocabs_coverage, by="source_vocabulary_id")

  # Create a new database called "all_databases with the normalised the n_events in all databass
  if(calculate_all_databases==TRUE){
    all_code_counts <- dplyr::bind_rows(
      all_code_counts,
      all_code_counts |>
        dplyr::group_by(database_name, source_vocabulary_id ) |>
        dplyr::mutate(n_total_events = sum(n_events)) |>
        # normalised counts to per 1 000 000
        dplyr::mutate( n_events_norm = round(n_events/n_total_events*1000000)) |>
        # average
        dplyr::group_by(source_vocabulary_id, source_code ) |>
        dplyr::summarise(n_events = mean(n_events_norm),  .groups="drop") |>
        # add database name
        dplyr::mutate(database_name="all_databases")
    )
  }

  # calcualte percentage of codes per database and vocab
  all_code_counts <- all_code_counts |>
    dplyr::group_by(database_name, source_vocabulary_id) |>
    dplyr::mutate(per_events=n_events/sum(n_events)*100) |>
    dplyr::ungroup()



  ## CALCULATE MAPPINGS
  # calculate mapping status
  code_counts_matched <- all_code_counts |>
    dplyr::left_join(
      concepts_to_match |> dplyr::rename(source_code = concept_code),
      by = c("source_vocabulary_id", "source_code")
    ) |>
    dplyr::mutate( has_code = ifelse(is.na(concept_id), FALSE, TRUE)) |>
    dplyr::mutate(mapping_status = dplyr::case_when(
      has_code & has_mapping ~ "mapped",
      has_code & !has_mapping ~ "no_mapping",
      !has_code  ~ "no_code"
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
