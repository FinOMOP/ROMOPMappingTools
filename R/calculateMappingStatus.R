

#' calculateMappingStatus
#'
#' @param path_to_vocabularies_coverage_file
#' @param omop_tables
#' @param databases_code_counts_tables
#' @param ignore_failed_rules
#' @param calculate_all_databases
#'
#' @return
#' @export
#'
#' @examples
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
