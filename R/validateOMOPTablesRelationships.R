

#' validateOMOPTablesRelationships
#'
#' @param tibble_with_tables
#'
#' @return
#' @export
#'
#' @examples
validateOMOPTablesRelationships <- function(
    tibble_with_tables
){


  ###
  ## validate input parameters
  ###
  tibble_with_tables |> checkmate::assertTibble()
  checkmate::assertSubset(
    c("CONCEPT","VOCABULARY", "CONCEPT_CLASS", "CONCEPT_RELATIONSHIP", "CONCEPT_SYNONYM", "DOMAIN"),
    tibble_with_tables |> pull(name)
  )

  ###
  ## function
  ###
  validation_summary <- tibble::tibble()
  failed_rules_table <- tibble::tibble()

  CONCEPT <- omop_tables |>
    dplyr::filter(name == "CONCEPT") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    dplyr::mutate(row = dplyr::row_number())

  # check vocabulary_ids in CONCEPT  in VOCABULARY
  vocabulary_ids_in_VOCABULARY <- omop_tables |>
    dplyr::filter(name == "VOCABULARY") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    distinct(vocabulary_id)

  vocabulary_ids_in_CONCEPT_notin_VOCABULARY <- CONCEPT |>
    dplyr::anti_join(vocabulary_ids_in_VOCABULARY, by="vocabulary_id")

  validation_summary <- dplyr::bind_rows(
    validation_summary,
    tibble::tibble(
      name = "vocabulary_ids in CONCEPT is in VOCABULARY",
      items = nrow(CONCEPT),
      passes = nrow(CONCEPT)-nrow(vocabulary_ids_in_CONCEPT_notin_VOCABULARY),
      fails=    nrow(vocabulary_ids_in_CONCEPT_notin_VOCABULARY),
      nNA = 0,
      error = fails>0,
      warning = FALSE,
      expression = " CONCEPT |> dplyr::anti_join(vocabulary_ids_in_VOCABULARY, by='vocabulary_id')"
    )
  )

  failed_rules_table <- dplyr::bind_rows(
    failed_rules_table,
    vocabulary_ids_in_CONCEPT_notin_VOCABULARY |>
      dplyr::mutate( name = "vocabulary_ids in CONCEPT is in VOCABULARY")
  )


  # check concept_class_ids in CONCEPT  in CONCEPT_CLASS
  concept_class_ids_in_CONCEPT_CLASS <- omop_tables |>
    dplyr::filter(name == "CONCEPT_CLASS") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    distinct(concept_class_id)

  concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS <- CONCEPT |>
    dplyr::anti_join(concept_class_ids_in_CONCEPT_CLASS, by="concept_class_id")

  validation_summary <- dplyr::bind_rows(
    validation_summary,
    tibble::tibble(
      name = "concept_class_ids in CONCEPT in CONCEPT_CLASS",
      items = nrow(CONCEPT),
      passes = nrow(CONCEPT)-nrow(concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS),
      fails=    nrow(concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS),
      nNA = 0,
      error = fails>0,
      warning = FALSE,
      expression = " CONCEPT |> dplyr::anti_join(concept_class_ids_in_CONCEPT_CLASS, by='concept_class_id')"
    )
  )

  failed_rules_table <- dplyr::bind_rows(
    failed_rules_table,
    concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS |>
      dplyr::mutate( name = "concept_class_ids in CONCEPT in CONCEPT_CLASS")
  )


  # check domain_id in CONCEPT is in  DOMAIN
  domain_ids_in_DOMAIN <- omop_tables  |>
    dplyr::filter(name == "DOMAIN") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    distinct(domain_id)


  domain_ids_in_CONCEPT_notin_DOMAIN <- CONCEPT |>
    dplyr::anti_join(domain_ids_in_DOMAIN, by="domain_id")

  validation_summary <- dplyr::bind_rows(
    validation_summary,
    tibble::tibble(
      name = "domain_id in CONCEPT is in DOMAIN",
      items = nrow(CONCEPT),
      passes = nrow(CONCEPT)-nrow(domain_ids_in_CONCEPT_notin_DOMAIN),
      fails=    nrow(domain_ids_in_CONCEPT_notin_DOMAIN),
      nNA = 0,
      error = fails>0,
      warning = FALSE,
      expression = " CONCEPT |> dplyr::anti_join(domain_ids_in_DOMAIN, by='domain_id')"
    )
  )

  failed_rules_table <- dplyr::bind_rows(
    failed_rules_table,
    domain_ids_in_CONCEPT_notin_DOMAIN |> dplyr::mutate(name = "domain_id in CONCEPT is in DOMAIN")
  )


  # return
  n_failed_rules <- nrow(failed_rules_table)


  tibble_with_tables <- dplyr::bind_rows(
    tibble_with_tables,
    tibble::tibble(
      name = "cross tables rules",
      table = list(tibble::tibble()),
      validation_summary = list(validation_summary),
      failed_rules_table = list(failed_rules_table),
      n_failed_rules = n_failed_rules
    )
  )



  return(tibble_with_tables)

}
