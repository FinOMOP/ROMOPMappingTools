

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
    c("CONCEPT","VOCABULARY", "CONCEPT_CLASS", "CONCEPT_RELATIONSHIP", "CONCEPT_SYNONYM"),
    tibble_with_tables |> pull(name)
  )

  ###
  ## function
  ###
  validation_summary <- tibble::tibble()
  failed_rules_table <- tibble::tibble()

  # check vocabulary_ids in CONCEPT  in VOCABULARY
  vocabulary_ids_in_CONCEPT <- omop_tables |>
    dplyr::filter(name == "CONCEPT") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    distinct(vocabulary_id) |>  pull()
  vocabulary_ids_in_VOCABULARY <- omop_tables |>
    dplyr::filter(name == "VOCABULARY") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    distinct(vocabulary_id) |>  pull()

  vocabulary_ids_in_CONCEPT_notin_VOCABULARY <- setdiff( vocabulary_ids_in_CONCEPT, vocabulary_ids_in_VOCABULARY)


  validation_summary <- dplyr::bind_rows(
    validation_summary,
    tibble::tibble(
      name = "vocabulary_ids in CONCEPT in VOCABULARY",
      items = length(vocabulary_ids_in_CONCEPT),
      passes = length(vocabulary_ids_in_CONCEPT)-length(vocabulary_ids_in_CONCEPT_notin_VOCABULARY),
      fails=    length(vocabulary_ids_in_CONCEPT_notin_VOCABULARY),
      nNA = 0,
      error = fails>0,
      warning = FALSE,
      expression = "setdiff( vocabulary_ids_in_CONCEPT, vocabulary_ids_in_VOCABULARY)"
    )
  )

  failed_rules_table <- dplyr::bind_rows(
    failed_rules_table,
    tibble::tibble(
      fails = vocabulary_ids_in_CONCEPT_notin_VOCABULARY,
      name = "vocabulary_ids in CONCEPT in VOCABULARY"
    )
  )


  # check concept_class_ids in CONCEPT  in CONCEPT_CLASS
  concept_class_ids_in_CONCEPT <- omop_tables |>
    dplyr::filter(name == "CONCEPT") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    distinct(concept_class_id) |>  pull()
  concept_class_ids_in_CONCEPT_CLASS <- omop_tables |>
    dplyr::filter(name == "CONCEPT_CLASS") |> dplyr::select(table) |>  tidyr::unnest(table) |>
    distinct(concept_class_id) |>  pull()

  concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS <- setdiff( concept_class_ids_in_CONCEPT, concept_class_ids_in_CONCEPT_CLASS)

  validation_summary <- dplyr::bind_rows(
    validation_summary,
    tibble::tibble(
      name = "concept_class_ids in CONCEPT in CONCEPT_CLASS",
      items = length(concept_class_ids_in_CONCEPT),
      passes = length(concept_class_ids_in_CONCEPT)-length(concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS),
      fails=    length(concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS),
      nNA = 0,
      error = fails>0,
      warning = FALSE,
      expression = "setdiff( concept_class_ids_in_CONCEPT, concept_class_ids_in_CONCEPT_CLASS)"
    )
  )

  failed_rules_table <- dplyr::bind_rows(
    failed_rules_table,
    tibble::tibble(
      fails = concept_class_ids_in_CONCEPT_notin_CONCEPT_CLASS,
      name = "concept_class_ids in CONCEPT in CONCEPT_CLASS"
    ))



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
