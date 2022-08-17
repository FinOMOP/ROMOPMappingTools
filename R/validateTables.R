

validateTables <- function(
    tibble_with_tables,
    table_type = NULL
){


  ###
  ## validate input parameters
  ###
  tibble_with_tables |> checkmate::assertTibble()
  checkmate::assertSubset(c("name", "table"), tibble_with_tables |> names())

  ###
  ## function
  ###
  if(is.null(table_type)){
    table_type <- tibble_with_tables$name
  }

  tibble_with_tables <-  tibble_with_tables |>
    dplyr::mutate(tmp = purrr::map2(.x=table, .y={{table_type}}, .f=validateTable)) |>
    dplyr:: mutate(validation_summary = purrr::map(tmp, ~{.x$validation_summary}))|>
    dplyr:: mutate(failed_rules_rows = purrr::map(tmp, ~{.x$failed_rules_rows})) |>
    dplyr::select(-tmp) |>
    # failed rows to failed table
    dplyr::mutate(failed_rules_table = purrr::map2(.x=table, .y=failed_rules_rows, .f=~{
      .x |> dplyr::mutate(row = row_number()) |> dplyr::right_join(.y, by="row")
    })) |>
    dplyr::select(-failed_rules_rows)|>
    # add n_failed_rules
    dplyr::mutate(n_failed_rules = purrr::map_int(failed_rules_table, nrow))


  return(tibble_with_tables)

}
