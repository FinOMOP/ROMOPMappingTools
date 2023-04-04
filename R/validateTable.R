
#' @title validateTable
#' @description check that a table has the correct columns format.
#' Vaidation rules are defined in variable `tables_valid_format`.
#' @param table tibble to validate
#' @param table_type name in `tables_valid_format` to read and check the imported table.
#' @return
#' `validation_summary` summary of rules passed;
#' `failed_rules_rows` a tibble with the failed rul name an the corresponding row in `table`.
#' @export
#' @importFrom utils data
#' @importFrom checkmate assertTibble assertCharacter assertChoice
#' @importFrom dplyr filter mutate across if_else row_number
#' @importFrom validate confront summary values
#' @importFrom tibble as_tibble
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer everything
validateTable <- function(table, table_type) {

  utils::data("tables_valid_format", package = "ROMOPMappingTools")

  ###
  ## validate input parameters
  ###
  table |> checkmate::assertTibble()
  table_type |> checkmate::assertCharacter(min.len = 1L, max.len = 1L)
  table_type |> checkmate::assertChoice(tables_valid_format$table_type)


  ###
  ## function
  ###
  table_info <- tables_valid_format |>  dplyr::filter(table_type=={{table_type}})

  # validate
  validations <- validate::confront(table, table_info$validation_rules[[1]])

  # get validations summary
  validation_summary <- validate::summary(validations) |> tibble::as_tibble()
  if("name" %in% names(validation_summary)){
    validation_summary <- validation_summary|>
      dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " "))
  }

  # get failed rules with row numbers
  failed_rules_rows <- validate::values(validations) |> tibble::as_tibble()
  if(ncol(failed_rules_rows)>0){
    failed_rules_rows <- failed_rules_rows |>
      dplyr::mutate_all(~dplyr::if_else(!.x,dplyr::row_number(), as.integer(NA))) |>
      tidyr::pivot_longer(cols = tidyr::everything(), names_to = "name", values_to = "row", values_drop_na = TRUE) |>
      dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " "))
  }else{
    failed_rules_rows <- tibble::tibble(name=as.character(NA), row=as.integer()) |> dplyr::filter(FALSE)
  }

  # output
  return(list(
    validation_summary = validation_summary,
    failed_rules_rows = failed_rules_rows
  ))

}



.stopIfValidationErrors <- function(validation_results, table_type, table_file_path) {

  failed_rules_rows <- validation_results$failed_rules_rows

  # error if format errors
  if(nrow(failed_rules_rows)>0){
    failed_rules  <- failed_rules_rows |>
      dplyr::group_by(name) |>
      dplyr::summarise(rows = paste(row, collapse = ", "))
    stop(
      "Formating errors in ", table_type, " file.\n File", table_file_path, "\n Errors: \n",
      paste0("failed rule: '", failed_rules$name, "' in rows: ", failed_rules$rows, collapse = "\n")
    )
  }


}
