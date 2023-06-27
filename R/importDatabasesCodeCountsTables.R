


#' Import Databases Code Counts Tables
#'
#' This function imports individu databases code counts tables.
#'
#' @param path_to_database_code_counts_file The path to the file containing databases code counts.
#' @param calculate_all_databases Boolean indicating whether to calculate counts for all databases.
#'
#' @return The imported databases code counts tables.
#'
#' @importFrom checkmate assertFileExists
#' @importFrom dplyr mutate select
#' @importFrom purrr map
#'
#' @export
importDatabasesCodeCountsTables <- function(path_to_database_code_counts_file, calculate_all_databases=TRUE) {

  ###
  ## validate input parameters
  ###
  path_to_database_code_counts_file |> checkmate::assertFileExists()

  databases_code_counts <- readTable(path_to_database_code_counts_file, table_type = "DatabasesCodeCounts")
  databases_code_counts_validation <- validateTable(databases_code_counts, table_type = "DatabasesCodeCounts")

  # error if format errors
  .stopIfValidationErrors(databases_code_counts_validation, table_type = "DatabasesCodeCounts", table_file_path = path_to_database_code_counts_file)

  # force to absolute paths
  databases_code_counts <- databases_code_counts |> dplyr::mutate(dplyr::across(
    dplyr::starts_with("path"),
    ~stringr::str_replace(., "\\.", dirname(path_to_database_code_counts_file))
  ))

  # validate files exits
  databases_code_counts$path_to_code_counts_file |> checkmate::assertFileExists()


  ###
  ## function
  ###

  # read code_counts files
  databases_code_counts <- databases_code_counts |>
    dplyr::select(name = database_name, path_to_code_counts_file) |>
    dplyr::mutate(table = purrr::map(.x = path_to_code_counts_file , .f = ~readTable(.x, "CodeCounts"))) |>
    dplyr::select(-path_to_code_counts_file )


  return(databases_code_counts)

}

