#' @title readTable
#' @description reads a csv table with the column format defined in `tables_valid_format`.
#' @param path_to_table_file path to csv file to read
#' @param table_type name in `tables_valid_format` to read and check the imported table.
#' @param delim passed to readr::read_delim, delimiter used in the csv table, Default: ','
#' @return `table` read table;
#' @export
#' @importFrom utils data
#' @importFrom checkmate assertFileExists assertCharacter assertChoice
#' @importFrom dplyr filter
#' @importFrom readr read_delim
readTable <- function(path_to_table_file, table_type, delim=",") {

  utils::data("tables_valid_format", package = "ROMOPMappingTools")

  ###
  ## validate input parameters
  ###
  path_to_table_file |> checkmate::assertFileExists()
  table_type |> checkmate::assertCharacter(min.len = 1L, max.len = 1L)
  table_type |> checkmate::assertChoice(tables_valid_format$table_type)


  ###
  ## function
  ###
  table_info <- tables_valid_format |>  dplyr::filter(table_type=={{table_type}})

  # read
  table <- readr::read_delim(
    path_to_table_file,
    col_types = table_info$col_type[[1]],
    delim =  delim,
    na = c("")
  )

  # if it has an ignore column take only selected values
  if("ignore" %in% names(table)){
    table <- table |> dplyr::filter(!ignore) |> dplyr::select(-ignore)
  }

  # output
  return(table)

}
