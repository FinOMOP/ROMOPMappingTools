
#' @title readOMOPtables
#' @description reads a list of OMOP vocabulary tables
#' @param path_OMOP_vocabulary_folder path to folder with the OMOP vocabulary tables
#' @param table_names list of OMOP vocabulary tables specified in `table_names`, or all tables if NULL
#' @return tibble of OMOP vocabulary tables with columns:
#' `name` = name of omop vocabulary table
#' `table` = omop table
#' @export
#' @importFrom utils data
#' @importFrom stringr str_detect str_remove
#' @importFrom readr read_tsv
#' @importFrom dplyr filter pull
importOMOPtables <- function(path_OMOP_vocabulary_folder, table_names=NULL){

  OMOP_table_names <- c("CONCEPT", "CONCEPT_ANCESTOR", "CONCEPT_CLASS", "CONCEPT_RELATIONSHIP",
                        "CONCEPT_SYNONYM","DOMAIN", "DRUG_STRENGTH", "RELATIONSHIP", "VOCABULARY")

  if(is.null(table_names)){
    table_names <- OMOP_table_names
  }
  ###
  ## validate input parameters
  ###
  path_OMOP_vocabulary_folder |> checkmate::testDirectoryExists()
  table_names |> checkmate::assertSubset(OMOP_table_names)

  # check requested tables exists in folder
  files_in_folder <- list.files(path_OMOP_vocabulary_folder)
  table_names_in_folder  <- files_in_folder[ files_in_folder |> stringr::str_detect("\\.csv$")] |> stringr::str_remove("\\.csv$")

  table_names |> checkmate::assertSubset(table_names_in_folder)


  ###
  ## function
  ###
  omop_vocab_tables <- tibble::tibble(
    name = table_names,
    path_to_table = file.path(path_OMOP_vocabulary_folder, paste0(table_names, ".csv"))
    )

  omop_vocab_tables <- omop_vocab_tables |>
    dplyr::mutate(table = purrr::map2(.x = path_to_table, .y = name, .f = readTable, delim = "\t")) |>
    dplyr::select(-path_to_table)


  return(omop_vocab_tables)

}
