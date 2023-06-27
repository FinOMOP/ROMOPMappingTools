
#' importMappingTables
#'
#' Imports mapping tables from the given input vocabulary information file.
#' - Reads the vocabularies_info.csv.
#' - Throws an error if vocabularies_info.csv has the wrong format.
#' - Reads all the info.csv and usagi.csv files defined in vocabularies_info.csv, unless `ignore` column is TRUE.
#'
#' @param path_to_input_vocabularies_info_file Path to the input vocabulary information file.
#'
#' @return A list with two tibbles, tibble combining all the usagi.csv files and tibble combining all the info.cvs files.
#'
#' @importFrom checkmate assertFileExists
#' @importFrom purrr map
#' @importFrom dplyr select mutate filter starts_with vars
#' @importFrom stringr str_replace
#'
#' @export
importMappingTables <- function(path_to_input_vocabularies_info_file) {

  ###
  ## validate input parameters
  ###
  checkmate::assertFileExists(path_to_input_vocabularies_info_file)

  ###
  ## function
  ###
  mapping_vocabularies_info <- readTable(path_to_input_vocabularies_info_file, table_type = "MappingVocabulariesInfo")

  mapping_vocabularies_info_validation <- validateTable(mapping_vocabularies_info, table_type = "MappingVocabulariesInfo")

  .stopIfValidationErrors(mapping_vocabularies_info_validation, "MappingVocabulariesInfo", path_to_input_vocabularies_info_file)


  # force to absolute paths
  mapping_vocabularies_info <- mapping_vocabularies_info |>
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("path")),
      ~stringr::str_replace(., "\\.", dirname(path_to_input_vocabularies_info_file))
    )


  # read usagi files
  usagi_mapping_tables <- mapping_vocabularies_info |>
    dplyr::select(name = source_vocabulary_id, mapping_type, mapping_version, last_modify_date, path_to_usagi_file) |>
    dplyr::mutate(table = purrr::map(.x = path_to_usagi_file, .f = ~readTable(.x, "UsagiForCCR"))) |>
    dplyr::select(-path_to_usagi_file)


  # read vocabulary info files
  vocabulary_info_mapping_tables <- mapping_vocabularies_info |>
    dplyr::filter(mapping_type == "CCR") |>
    dplyr::select(name = source_vocabulary_id, mapping_version, path_to_vocabulary_info_file) |>
    dplyr::mutate(table = purrr::map(.x = path_to_vocabulary_info_file, .f = ~readTable(.x, "VocabularyInfo"))) |>
    dplyr::select(-path_to_vocabulary_info_file)


  return(
    list(
      usagi_mapping_tables = usagi_mapping_tables,
      vocabulary_info_mapping_tables = vocabulary_info_mapping_tables
    )
  )


}
