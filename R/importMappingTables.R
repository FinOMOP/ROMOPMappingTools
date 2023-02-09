
#' importMappingTables
#'
#' @param path_to_input_vocabularies_info_file
#'
#' @return
#' @export
#'
#' @examples
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
  mapping_vocabularies_info <- mapping_vocabularies_info |> dplyr::mutate(dplyr::across(
    dplyr::starts_with("path"),
    ~stringr::str_replace(., "\\.", dirname(path_to_input_vocabularies_info_file))
  ))


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
