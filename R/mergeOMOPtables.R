#' @title mergeOMOPtables
#' @description merge the omop vocabulary tables in `path_to_input_omop_vocabulary_folder` and `path_to_input_omop_vocabulary_folder` into a new folder `path_to_output_omop_vocabulary_folder`
#' @param path_to_input_omop_vocabulary_folder path to omop vocabulary tables to merge
#' @param path_to_toappend_omop_vocabulary_folder path to omop vocabulary tables to merge
#' @param path_to_output_omop_vocabulary_folder path to omop vocabulary tables to output
#' @export
#' @importFrom utils data
mergeOMOPtables <- function(path_to_input_omop_vocabulary_folder,
                            path_to_toappend_omop_vocabulary_folder,
                            path_to_output_omop_vocabulary_folder) {

  utils::data("valid_OMOP_tables_v53", package = "ROMOPMappingTools")
  valid_OMOP_tables <- valid_OMOP_tables_v53

  ###
  ## validate input parameters
  ###
  checkmate::assertDirectoryExists(path_to_input_omop_vocabulary_folder)
  checkmate::assertDirectoryExists(path_to_toappend_omop_vocabulary_folder)
  checkmate::assertDirectoryExists(path_to_output_omop_vocabulary_folder)

  ###
  ## function
  ###

  # copy base vocabulary
  for(table_name in valid_OMOP_tables$table_name){
    file.copy(
      file.path(path_to_input_omop_vocabulary_folder, paste0(table_name, ".csv") ),
      file.path(path_to_output_omop_vocabulary_folder, paste0(table_name, ".csv")),
      overwrite = TRUE
    )
  }

  # append new vocabulary
  for(table_name in valid_OMOP_tables$table_name){
    # if omop vocab table exists append it
    if(file.exists(file.path(path_to_toappend_omop_vocabulary_folder, paste0(table_name, ".csv")))){
      file.append(
        file.path(path_to_output_omop_vocabulary_folder, paste0(table_name, ".csv") ),
        file.path(path_to_toappend_omop_vocabulary_folder, paste0(table_name, ".csv") )
      )
    }
  }


}


