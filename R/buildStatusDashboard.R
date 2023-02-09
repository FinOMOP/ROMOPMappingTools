

#' buildStatusDashboard
#'
#' @param usagi_mapping_tables
#' @param vocabulary_info_mapping_tables
#' @param omop_tables
#' @param databases_code_counts_tables
#' @param mapping_status
#' @param output_file_html
#'
#' @return
#' @export
#'
#' @examples
buildStatusDashboard <- function(
    usagi_mapping_tables = NULL,
    vocabulary_info_mapping_tables = NULL,
    omop_tables  = NULL,
    databases_code_counts_tables = NULL,
    mapping_status  = NULL,
    output_file_html = file.path(tempdir(), "MappingStatusDashboard.html")) {



  ##
  # validate inputs
  ##
  col_names <- c("name", "table", "validation_summary", "failed_rules_table", "n_failed_rules")


  if(!is.null(usagi_mapping_tables)){
    usagi_mapping_tables |> checkmate::assertTibble()
    usagi_mapping_tables |> names() |> checkmate::assertSubset(c(col_names, "mapping_type", "mapping_version", "last_modify_date"))

    usagi_mapping_tables <- usagi_mapping_tables |> dplyr::select(-table)
  }

  if(!is.null(vocabulary_info_mapping_tables)){
    vocabulary_info_mapping_tables |> checkmate::assertTibble()
    vocabulary_info_mapping_tables |> names() |> checkmate::assertSubset(c(col_names, "mapping_version"))

    vocabulary_info_mapping_tables <- vocabulary_info_mapping_tables |> dplyr::select(-table)
  }

  if(!is.null(omop_tables)){
    omop_tables |> checkmate::assertTibble()
    omop_tables |> names() |> checkmate::assertSubset(col_names)

    omop_tables <- omop_tables |> dplyr::select(-table)
  }

  if(!is.null(databases_code_counts_tables)){
    databases_code_counts_tables |> checkmate::assertTibble()
    databases_code_counts_tables |> names() |> checkmate::assertSubset(col_names)

    databases_code_counts_tables <- databases_code_counts_tables |> dplyr::select(-table)
  }



  ##
  # Function
  ##
  rmarkdown::render(system.file("reports", "mapping_status_dashboard.Rmd", package = "ROMOPMappingTools"),
                    params = list(
                      usagi_mapping_tables = usagi_mapping_tables,
                      vocabulary_info_mapping_tables = vocabulary_info_mapping_tables,
                      omop_tables  = omop_tables,
                      databases_code_counts_tables = databases_code_counts_tables,
                      mapping_status  = mapping_status
                    ),
                    output_file = output_file_html)

  return(output_file_html)
}



.printSumary <- function(table_tables) {

  reactable_table <- table_tables |>
    dplyr::select(- validation_summary, -failed_rules_table) |>
    reactable::reactable(
      defaultPageSize=25,
      wrap = FALSE,
      resizable = TRUE)

  return(reactable_table)

}



.printValidationTables <- function(table_tables) {

  for(name in table_tables$name){
      row <- table_tables |>  dplyr::filter(.data$name == {{name}})

      cat("### ", name, dplyr::if_else(row$n_failed_rules>0, "*", ""), "\n")

      cat("#### Validation Summary \n")

      row$validation_summary[[1]] |>
        reactable::reactable(
          wrap = FALSE,
          resizable = TRUE
        ) |>
        shiny::div() |>
        print()

      cat("#### Failed Rules \n")

      row$failed_rules_table[[1]] |>
        reactable::reactable(
          wrap = FALSE,
          resizable = TRUE,
          filterable = TRUE,
          defaultPageSize = 20
        ) |>
        shiny::div() |>
        print()

    }

}




















