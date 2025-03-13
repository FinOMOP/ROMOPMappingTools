


#' Build Status Dashboard
#'
#' This function generates a status dashboard for mapping status.
#'
#' @param mapping_status Mapping status information.
#' @param output_file_html The path to the output HTML file.
#'
#' @return The path to the output HTML file.
#'
#' @importFrom dplyr select mutate filter if_else
#' @importFrom rmarkdown render
#' @importFrom reactable reactable
#' @importFrom shiny div
#'
#' @export
buildStatusDashboard <- function(
    mapping_status  = NULL,
    output_file_html = file.path(tempdir(), "MappingStatusDashboard.html")) {

  

  ##
  # Function
  ##
  rmarkdown::render(system.file("reports", "mapping_status_dashboard.Rmd", package = "ROMOPMappingTools"),
                    params = list(
                      mapping_status = mapping_status
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
