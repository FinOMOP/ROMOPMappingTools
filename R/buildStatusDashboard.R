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
    pathToCodeCountsFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    output_file_html = file.path(tempdir(), "MappingStatusDashboard.html")) {
  #
  # Validate parameters
  #

  pathToCodeCountsFolder |> checkmate::assert_directory_exists()
  connectionDetails |> checkmate::assert_class(c("ConnectionDetails"))
  vocabularyDatabaseSchema |> checkmate::assert_character()
  output_file_html |> checkmate::assert_character()


  #
  # Function
  #

  # validate code counts folder
  validationLogTibble <- validateCodeCountsFolder(
    pathToCodeCountsFolder = pathToCodeCountsFolder
  )

  if (validationLogTibble |> dplyr::filter(type == "ERROR") |> nrow() > 0) {
    return(validationLogTibble)
  }

  # calculate mapping status
  mappingStatus <- calculateMappingStatus(
    pathToCodeCountsFolder = pathToCodeCountsFolder,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

  rmarkdown::render(system.file("reports", "mapping_status_dashboard.Rmd", package = "ROMOPMappingTools"),
    params = list(
      mapping_status = mappingStatus
    ),
    output_file = output_file_html
  )

  return(validationLogTibble)
}



.printSumary <- function(table_tables) {
  reactable_table <- table_tables |>
    dplyr::select(-validation_summary, -failed_rules_table) |>
    reactable::reactable(
      defaultPageSize = 25,
      wrap = FALSE,
      resizable = TRUE
    )

  return(reactable_table)
}



.printValidationTables <- function(table_tables) {
  for (name in table_tables$name) {
    row <- table_tables |> dplyr::filter(.data$name == {{ name }})

    cat("### ", name, dplyr::if_else(row$n_failed_rules > 0, "*", ""), "\n")

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
