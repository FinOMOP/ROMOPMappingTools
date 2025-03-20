#' Build Validation Status Markdown
#'
#' @description
#' Creates a markdown file containing the validation status results.
#' The markdown file includes a table showing validation results with:
#' - Context of the validation
#' - Type of message (SUCCESS, WARNING, ERROR)
#' - Validation step
#' - Detailed message
#'
#' @param validationLogTibble A tibble containing validation results with columns:
#'   context, type, step, and message
#' @param pathToOutputFile Path where the markdown file will be saved
#'   (default: tempdir()/VOCABULARIES_VALIDATION_STATUS.md)
#'
#' @return Path to the generated markdown file
#'
#' @importFrom checkmate assertTibble assertNames
#' @importFrom knitr kable
#'
#' @export
buildValidationStatusMd <- function(
    validationLogTibble, 
    pathToValidationStatusMdFile = file.path(tempdir(), "VOCABULARIES_VALIDATION_STATUS.md")
    ) {
    # validate parameters
    validationLogTibble |> checkmate::assertTibble()
    validationLogTibble |> names() |> checkmate::assertSetEqual(c("context", "type", "step", "message"))

    contextOrder  <- validationLogTibble |> dplyr::distinct(context)

    # summarize the validation log tibble
    validationLogTibbleSummary <- validationLogTibble |>
    dplyr::group_by(context) |>
    dplyr::summarise(
        SUCCESS = sum(type == "SUCCESS"),
        WARNING = sum(type == "WARNING"), 
        ERROR = sum(type == "ERROR")
    ) 

    validationLogTibbleSummary <- contextOrder |>
    dplyr::left_join(validationLogTibbleSummary, by = "context")

    # build markdown file
    mdText  <- "# Vocabularies Validation Status\n\n"
    mdText <- paste0(mdText, "This is an automatically generated log file by ROMOPMappingTools to detect changes by the github diffs, DO NOT EDIT.\n\n")
    mdText <- paste0(mdText, "ROMOPMappingTools version: ", packageVersion("ROMOPMappingTools"), "\n\n")
    mdText <- paste0(mdText, "### Summary\n\n")
    mdText <- paste0(mdText, validationLogTibbleSummary |> knitr::kable()  |> paste0(collapse = "\n"))
    mdText <- paste0(mdText, "\n\n")
    mdText <- paste0(mdText, "### Full log\n\n")
    mdText <- paste0(mdText, validationLogTibble |> knitr::kable()  |> paste0(collapse = "\n"))
    mdText <- paste0(mdText, "\n\n")
    
    
    # save markdown file
    writeLines(mdText, pathToValidationStatusMdFile)

    return(pathToValidationStatusMdFile)
}
