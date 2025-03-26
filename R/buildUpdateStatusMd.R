#' Build Update Status Markdown
#'
#' @description
#' Creates a markdown file containing the update status results.
#' The markdown file includes a table showing update results with:
#' - Context of the update
#' - Type of message (SUCCESS, WARNING, ERROR)
#' - Update step
#' - Detailed message
#'
#' @param updateLogTibble A tibble containing update results with columns:
#'   context, type, step, and message
#' @param pathToUpdateStatusMdFile Path where the markdown file will be saved
#'   (default: tempdir()/VOCABULARIES_LAST_AUTOMATIC_UPDATE_STATUS.md)
#'
#' @return Path to the generated markdown file
#'
#' @importFrom checkmate assertTibble assertNames
#' @importFrom knitr kable
#'
#' @export
buildUpdateStatusMd <- function(
    updateLogTibble, 
    pathToUpdateStatusMdFile = file.path(tempdir(), "VOCABULARIES_LAST_AUTOMATIC_UPDATE_STATUS.md")
    ) {
    # validate parameters
    updateLogTibble |> checkmate::assertTibble()
    updateLogTibble |> names() |> checkmate::assertSetEqual(c("context", "type", "step", "message"))

    # build markdown file
    mdText  <- "# Vocabularies Last Automatic Update Status\n\n"
    mdText <- paste0(mdText, "This is an automatically generated log file by ROMOPMappingTools to detect changes by the github diffs, DO NOT EDIT.\n\n")
    mdText <- paste0(mdText, "ROMOPMappingTools version: ", packageVersion("ROMOPMappingTools"), "\n\n")
    mdText <- paste0(mdText, "### Full log\n\n")
    mdText <- paste0(mdText, updateLogTibble |> knitr::kable()  |> paste0(collapse = "\n"))
    mdText <- paste0(mdText, "\n\n")
    
    # save markdown file
    writeLines(mdText, pathToValidationStatusMdFile)

    return(pathToValidationStatusMdFile)
}
