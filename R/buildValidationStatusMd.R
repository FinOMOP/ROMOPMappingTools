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
#' @param pathToValidationStatusMdFile Path where the markdown file will be saved
#'   (default: tempdir()/VOCABULARIES_VALIDATION_STATUS.md)
#' @param pathToVocabularyFolder Optional path to the vocabulary folder. If provided,
#'   a mapping summary section will be included showing the count of distinct
#'   sourceCodes per mappingStatus. If NULL, the mapping summary section is omitted.
#'
#' @return Path to the generated markdown file
#'
#' @importFrom checkmate assertTibble assertNames
#' @importFrom knitr kable
#'
#' @export
buildValidationStatusMd <- function(
    validationLogTibble, 
    pathToValidationStatusMdFile = file.path(tempdir(), "VOCABULARIES_VALIDATION_STATUS.md"),
    pathToVocabularyFolder = NULL
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
    
    # Add Mapping summary section if pathToVocabularyFolder is provided
    if (!is.null(pathToVocabularyFolder)) {
       pathToVocabularyFolder |> checkmate::assertDirectoryExists()
        pathToVocabularyInfoFile <- file.path(pathToVocabularyFolder, "vocabularies.csv")
        if (file.exists(pathToVocabularyInfoFile)) {
            vocabulariesTibble <- readr::read_csv(pathToVocabularyInfoFile, show_col_types = FALSE) |>
                dplyr::filter(ignore == FALSE)
            
            # Read all Usagi files and count distinct sourceCodes per mappingStatus
            mappingSummaryList <- list()
            for (i in 1:nrow(vocabulariesTibble)) {
                pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
                if (file.exists(pathToUsagiFile)) {
                    usagiTibble <- readr::read_csv(pathToUsagiFile, show_col_types = FALSE)
                if ("ADD_INFO:ignoreReason" %in% names(usagiTibble)) {
                    usagiTibble <- usagiTibble |>
                        dplyr::mutate(
                            mappingStatus = dplyr::if_else(!is.na(`ADD_INFO:ignoreReason`) & `ADD_INFO:ignoreReason`, "IGNORE", mappingStatus)
                        )
                }
                    mappingSummary <- usagiTibble |>
                        dplyr::count(mappingStatus, name = "n_sourceCodes") |>
                        dplyr::mutate(vocabulary = vocabulariesTibble$source_vocabulary_id[i])
                    mappingSummaryList[[i]] <- mappingSummary
                }
            }
            
            if (length(mappingSummaryList) > 0) {
                mappingSummaryTibble <- dplyr::bind_rows(mappingSummaryList) |>
                    dplyr::select(vocabulary, mappingStatus, n_sourceCodes)
              
            mappingSummaryTibble <- mappingSummaryTibble |>
                tidyr::pivot_wider(
                    names_from = mappingStatus,
                    values_from = n_sourceCodes,
                    values_fill = 0
                )
                
                mdText <- paste0(mdText, "### Mapping summary\n\n")
                mdText <- paste0(mdText, mappingSummaryTibble |> knitr::kable() |> paste0(collapse = "\n"))
                mdText <- paste0(mdText, "\n\n")
            }
        }
    }
    
    mdText <- paste0(mdText, "### Validation Summary\n\n")
    mdText <- paste0(mdText, validationLogTibbleSummary |> knitr::kable()  |> paste0(collapse = "\n"))
    mdText <- paste0(mdText, "\n\n")
    mdText <- paste0(mdText, "### Full log\n\n")
    mdText <- paste0(mdText, validationLogTibble |> knitr::kable()  |> paste0(collapse = "\n"))
    mdText <- paste0(mdText, "\n\n")
    
    
    # save markdown file
    writeLines(mdText, pathToValidationStatusMdFile)

    return(pathToValidationStatusMdFile)
}
