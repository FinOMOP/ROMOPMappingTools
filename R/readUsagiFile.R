#' Read Usagi File
#'
#' Reads a Usagi mapping file with appropriate column type specifications
#'
#' @param pathToUsagiFile Path to the Usagi mapping file to read
#'
#' @return A tibble containing the Usagi mapping data
#'
#' @importFrom readr read_csv cols col_character col_integer col_double col_date
#'
#' @export
readUsagiFile <- function(pathToUsagiFile) {
    cols <- readr::cols(
        sourceCode = readr::col_character(),
        sourceName = readr::col_character(),
        sourceFrequency = readr::col_integer(),
        sourceAutoAssignedConceptIds = readr::col_integer(),
        matchScore = readr::col_double(),
        mappingStatus = readr::col_character(),
        equivalence = readr::col_character(),
        statusSetBy = readr::col_character(),
        statusSetOn = readr::col_double(),
        conceptId = readr::col_integer(),
        conceptName = readr::col_character(),
        domainId = readr::col_character(),
        mappingType = readr::col_character(),
        comment = readr::col_character(),
        createdBy = readr::col_character(),
        createdOn = readr::col_double(),
        assignedReviewer = readr::col_character(),
        #
        `ADD_INFO:sourceConceptId` = readr::col_double(),
        `ADD_INFO:sourceConceptClass` = readr::col_character(),
        `ADD_INFO:sourceDomain` = readr::col_character(),
        `ADD_INFO:sourceValidStartDate` = readr::col_date(),
        `ADD_INFO:sourceValidEndDate` = readr::col_date(),
        `ADD_INFO:sourceParents` = readr::col_character(),
        `ADD_INFO:sourceParentVocabulary` = readr::col_character(),
        `ADD_INFO:validationMessagess` = readr::col_character(),
        #
        .default = readr::col_character()
    )

    suppressWarnings(
        readr::read_csv(pathToUsagiFile, col_types = cols)
    )
} 