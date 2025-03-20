#' Read Usagi File
#'
#' Reads a Usagi mapping file with appropriate column type specifications.
#' Warnings about missing columns are suppressed as the ADD_INFO columns are optional.
#'
#' Required columns:
#' - sourceCode (character): Source vocabulary code
#' - sourceName (character): Description of the source code
#' - sourceFrequency (integer): Frequency of occurrence
#' - sourceAutoAssignedConceptIds (integer): Auto-assigned concept IDs
#' - matchScore (double): Score of the mapping match
#' - mappingStatus (character): Status of the mapping (APPROVED, UNCHECKED, etc.)
#' - equivalence (character): Type of equivalence
#' - statusSetBy (character): User who set the status
#' - statusSetOn (double): Timestamp when status was set
#' - conceptId (integer): Target concept ID
#' - conceptName (character): Name of target concept
#' - domainId (character): Domain of target concept
#' - mappingType (character): Type of mapping
#' - comment (character): Additional comments
#' - createdBy (character): User who created the mapping
#' - createdOn (double): Timestamp of creation
#' - assignedReviewer (character): Assigned reviewer
#'
#' Optional ADD_INFO columns:
#' - sourceConceptId (double): Source concept ID
#' - sourceConceptClass (character): Class of source concept
#' - sourceDomain (character): Domain of source concept
#' - sourceValidStartDate (date): Start date of validity
#' - sourceValidEndDate (date): End date of validity
#' - sourceParents (character): Parent concepts
#' - sourceParentVocabulary (character): Vocabulary of parent concepts
#' - validationMessagess (character): Validation messages
#'
#' @param pathToUsagiFile Path to the Usagi mapping file to read
#'
#' @return A tibble containing the Usagi mapping data with appropriate column types
#'
#' @importFrom readr read_csv cols col_character col_integer col_double col_date
#' @importFrom dplyr select filter
#' @importFrom lubridate ymd
#' @importFrom checkmate assertFileExists
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
        readr::read_csv(pathToUsagiFile, col_types = cols, na = c(""))
    )
} 