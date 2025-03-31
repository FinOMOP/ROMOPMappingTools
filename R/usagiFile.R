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
        `ADD_INFO:validationMessages` = readr::col_character(),
        #
        .default = readr::col_character()
    )

    suppressWarnings(
        readr::read_csv(pathToUsagiFile, col_types = cols, na = c(""))
    )
}

#' Write a Usagi mapping file
#'
#' @description
#' Write a Usagi mapping file to disk
#'
#' @details
#' The function writes a Usagi mapping file with the following columns:
#' - sourceCode (character): Source code
#' - sourceName (character): Source name
#' - sourceFrequency (integer): Source frequency
#' - sourceAutoAssignedConceptIds (integer): Source auto-assigned concept IDs
#' - matchScore (double): Match score
#' - mappingStatus (character): Mapping status
#' - equivalence (character): Equivalence
#' - statusSetBy (character): Status set by
#' - statusSetOn (double): Status set on
#' - conceptId (integer): Concept ID
#' - conceptName (character): Concept name
#' - domainId (character): Domain ID
#' - mappingType (character): Mapping type
#' - comment (character): Comment
#' - createdBy (character): Created by
#' - createdOn (double): Created on
#' - assignedReviewer (character): Assigned reviewer
#' - ADD_INFO:sourceConceptId (double): Source concept ID
#' - ADD_INFO:sourceConceptClass (character): Source concept class
#' - ADD_INFO:sourceDomain (character): Source domain
#' - ADD_INFO:sourceValidStartDate (date): Source valid start date
#' - ADD_INFO:sourceValidEndDate (date): Source valid end date
#' - ADD_INFO:sourceParents (character): Source parents
#' - ADD_INFO:sourceParentVocabulary (character): Source parent vocabulary
#' - ADD_INFO:validationMessages (character): Validation messages
#' - ADD_INFO:autoUpdatingInfo (character): Auto updating info
#'
#' @param usagiTibble A tibble containing the Usagi mapping data
#' @param pathToUsagiFile Path where the Usagi mapping file should be written
#'
#' @return None
#'
#' @importFrom readr write_csv
#' @importFrom dplyr select any_of
#'
#' @export
writeUsagiFile <- function(usagiTibble, pathToUsagiFile) {
    firstColNames <- c(
        "sourceCode",
        "sourceName",
        "sourceFrequency",
        "sourceAutoAssignedConceptIds",
        "matchScore",
        "mappingStatus",
        "equivalence",
        "statusSetBy",
        "statusSetOn",
        "conceptId",
        "conceptName",
        "domainId",
        "mappingType",
        "comment",
        "createdBy",
        "createdOn",
        "assignedReviewer",
        "ADD_INFO:sourceConceptId",
        "ADD_INFO:sourceConceptClass",
        "ADD_INFO:sourceDomain",
        "ADD_INFO:sourceValidStartDate",
        "ADD_INFO:sourceValidEndDate",
        "ADD_INFO:sourceParents",
        "ADD_INFO:sourceParentVocabulary"
    )

    lastColNames <- c(
        "ADD_INFO:validationMessages",
        "ADD_INFO:autoUpdatingInfo"
    )

    colNames <- usagiTibble |> names()
    midColNames <- colNames |> setdiff(c(firstColNames, lastColNames))

    # order the columns
    usagiTibble <- usagiTibble |>
        dplyr::select(
            dplyr::any_of(c(firstColNames, midColNames, lastColNames))
        )

    readr::write_csv(usagiTibble, pathToUsagiFile, na = "")
}
