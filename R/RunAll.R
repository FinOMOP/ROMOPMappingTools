#' Run All Validation and Upload Steps
#'
#' @description
#' Runs the complete workflow of validating vocabulary files and uploading them to CDM tables.
#' It performs the following steps:
#' 1. Validate the vocabulary folder
#' 2. If sourceToConceptMapTable is not NULL, create the SourceToConceptMap table
#' 3. Upload the vocabulary.csv and Usagi files to the SourceToConceptMap table
#' 4. Move the SourceToConceptMap table to the CDM table
#' 5. Validate the CDM tables with DQD
#'
#' @param pathToVocabularyFolder Path to folder containing vocabulary files
#' @param connectionDetails DatabaseConnector connection details object
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables
#' @param validationResultsFolder Folder where validation results will be saved
#' @param sourceToConceptMapTable Optional name of source to concept map table
#'
#' @importFrom checkmate assertDirectory assertClass assertString
#' @importFrom DatabaseConnector connect
#' @importFrom dplyr filter
#'
#' @export
runAll <- function(
    pathToVocabularyFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    validationResultsFolder,
    sourceToConceptMapTable = NULL) {
    # validate parameters
    pathToVocabularyFolder |> checkmate::assertDirectory()
    connectionDetails |> checkmate::assertClass("ConnectionDetails")
    vocabularyDatabaseSchema |> checkmate::assertString()
    validationResultsFolder |> checkmate::assertDirectory()
    sourceToConceptMapTable |> checkmate::assertString(null.ok = TRUE)

    # create a connection to the database
    connection <- DatabaseConnector::connect(connectionDetails)

    # validate the vocabulary folder
    message("Validating the vocabulary folder")
    validationLogTibble <- validateVocabularyFolder(
        pathToVocabularyFolder = pathToVocabularyFolder,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        validationResultsFolder = validationResultsFolder
    )

    # if there are errors at this point stop
    if (validationLogTibble |> dplyr::filter(type != "SUCCESS") |> nrow() > 0) {
        message("Errors found in the vocabulary folder")
        return(validationLogTibble)
    }

    # if sourceToConceptMapTable is not NULL, create the SourceToConceptMap table
    if (is.null(sourceToConceptMapTable)) {
        message("Creating the SourceToConceptMap table")
        sourceToConceptMapTable <- "source_to_concept_map_extended"
        createSourceToConceptMapExtended(connection, vocabularyDatabaseSchema, sourceToConceptMapTable)
    }

    # upload the vocabulary.csv and Usagi files to the SourceToConceptMap table
    message("Uploading the vocabulary.csv and Usagi files to the SourceToConceptMap table")
    errorMessage <- ""
    tryCatch(
        {
            vocabularyFolderToSTCMVocabularyConcepClassTables(
                pathToVocabularyFolder = pathToVocabularyFolder,
                connection = connection,
                vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                sourceToConceptMapTable = sourceToConceptMapTable
            )
        },
        error = function(e) {
            errorMessage <<- e$message
        }
    )

    if (errorMessage != "") {
        validationLogTibble <- dplyr::bind_rows(validationLogTibble, dplyr::tibble(
            context = "vocabulary.csv",
            type = "ERROR",
            step = "uploading the vocabulary.csv and Usagi files to the SourceToConceptMap table",
            message = errorMessage
        ))
        return(validationLogTibble)
    }

    # STCM to CDM table
    message("Uploading the SourceToConceptMap table to the CDM table")
    errorMessage <- ""
    tryCatch(
        {
            STCMToCDMTables(
                connection = connection,
                vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                sourceToConceptMapTable = sourceToConceptMapTable
            )
        },
        error = function(e) {
            errorMessage <<- e$message
        }
    )

    if (errorMessage != "") {
        validationLogTibble <- dplyr::bind_rows(validationLogTibble, dplyr::tibble(
            context = "STCMToCDMTables",
            type = "ERROR",
            step = "moving the SourceToConceptMap table to the CDM table",
            message = errorMessage
        ))
        return(validationLogTibble)
    }

    # create the ancestor tables
    message("Creating the ancestor tables")
    errorMessage <- ""
    tryCatch(
        {
            # get all the vocabulary ids used as ancestors
            vocabularyIds <- c()
            vocabulariesTibble <- readr::read_csv(pathToVocabularyFolder |> file.path("vocabularies.csv"), show_col_types = FALSE)
            for (i in 1:nrow(vocabulariesTibble)) {
                vocabularyIds <- c(vocabularyIds, vocabulariesTibble$source_vocabulary_id[i])
                pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
                usagiTibble <- readUsagiFile(pathToUsagiFile)
                if ("ADD_INFO:sourceParentVocabulary" %in% names(usagiTibble)) {
                    vocabularyId <- usagiTibble |>
                        dplyr::distinct(`ADD_INFO:sourceParentVocabulary`) |>
                        dplyr::pull(`ADD_INFO:sourceParentVocabulary`) |>
                        stringr::str_split("\\|") |>
                        unlist()
                    vocabularyIds <- c(vocabularyIds, vocabularyId)
                }
            }
            vocabularyList <- vocabularyIds |>
                unique() |>
                na.omit() |>
                setdiff("")

            conceptRelationshipToAncestorTables(
                connection = connection,
                vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                vocabularyList = vocabularyList
            )
        },
        error = function(e) {
            errorMessage <<- e$message
        }
    )

    if (errorMessage != "") {
        validationLogTibble <- dplyr::bind_rows(validationLogTibble, dplyr::tibble(
            context = "conceptRelationshipToAncestorTables",
            type = "ERROR",
            step = "creating the ancestor tables",
            message = errorMessage
        ))
    }

    # close the connection
    DatabaseConnector::disconnect(connection)

    # validation with DQD
    message("Validating the CDM tables with DQD")
    validationLogTibbleTmp <- validateCDMtablesWithDQD(
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        validationResultsFolder = validationResultsFolder
    )

    # join and save the validation log tibble
    validationLogTibble <- dplyr::bind_rows(validationLogTibble, validationLogTibbleTmp) |>
        dplyr::select(context, type, step, message)
    validationLogTibble |> readr::write_csv(file.path(validationResultsFolder, "validationLogTibble.csv"), na = "")

    return(validationLogTibble)
}
