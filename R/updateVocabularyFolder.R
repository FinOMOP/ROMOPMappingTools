#' Update Vocabulary Folder
#'
#' Updates all Usagi files in a vocabulary folder with the latest concept information.
#' The folder must contain a vocabularies.csv file that describes the vocabularies
#' and their corresponding Usagi files.
#'
#' @param pathToVocabularyFolder Path to folder containing vocabulary files
#' @param connection A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @param updateResultsFolder Folder where updated files will be saved
#' @param skipValidation Whether to skip validation checks if TRUE. Default is TRUE
#'
#' @return A tibble containing update results for all files
#'
#' @importFrom checkmate assertDirectory assertFileExists
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows mutate select everything
#'
#' @export
updateVocabularyFolder <- function(
    pathToVocabularyFolder,
    connection,
    vocabularyDatabaseSchema,
    updateResultsFolder,
    skipValidation = TRUE
) {
    #
    # Parameter validation
    #
    checkmate::assertClass(connection, "DBIConnection")
    checkmate::assertString(vocabularyDatabaseSchema)

    pathToVocabularyFolder |> checkmate::assertDirectoryExists()
    pathToVocabularyInfoFile <- pathToVocabularyFolder |> file.path("vocabularies.csv")
    pathToVocabularyInfoFile |> checkmate::assertFileExists()

    if (skipValidation == FALSE) {
        validationLogR6 <- validateVocabularyFolder(
            pathToVocabularyFolder = pathToVocabularyFolder,
            connection = connection,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            validationResultsFolder = tempdir()
        )

        if (validationLogR6 |> dplyr::filter(type != "SUCCESS") |> nrow() > 0) {
            stop("The vocabulary folder has the following errors: ", validationLogR6 |> dplyr::filter(type != "SUCCESS") |> dplyr::pull(message) |> paste(collapse = "\n"))
        }
    }

    #
    # Function
    #
    vocabulariesTibble <- readr::read_csv(pathToVocabularyInfoFile, show_col_types = FALSE)

    # update each Usagi file
    updateLogTibble <- tibble::tibble()
    for (i in 1:nrow(vocabulariesTibble)) {
        message(paste0("Updating Usagi file ", vocabulariesTibble$path_to_usagi_file[i]))

        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        pathToUpdatedUsagiFile <- file.path(updateResultsFolder, vocabulariesTibble$path_to_usagi_file[i])
        sourceConceptIdOffset <- vocabulariesTibble$source_concept_id_offset[i]
        dir.create(dirname(pathToUpdatedUsagiFile), showWarnings = FALSE, recursive = TRUE)

        oneFileUpdateLogTibble <- updateUsagiFile(
            pathToUsagiFile = pathToUsagiFile,
            connection = connection,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            pathToUpdatedUsagiFile = pathToUpdatedUsagiFile,
            sourceConceptIdOffset = sourceConceptIdOffset
        )
        oneFileUpdateLogTibble <- oneFileUpdateLogTibble |>
            dplyr::mutate(context = vocabulariesTibble$source_vocabulary_id[i]) |>
            dplyr::select(context, dplyr::everything())

        updateLogTibble <- updateLogTibble |> dplyr::bind_rows(oneFileUpdateLogTibble)
    }

    return(updateLogTibble  |> dplyr::select(context, type, step, message))
}
