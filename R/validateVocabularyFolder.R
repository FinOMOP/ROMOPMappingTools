#' Validate Vocabulary Folder
#'
#' Validates all Usagi files in a vocabulary folder against a set of rules.
#' The folder must contain a vocabularies.csv file that describes the vocabularies
#' and their corresponding Usagi files.
#'
#' @param pathToVocabularyFolder Path to folder containing vocabulary files
#' @param connection A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @param validationResultsFolder Folder where validation results will be saved
#'
#' @return A tibble containing validation results for all files
#'
#' @importFrom checkmate assertDirectory assertFileExists
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows mutate select everything
#'
#' @export
validateVocabularyFolder <- function(pathToVocabularyFolder, connection, vocabularyDatabaseSchema, validationResultsFolder) {
    #
    # Parameter validation
    #
    checkmate::assertClass(connection, "DBIConnection")
    checkmate::assertString(vocabularyDatabaseSchema)

    pathToVocabularyFolder |> checkmate::assertDirectoryExists()
    pathToVocabularyInfoFile <- pathToVocabularyFolder |> file.path("vocabularies.csv")
    pathToVocabularyInfoFile |> checkmate::assertFileExists()

    vocabulariesTibble <- readr::read_csv(pathToVocabularyInfoFile, show_col_types = FALSE)

    # Add tmpvalidationMessages message column
    vocabulariesTibble <- vocabulariesTibble |>
        dplyr::mutate(tmpvalidationMessages = "")

    #
    # Checks
    #
    validationLogTibble <- LogTibble$new()

    # check the vocabularies.csv file has the correct columns
    missingColumns <- c("source_vocabulary_id", "source_vocabulary_name", "source_concept_id_offset", "path_to_usagi_file", "path_to_news_file", "ignore") |>
        setdiff(names(vocabulariesTibble))
    if (length(missingColumns) > 0) {
        validationLogTibble$ERROR("Missing columns", paste0("Missing columns: ", paste(missingColumns, collapse = ", ")))
        return(validationLogTibble$logTibble)
    }

    # check the vocabularies.csv file has correct values
    validationRules <- validate::validator(
        source_vocabulary_id.is.not.empty = !is.na(source_vocabulary_id) & source_vocabulary_id != "",
        source_vocabulary_id.is.less.than.20.characters = nchar(source_vocabulary_id) < 20,
        source_vocabulary_name.is.not.empty = !is.na(source_vocabulary_name) & source_vocabulary_name != "",
        source_vocabulary_name.is.less.than.255.characters = nchar(source_vocabulary_name) < 255,
        source_concept_id_offset.is.a.number.over.2.billion = source_concept_id_offset > 2000000000,
        source_concept_id_offset.is.unique = is_unique(source_concept_id_offset)
    )
    validations <- validate::confront(vocabulariesTibble, validationRules)
    result <- .applyValidationRules(fileTibble = vocabulariesTibble, validations, validationLogTibble)
    vocabulariesTibble <- result$fileTibble
    validationLogTibble <- result$validationLogTibble

    if (validationLogTibble$logTibble |> dplyr::filter(type != "SUCCESS") |> nrow() > 0) {
        vocabulariesTibble |>
            readr::write_csv(file.path(validationResultsFolder, "vocabularies.csv"), na = "")
        return(validationLogTibble$logTibble)
    }

    vocabulariesTibble <- vocabulariesTibble |>
        dplyr::filter(ignore == FALSE)

    vocabulariesTibble |> checkmate::assertDataFrame(min.rows = 1)

    # check all the usagi files exist
    for (i in 1:nrow(vocabulariesTibble)) {
        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        pathToUsagiFile |> checkmate::assertFileExists()
    }

    # check all the news files exist
    for (i in 1:nrow(vocabulariesTibble)) {
        pathToNewsFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_news_file[i])
        pathToNewsFile |> checkmate::assertFileExists()
    }

    # check all the news files have a valid version tag
    versionColumn <- character(nrow(vocabulariesTibble))
    for (i in 1:nrow(vocabulariesTibble)) {
        pathToNewsFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_news_file[i])
        newsFileContent <- readLines(pathToNewsFile, n = 1)
        versionTag <- newsFileContent |> stringr::str_extract("v[0-9]+\\.[0-9]+\\.[0-9]+")
        versionTag |> checkmate::assertCharacter(pattern = "^v[0-9]+\\.[0-9]+\\.[0-9]+")
        versionColumn[i] <- versionTag
    }
    vocabulariesTibble$version_from_news_file <- versionColumn

    #
    # Function
    #

    # Validate each Usagi file
    validationsLogTibble <- validationLogTibble$logTibble |>
        dplyr::mutate(context = "vocabulary.csv")
    for (i in 1:nrow(vocabulariesTibble)) {
        message(paste0("Validating Usagi file ", vocabulariesTibble$path_to_usagi_file[i]))

        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        pathToValidatedUsagiFile <- file.path(validationResultsFolder, vocabulariesTibble$path_to_usagi_file[i])
        sourceConceptIdOffset <- vocabulariesTibble$source_concept_id_offset[i]
        dir.create(dirname(pathToValidatedUsagiFile), showWarnings = FALSE, recursive = TRUE)

        validationLogTibble <- validateUsagiFile(
            pathToUsagiFile = pathToUsagiFile,
            connection = connection,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            pathToValidatedUsagiFile = pathToValidatedUsagiFile,
            sourceConceptIdOffset = sourceConceptIdOffset
        )

        validationLogTibble <- validationLogTibble |>
            dplyr::mutate(context = vocabulariesTibble$source_vocabulary_id[i]) |>
            dplyr::select(context, dplyr::everything())

        validationsLogTibble <- validationsLogTibble |> dplyr::bind_rows(validationLogTibble)
    }

    return(validationsLogTibble)
}
