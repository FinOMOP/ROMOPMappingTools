validateVocabularyFolder <- function(pathToVocabularyFolder, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFolder) {
    #
    # Parameter validation
    #
    checkmate::assertClass(connection, "DBIConnection")
    checkmate::assertString(vocabularyDatabaseSchema)

    pathToVocabularyFolder |> checkmate::assertDirectoryExists()
    pathToVocabularyInfoFile <- pathToVocabularyFolder |> file.path("vocabularies.csv")
    pathToVocabularyInfoFile |> checkmate::assertFileExists()

    vocabulariesTibble <- readr::read_csv(pathToVocabularyInfoFile, show_col_types = FALSE)

    vocabulariesTibble |>
        names() |>
        checkmate::assertSubset(c("source_vocabulary_id", "source_concept_id_offset", "path_to_usagi_file", "path_to_news_file", "ignore"))

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
    validationsLogTibble <- tibble::tibble()
    for (i in 1:nrow(vocabulariesTibble)) {
        message(paste0("Validating Usagi file ", vocabulariesTibble$path_to_usagi_file[i]))
        
        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        pathToValidatedUsagiFile <- file.path(pathToValidatedUsagiFolder, vocabulariesTibble$path_to_usagi_file[i])
        dir.create(dirname(pathToValidatedUsagiFile), showWarnings = FALSE, recursive = TRUE)
       
        validationLogTibble <- validateUsagiFile(
            pathToUsagiFile = pathToUsagiFile,
            connection = connection,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            pathToValidatedUsagiFile = pathToValidatedUsagiFile
        )

        validationLogTibble  <- validationLogTibble |> 
        dplyr::mutate(vocabulary_id = vocabulariesTibble$source_vocabulary_id[i]) |> 
        dplyr::select(vocabulary_id, dplyr::everything())

        validationsLogTibble <- validationsLogTibble |> dplyr::bind_rows(validationLogTibble)
    }

    return(validationsLogTibble)

}