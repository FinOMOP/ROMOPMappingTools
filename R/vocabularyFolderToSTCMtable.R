
vocabularyFolderToSTCMtable <- function(pathToVocabularyFolder, connection, vocabularyDatabaseSchema, sourceToConceptMapTable, skipValidation = TRUE) {

    if (!skipValidation) {
        validationsLogTibble <- validateVocabularyFolder(pathToVocabularyFolder, connection, vocabularyDatabaseSchema, pathToValidatedUsagiFolder)
        if (nrow(validationsLogTibble |> dplyr::filter(type != "SUCCESS")) > 0) {
            stop("Validation failed, use validateVocabularyFolder to see the errors")
        }
    }

    for (i in 1:nrow(vocabulariesTibble)) {
        message(paste0("Appending Usagi file ", vocabulariesTibble$path_to_usagi_file[i]))
        vocabularyId <- vocabulariesTibble$source_vocabulary_id[i]
        pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabulariesTibble$path_to_usagi_file[i])
        appendUsagiFileToSTCMtable(vocabularyId, pathToUsagiFile, connection, vocabularyDatabaseSchema, sourceToConceptMapTable, skipValidation = TRUE)
    }

}
