

checkUsagiFile <- function(
    pathToUsagiFile,
    pathToOMOPVocabularyDuckDBfile,
    pathToEditedUsagiFile = pathToUsagiFile
) {

    checkmate::assertFileExists(pathToUsagiFile)
    checkmate::assertFileExists(pathToOMOPVocabularyDuckDBfile)




}
