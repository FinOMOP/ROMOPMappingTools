
createATemporaryCopyOfTheOMOPVocabularyDuckDB <- function() {
    pathToOMOPVocabularyDuckDBfile <- system.file("testdata/OMOPVocabularyICD10only/OMOPVocabularyICD10only.duckdb", package = "ROMOPMappingTools")
    pathToOMOPVocabularyDuckDBfileCopy <- tempfile(fileext = ".duckdb")
    file.copy(pathToOMOPVocabularyDuckDBfile, pathToOMOPVocabularyDuckDBfileCopy)
    return(pathToOMOPVocabularyDuckDBfileCopy)
}
