#' Create a Temporary Copy of the OMOP Vocabulary DuckDB
#'
#' This function creates a temporary copy of the OMOP vocabulary DuckDB file from the package's test data.
#'
#' @return A character string containing the path to the temporary copy of the DuckDB file
#' @export
#'
helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB <- function() {
    pathToOMOPVocabularyDuckDBfile <- system.file("testdata/OMOPVocabulary/OMOPVocabulary.duckdb", package = "ROMOPMappingTools")
    pathToOMOPVocabularyDuckDBfileCopy <- tempfile(fileext = ".duckdb")
    file.copy(pathToOMOPVocabularyDuckDBfile, pathToOMOPVocabularyDuckDBfileCopy)
    return(pathToOMOPVocabularyDuckDBfileCopy)
}
