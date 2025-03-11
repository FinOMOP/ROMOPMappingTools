#' Convert CONCEPT_RELATIONSHIP to CONCEPT_ANCESTOR
#'
#' @description
#' Creates and populates the CONCEPT_ANCESTOR table
#' from CONCEPT_RELATIONSHIP table data.
#'
#' @param connection           A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema containing the vocabulary and STCM tables
#' @param vocabularyList       Vector of vocabulary_ids to include (default: c("ICD10"))
#'
#' @return
#' Invisible TRUE if successful
#'
#' @importFrom checkmate assertClass assertString assertCharacter
#' @importFrom SqlRender readSql render translate
#' @importFrom DatabaseConnector executeSql
#'
#' @export
conceptRelationshipToAncestorTables <- function(
    connection,
    vocabularyDatabaseSchema,
    vocabularyList) {
  # Input validation
  connection |> checkmate::assertClass("DBIConnection")
  vocabularyDatabaseSchema |> checkmate::assertString()
  vocabularyList |> checkmate::assertCharacter()

  vocabulariesInTheDatabase <- DatabaseConnector::dbGetQuery(
    connection,
    "SELECT vocabulary_id FROM vocabulary"
  ) |> dplyr::pull(vocabulary_id)

  vocabularyList |> checkmate::assertSubset(vocabulariesInTheDatabase)
  
  # Read and render SQL
  # Get SQL script path
  sqlPath <- system.file(
    "sql", "sql_server", 
    "CONCEPT_RELATIONSHIPToANCESTOR.sql", 
    package = "ROMOPMappingTools"
  )
  
  # Read and render SQL
  sql <- SqlRender::readSql(sqlPath)
  sql <- SqlRender::render(sql,
                          vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                          vocabularyList = paste(paste0("'", vocabularyList, "'"), collapse = ","))
  
  # Translate to target dialect
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

  # Execute SQL
  DatabaseConnector::executeSql(connection, sql)

  return(invisible(TRUE))
}
