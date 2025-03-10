#' Convert Source to Concept Map to CDM Tables
#'
#' Converts the contents of a Source to Concept Map table into standard CDM vocabulary tables.
#' This includes creating entries in:
#' - CONCEPT table for source concepts
#' - CONCEPT_RELATIONSHIP table for mapping relationships
#' - Additional relationship types if using extended STCM format
#'
#' @param connection A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @param sourceToConceptMapTable Name of the source to concept map table
#'
#' @importFrom DatabaseConnector renderTranslateExecuteSql
#' @importFrom SqlRender readSql render translate
#'
#' @export
STCMToCDMTables <- function(connection,
                           vocabularyDatabaseSchema,
                           sourceToConceptMapTable
                           ) {
  
  # Input validation
  connection |> checkmate::assertClass("DBIConnection")
  vocabularyDatabaseSchema |> checkmate::assertString()
  sourceToConceptMapTable |> checkmate::assertString()
  
  # Get SQL script path
  sqlPath <- system.file(
    "sql", "sql_server", 
    "STCMExtendedToCDM.sql", 
    package = "ROMOPMappingTools"
  )
  
  # Read and render SQL
  sql <- SqlRender::readSql(sqlPath)
  sql <- SqlRender::render(sql,
                          vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                          sourceToConceptMapTable = sourceToConceptMapTable)
  
  # Translate to target dialect
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  
  # Execute SQL
  DatabaseConnector::executeSql(connection, sql)
  
  return(invisible(TRUE))
}
