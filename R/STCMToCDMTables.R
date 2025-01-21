#' Convert SOURCE_TO_CONCEPT_MAP_EXTENDED to CDM Tables
#'
#' @description
#' Creates and populates CDM tables (CONCEPT, CONCEPT_RELATIONSHIP, CONCEPT_ANCESTOR) 
#' from SOURCE_TO_CONCEPT_MAP_EXTENDED table data.
#'
#' @param connection           A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema containing the vocabulary and STCM tables
#' @param sourceToConceptMapTable Name of the SOURCE_TO_CONCEPT_MAP_EXTENDED table
#'
#' @return
#' Invisible TRUE if successful
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
