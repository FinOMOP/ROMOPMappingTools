#' Convert CONCEPT_RELATIONSHIP to CONCEPT_ANCESTOR
#'
#' @description
#' Creates and populates the CONCEPT_ANCESTOR table 
#' from CONCEPT_RELATIONSHIP table data.
#'
#' @param connection           A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema containing the vocabulary and STCM tables
#' @param sourceToConceptMapTable Name of the SOURCE_TO_CONCEPT_MAP_EXTENDED table
#'
#' @return
#' Invisible TRUE if successful
#'
#' @export
ConceptRelationshipToAncestorTables <- function(connection,
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
    "CONCEPT_RELATIONSHIPToANCESTOR.sql", 
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
