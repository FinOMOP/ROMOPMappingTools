#' Create Extended Source to Concept Map Table
#'
#' Creates an extended version of the Source to Concept Map table that includes additional columns
#' for source concept class, domain, and parent relationships.
#'
#' @param connection A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @param sourceToConceptMapTable Name of the extended source to concept map table to create
#' @param overwrite Whether to overwrite an existing table. Default is TRUE
#'
#' @importFrom DatabaseConnector renderTranslateExecuteSql
#'
#' @export
createSourceToConceptMapExtended <- function(connection, vocabularyDatabaseSchema, sourceToConceptMapTable, overwrite = TRUE) {
  
  # Input validation
  connection |> checkmate::assertClass("DBIConnection")
  sourceToConceptMapTable |> checkmate::assertString()
  
  # Get the SQL script path
  sqlPath <- system.file(
    "ddl", "5.4", "sql_server", 
    "createSourceToConceptMapExtended.sql", 
    package = "ROMOPMappingTools"
  )

  sql <- SqlRender::readSql(sqlPath)
  
  # Render and execute the SQL script
  DatabaseConnector::renderTranslateExecuteSql(
    connection,
    sql,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    sourceToConceptMapTable = sourceToConceptMapTable,
    overwrite = overwrite
  )
  
} 