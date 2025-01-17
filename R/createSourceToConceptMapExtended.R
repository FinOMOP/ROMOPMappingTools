#' Create SOURCE_TO_CONCEPT_MAP_EXTENDED table
#'
#' @description
#' Creates the SOURCE_TO_CONCEPT_MAP_EXTENDED table in the specified schema
#'
#' @param connectionDetails    An R object of type \code{connectionDetails} created using the
#'                            function \code{createConnectionDetails} in the
#'                            \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema   The name of the database schema that will contain the OMOP CDM
#'                           tables. Requires appropriate user permissions to create new tables.
#' @param overwrite          If TRUE, will drop existing table before creating new one. If FALSE,
#'                          will raise an error if table already exists. Default is TRUE.
#'
#' @return
#' TRUE if table is created successfully
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