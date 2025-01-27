#' Validate OMOP Tables with Data Quality Dashboard
#'
#' This function validates the OMOP vocabulary tables using the Data Quality Dashboard package.
#'
#' @param connection A DatabaseConnector connection
#' @param vocabularyDatabaseSchema A string with the name of the schema where the vocabulary tables are stored
#' @importFrom DatabaseConnector dbListTables
#' @importFrom DataQualityDashboard executeDqChecks
#' @importFrom checkmate assertClass assertString
#' @return Results from the Data Quality Dashboard.
#'
#' @export

validateCDMtablesWithDQD <- function(connectionDetails, vocabularyDatabaseSchema) {
  # check if the connection is a DatabaseConnector connection
  connectionDetails |> checkmate::assertClass("ConnectionDetails")
  vocabularyDatabaseSchema |> checkmate::assertString()

  connection <- DatabaseConnector::connect(connectionDetails)
  tableNames <- DatabaseConnector::dbListTables(connection, vocabularyDatabaseSchema)
  DatabaseConnector::disconnect(connection)

  OMOPVocabularyTableNames <- c(
    "concept",
    "concept_ancestor",
    "concept_class",
    "concept_relationship",
    "concept_synonym",
    "domain",
    "drug_strength",
    "relationship",
    "vocabulary"
  ) 
  
  OMOPVocabularyTableNames |>
    checkmate::assertSubset(tableNames)

  tablesToExclude <- setdiff(
    tableNames,
    OMOPVocabularyTableNames
  )

  resultsDQD <- DataQualityDashboard::executeDqChecks(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = vocabularyDatabaseSchema,
    resultsDatabaseSchema = vocabularyDatabaseSchema,
    cdmSourceName = "tmp_vocab_table",
    outputFolder = tempdir(),
    tablesToExclude = tablesToExclude, cdmVersion = "5.4"
  )

  return(resultsDQD)
}
