

#' Validate OMOP Tables with Data Quality Dashboard
#'
#' This function validates the OMOP vocabulary tables using the Data Quality Dashboard package.
#'
#' @param connection_details A named list of database connection details
#' @importFrom DatabaseConnector connect dbListTables disconnect
#' @importFrom DataQualityDashboard executeDqChecks
#' @importFrom stringr str_to_lower
#' @return Results from the Data Quality Dashboard.
#'
#' @export

validateOMOPtablesWithDQD <- function(connection_details) {

  OMOP_vocabulary_table_names <- c(
    "CONCEPT",
    "CONCEPT_ANCESTOR",
    "CONCEPT_CLASS",
    "CONCEPT_RELATIONSHIP",
    "CONCEPT_SYNONYM",
    "DOMAIN",
    #"DRUG_STRENGTH", TEMP: loading makes error, leave out at the moment
    "RELATIONSHIP",
    "VOCABULARY"
  )

  connection <- DatabaseConnector::connect(connection_details)
  tablesToExclude <- setdiff(
    DatabaseConnector::dbListTables(connection),
    OMOP_vocabulary_table_names |> stringr::str_to_lower()
    )
  DatabaseConnector::disconnect(connection)

  results_DQD <- DataQualityDashboard::executeDqChecks(
    connectionDetails = connection_details,
    cdmDatabaseSchema = "main",
    resultsDatabaseSchema = "main",
    cdmSourceName = "tmp_vocab_table",
    outputFolder = tempdir(),
    tablesToExclude = tablesToExclude, cdmVersion = "5.4"
  )

  return(results_DQD)

}
