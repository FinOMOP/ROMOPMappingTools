#' Validate CDM Tables Using Data Quality Dashboard
#'
#' Validates the CDM vocabulary tables using the OHDSI Data Quality Dashboard.
#' This performs standard CDM vocabulary table checks to ensure data quality and consistency.
#'
#' @param connectionDetails DatabaseConnector connection details object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @param validationResultsFolder Folder where validation results will be saved
#'
#' @return A tibble containing validation results
#'
#' @importFrom DatabaseConnector connect disconnect
#' @importFrom DataQualityDashboard executeDqChecks
#'
#' @export
validateCDMtablesWithDQD <- function(connectionDetails, vocabularyDatabaseSchema, validationResultsFolder) {
  # check if the connection is a DatabaseConnector connection
  connectionDetails |> checkmate::assertClass("ConnectionDetails")
  vocabularyDatabaseSchema |> checkmate::assertString()
  validationResultsFolder |> checkmate::assertDirectory()

  connection <- DatabaseConnector::connect(connectionDetails)
  tableNames <- DatabaseConnector::getTableNames(connection, vocabularyDatabaseSchema)
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

  jsonlite::write_json(resultsDQD, file.path(validationResultsFolder, "resultsDQD.json"))

  # convert resultsDQD to a validationLogR6
  validationLogR6 <- resultsDQD$CheckResults |> 
    tibble::as_tibble() |> 
    dplyr::transmute( 
      context = "DQD",
      type = dplyr::case_when(
        notApplicable == 1  | !is.na(error) | !is.na(warning) ~ "WARNING",
        failed == 1 ~ "ERROR",
        TRUE ~ "SUCCESS"
      ),
      step = paste0(cdmTableName, dplyr::if_else(is.na(cdmFieldName), "", paste0(".", cdmFieldName)), ": ", checkDescription),
      message = paste0("Number of violated rows: ", numViolatedRows)
    )



  return(validationLogR6)
}
