

#' Create a Temporal Database with OMOP Vocabulary Tables
#'
#' This function creates a temporary database and load the OMOP vocabulary tables from `path_to_omop_vocabulary_folder`.
#'
#' @param path_to_omop_vocabulary_folder The path to the folder containing the OMOP vocabulary tables in CSV format.
#' @importFrom DatabaseConnector createConnectionDetails connect dbExecute disconnect
#' @importFrom SqlRender readSql render translate
#' @importFrom checkmate assertDirectoryExists assertSubset
#' @return Connection details to the temporal database
#' @export


createTemporalDatabaseWithOMOPtable <- function(path_to_omop_vocabulary_folder) {


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

  ###
  ## validate input parameters
  ###

  path_to_omop_vocabulary_folder |> checkmate::assertDirectoryExists()

  checkmate::assertSubset(
    OMOP_vocabulary_table_names |> paste0(".csv"),
    path_to_omop_vocabulary_folder |> list.files()
  )

  ###
  ## function
  ###

  tempfile <- file.path(tempdir(),"tmp.duckdb")
  if(file.exists(tempfile)){file.remove(tempfile)}
  connection_details <- DatabaseConnector::createConnectionDetails(
    dbms = "duckdb",
    server = tempfile,
    pathToDriver = "../" #TEMP: needs an empty file to work
  )

  connection <- DatabaseConnector::connect(connection_details)

  # create cdm tables
  sql <- SqlRender::readSql(system.file("ddl/5.4/sql_server/OMOPCDM_sql_server_5.4_ddl.sql", package = "ROMOPMappingTools", mustWork = TRUE))
  sql <- SqlRender::render(
    sql = sql,
    cdmDatabaseSchema = "main"
  )
  sql <- SqlRender::translate(
    sql = sql,
    targetDialect = "duckdb"
  )

  DatabaseConnector::dbExecute(connection, sql)

  # Load vocabulary tables
  for(table_name in OMOP_vocabulary_table_names){

    table_full_path <- file.path(path_to_omop_vocabulary_folder, paste0(table_name,".csv"))
    DatabaseConnector::dbExecute(
      conn = connection,
      statement = paste0("COPY ", table_name," FROM '",table_full_path,"' (DATEFORMAT '%Y%m%d', DELIM '\t', HEADER true, QUOTE '')"))

  }

  # DQD needs the cdm_source table
  DatabaseConnector::dbExecute(connection, "INSERT INTO main.cdm_source VALUES ('tmp_vocab_table', 'tmp_vocab_table', 'tmp_vocab_table', '', '', '', DATE '1992-09-20' , DATE '1992-09-20', '', 1, 'test')")

  #
  DatabaseConnector::disconnect(connection)

  return(connection_details)

}
