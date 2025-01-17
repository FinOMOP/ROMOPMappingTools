#' Convert OMOP Vocabulary CSVs to Database
#'
#' This function converts OMOP vocabulary CSV files to a Database.
#'
#' @param pathToOMOPVocabularyCSVsFolder Path to folder containing OMOP vocabulary CSV files
#' @param connection A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @importFrom DatabaseConnector createConnectionDetails connect dbExecute disconnect
#' @importFrom SqlRender readSql render translate
#' @importFrom checkmate assertDirectoryExists assertSubset
#'
#' @return Nothing
#'
#' @export

omopVocabularyCSVsToDuckDB <- function(
    pathToOMOPVocabularyCSVsFolder,
    connection,
    vocabularyDatabaseSchema
    ) {
    OMOPVocabularyTableNames <- c(
        "CONCEPT",
        "CONCEPT_ANCESTOR",
        "CONCEPT_CLASS",
        "CONCEPT_RELATIONSHIP",
        "CONCEPT_SYNONYM",
        "DOMAIN",
        # "DRUG_STRENGTH", TEMP: loading makes error, leave out at the moment
        "RELATIONSHIP",
        "VOCABULARY"
    )

    ###
    ## validate input parameters
    ###

    pathToOMOPVocabularyCSVsFolder |> checkmate::assertDirectoryExists()

    pathToOMOPVocabularyCSVsFolder |>
        list.files() |>
        checkmate::checkSetEqual(
            OMOPVocabularyTableNames |> paste0(".csv")
        )

    ###
    ## function
    ##

    # create cdm tables
    sql <- SqlRender::readSql(system.file("ddl/5.4/sql_server/OMOPCDM_sql_server_5.4_ddl.sql", package = "ROMOPMappingTools", mustWork = TRUE))
    sql <- SqlRender::render(
        sql = sql,
        cdmDatabaseSchema = vocabularyDatabaseSchema
    )
    sql <- SqlRender::translate(
        sql = sql,
        targetDialect = "duckdb"
    )

    DatabaseConnector::dbExecute(connection, sql)

    # Load vocabulary tables
    for (table_name in OMOPVocabularyTableNames) {
        table_full_path <- file.path(pathToOMOPVocabularyCSVsFolder, paste0(table_name, ".csv"))
        DatabaseConnector::dbExecute(
            conn = connection,
            statement = paste0("COPY ", table_name, " FROM '", table_full_path, "' (DATEFORMAT '%Y%m%d', DELIM '\t', HEADER true, QUOTE '')")
        )
    }

    # DQD needs the cdm_source table
    DatabaseConnector::dbExecute(connection, "INSERT INTO main.cdm_source VALUES ('tmp_vocab_table', 'tmp_vocab_table', 'tmp_vocab_table', '', '', '', DATE '1992-09-20' , DATE '1992-09-20', '', 1, 'test')")

}
