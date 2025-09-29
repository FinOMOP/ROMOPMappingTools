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
    vocabularyDatabaseSchema) {
    OMOPVocabularyTableNames <- c(
        "CONCEPT",
        "CONCEPT_ANCESTOR",
        "CONCEPT_CLASS",
        "CONCEPT_RELATIONSHIP",
        "CONCEPT_SYNONYM",
        "DOMAIN",
        "DRUG_STRENGTH", 
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


#' Export vocabulary tables from a database to CSV files
#'
#' @param connection DatabaseConnector connection object to the database containing vocabulary tables
#' @param vocabularyDatabaseSchema Schema name where vocabulary tables are located
#' @param OMOPVocabularyTableNames Vector of vocabulary table names to export. If NULL, exports standard set.
#' @param pathToOMOPVocabularyCSVsFolder Directory path where CSV files will be written
#' @importFrom DatabaseConnector dbListTables dbReadTable
#' @importFrom dplyr rename_all mutate across
#' @importFrom readr write_tsv
#'
#' @return No return value, called for side effects
#' @export
duckdbToOMOPVocabularyCSVs <- function(
    connection,
    vocabularyDatabaseSchema,
    OMOPVocabularyTableNames,
    pathToOMOPVocabularyCSVsFolder) {
    if (is.null(OMOPVocabularyTableNames)) {
        OMOPVocabularyTableNames <- c(
            "CONCEPT",
            "CONCEPT_ANCESTOR",
            "CONCEPT_CLASS",
            "CONCEPT_RELATIONSHIP",
            "CONCEPT_SYNONYM",
            "DOMAIN",
            "DRUG_STRENGTH",
            "RELATIONSHIP",
            "VOCABULARY"
        )
    }

    #
    # Validate input parameters
    #

    connection |> checkmate::assertClass("DatabaseConnectorDbiConnection")
    vocabularyDatabaseSchema |> checkmate::assertCharacter()

    OMOPVocabularyTableNames |>
        stringr::str_to_lower() |>
        checkmate::assertSubset(
            DatabaseConnector::getTableNames(connection)
        )

    pathToOMOPVocabularyCSVsFolder |> checkmate::assertDirectoryExists()

    #
    # Function
    #

    for (table_name in OMOPVocabularyTableNames) {
        message("Exporting table: ", table_name)
        out_path <- file.path(pathToOMOPVocabularyCSVsFolder, paste0(table_name, ".csv"))
        
        col_info <- DBI::dbGetQuery(
            connection,
            paste0("PRAGMA table_info(", table_name, ");")
        )
        cols <- col_info$name
        date_cols <- col_info$name[grepl("^date$", tolower(col_info$type))]
        
        select_cols <- sapply(cols, function(col) {
            if (col %in% date_cols) {
                paste0("STRFTIME('%Y%m%d', ", col, ") AS ", col)
            } else {
                col
            }
        })

        select_sql <- paste(select_cols, collapse = ", ")
        sql <- paste0("COPY (SELECT ", select_sql, " FROM ", table_name, ") TO '", out_path, "' (HEADER, DELIM '\t');")
        DatabaseConnector::dbExecute(connection, sql)
    }

    DatabaseConnector::disconnect(connection)
}
