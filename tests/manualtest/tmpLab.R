#
# Build the OMOP vocabulary duckdb file
#
pathToFullOMOPVocabularyCSVsFolder <- "../OMOP_vocabularies/data/input_omop_vocabulary"

# convert to duckdb
pathToFullOMOPVocabularyDuckDBfile <- tempfile()

connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "duckdb",
    server = pathToFullOMOPVocabularyDuckDBfile
)

connection <- DatabaseConnector::connect(connectionDetails)

omopVocabularyCSVsToDuckDB(
    pathToOMOPVocabularyCSVsFolder = pathToFullOMOPVocabularyCSVsFolder,
    connection = connection,
    vocabularyDatabaseSchema = "main"
)

DatabaseConnector::disconnect(connection)

pathToOMOPVocabularyDuckDBfile <- pathToFullOMOPVocabularyDuckDBfile

#
# Validate the LAB fi usagi file
#
pathToUsagiFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/LABfi_ALL.usagi.csv", package = "ROMOPMappingTools")

pathToUnitConversionFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/quantity_source_unit_conversion.tsv", package = "ROMOPMappingTools")
pathToValidUnitsFile <- system.file("testdata/VOCABULARIES/UNITfi/UNITfi.usagi.csv", package = "ROMOPMappingTools")

pathToValidatedUsagiFile <- tempfile(fileext = ".csv")
pathToValidatedUnitConversionFile <- tempfile(fileext = ".tsv")
vocabularyDatabaseSchema <- "main"
sourceConceptIdOffset <- 2002400000


# Create connection to test database
connection <- DatabaseConnector::connect(
    dbms = "duckdb",
    server = pathToOMOPVocabularyDuckDBfile
)

validationsSummary <- validateUsagiFile(
    pathToUsagiFile,
    connection,
    vocabularyDatabaseSchema,
    pathToValidatedUsagiFile,
    sourceConceptIdOffset,
    pathToValidUnitsFile,
    pathToUnitConversionFile,
    pathToValidatedUnitConversionFile
)

DatabaseConnector::disconnect(connection)

validationsSummary
if (nrow(validationsSummary |> dplyr::filter(type == "ERROR")) > 0) {
    pathToValidatedUnitConversionFile |>
        readr::read_tsv() |>
        View()


    validatedUsagiFile <- readUsagiFile(pathToValidatedUsagiFile)


    validatedUsagiFile |>
        dplyr::filter(stringr::str_detect(`ADD_INFO:validationMessages`, "with omop_quantity ")) |>
        dplyr::filter(stringr::str_detect(`ADD_INFO:validationMessages`, "ERROR")) |>
        select(sourceName, `ADD_INFO:validationMessages`) |>
        View()
}


#
# Build the status dashboard
#
pathToCodeCountsLabFolder <- system.file("testdata/CODE_COUNTS/databases/LABfi_FinnGenDF13", package = "ROMOPMappingTools")
pathToVocabularyLabFolder <- system.file("testdata/VOCABULARIES/LABfi_ALL", package = "ROMOPMappingTools")

connection <- DatabaseConnector::connect(
    dbms = "duckdb",
    server = pathToOMOPVocabularyDuckDBfile
)
#DatabaseConnector::disconnect(connection)

dashboardFolder <- tempdir()

summary <- .readCodeCountsLabFolder(pathToCodeCountsLabFolder)
summary <- .calcualteKStest(summary)
concept <- dplyr::tbl(connection, "CONCEPT") |>
    dplyr::filter(concept_id %in% summary$OMOP_CONCEPT_ID) |>
    dplyr::select(concept_id, concept_name) |>
    dplyr::collect()
summary <- summary |>
    dplyr::left_join(concept, by = c("OMOP_CONCEPT_ID" = "concept_id"))


usagiFile <- readUsagiFile(pathToValidatedUsagiFile)
approvedUsagiFile <- usagiFile |>
    dplyr::filter(mappingStatus == "APPROVED") |>
    dplyr::select(
        OMOP_CONCEPT_ID = conceptId,
        TEST_NAME = `ADD_INFO:testNameAbbreviation`, 
        MEASUREMENT_UNIT = `ADD_INFO:measurementUnit`,
        message = `ADD_INFO:validationMessages`) 

summary <- summary |>
    dplyr::left_join(approvedUsagiFile, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT"))



.summaryToSummaryTable(summary) 



