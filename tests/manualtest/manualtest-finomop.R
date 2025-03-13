#
# Setting environment
#
pathToOMOPVocabularyCSVsFolder <- "../OMOP_vocabularies/data/input_omop_vocabulary"
pathToVocabularyFolder <- "../FinOMOP_mappings/VOCABULARIES"
pathToCodeCountsFolder <- "../FinOMOP_mappings/CODE_COUNTS"

# create a temporary copy of the OMOP vocabulary duckdb file
pathToOMOPVocabularyDuckDBfile <- tempfile(fileext = ".duckdb")

connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "duckdb",
    server = pathToOMOPVocabularyDuckDBfile
)

vocabularyDatabaseSchema <- "main"

connection <- DatabaseConnector::connect(connectionDetails)

ROMOPMappingTools::omopVocabularyCSVsToDuckDB(
    pathToOMOPVocabularyCSVsFolder = pathToOMOPVocabularyCSVsFolder,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
)

DatabaseConnector::disconnect(connection)

# Run function
validationResultsFolder <- file.path(tempdir(), "validationResults")
dir.create(validationResultsFolder, showWarnings = FALSE, recursive = TRUE)

validationLogTibble <- ROMOPMappingTools::runAll(
    pathToVocabularyFolder = pathToVocabularyFolder,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    pathToCodeCountsFolder = pathToCodeCountsFolder,
    validationResultsFolder = validationResultsFolder
)



# Validate individual usagi files
connection <- DatabaseConnector::connect(connectionDetails)

vocabularyId <- "ICD8fi"
sourceConceptIdOffset <- 2000300000
validationLogTibble <- ROMOPMappingTools::validateUsagiFile(
    pathToUsagiFile = file.path(pathToVocabularyFolder, vocabularyId, paste0(vocabularyId, ".usagi.csv")),
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    pathToValidatedUsagiFile = tempfile(fileext = ".csv"),
    sourceConceptIdOffset = sourceConceptIdOffset
)


updateLogTibble <- ROMOPMappingTools::updateUsagiFile(
    pathToUsagiFile = file.path(pathToVocabularyFolder, vocabularyId, paste0(vocabularyId, ".usagi.csv")),
    pathToUpdatedUsagiFile = tempfile(fileext = ".csv"),
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema, 
    skipValidation = TRUE
)
