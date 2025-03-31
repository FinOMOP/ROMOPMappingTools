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

validationLogTibble <- buildVocabulariesAll(
    pathToVocabularyFolder = pathToVocabularyFolder,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    validationResultsFolder = validationResultsFolder
)



# Validate individual usagi files
connection <- DatabaseConnector::connect(connectionDetails)

concept  <- dplyr::tbl(connection, "CONCEPT") 
concept   |> count(concept_id) |> arrange(desc(n))
concept   |> filter(concept_id == 2001900015) |> collect() |> View()

vocabularyId <- "SNOMED2fi"

pathToVocabularyInfoFile <- file.path(pathToVocabularyFolder, "vocabularies.csv")
vocabulariesTibble <- readr::read_csv(pathToVocabularyInfoFile, show_col_types = FALSE)
sourceConceptIdOffset <- vocabulariesTibble |> dplyr::filter(source_vocabulary_id == vocabularyId) |> dplyr::pull(source_concept_id_offset)
pathToUsagiFile <- file.path(pathToVocabularyFolder, vocabularyId, paste0(vocabularyId, ".usagi.csv"))

usagiTibble <- readUsagiFile(pathToUsagiFile)

pathToValidatedUsagiFile  <- tempfile(fileext = ".csv")
validationLogTibble <- validateUsagiFile(
    pathToUsagiFile = pathToUsagiFile,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    pathToValidatedUsagiFile = pathToValidatedUsagiFile,
    sourceConceptIdOffset = sourceConceptIdOffset
)

validatedUsagiFile <- readUsagiFile(pathToValidatedUsagiFile)
validatedUsagiFile |> dplyr::filter(!is.na(`ADD_INFO:validationMessages`)) |> View()

pathToUpdatedUsagiFile <- tempfile(fileext = ".csv")
updateLogTibble <- ROMOPMappingTools::updateUsagiFile(
    pathToUsagiFile = pathToUsagiFile,
    pathToUpdatedUsagiFile = pathToUpdatedUsagiFile,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema, 
    skipValidation = TRUE
)

pathToValidatedUsagiFileUpdated <- tempfile(fileext = ".csv")
validationLogTibbleUpdatedUsagiFile <- ROMOPMappingTools::validateUsagiFile(
    pathToUsagiFile = pathToUpdatedUsagiFile,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    pathToValidatedUsagiFile = pathToValidatedUsagiFileUpdated,
    sourceConceptIdOffset = sourceConceptIdOffset
)

pathToValidatedUsagiFileUpdated  |> readUsagiFile()|> filter(!is.na(`ADD_INFO:validationMessages`)) |> View()

updatedUsagiFile <- readUsagiFile(pathToValidatedUsagiFile)
updatedUsagiFile |> dplyr::filter(!is.na(`ADD_INFO:validationMessages`)) |> View()
updatedUsagiFile %>% dplyr::filter(conceptId == 0) %>% dplyr::count(mappingStatus)


updatedUsagiFile <- readUsagiFile(pathToUsagiFile)
updatedUsagiFile %>% dplyr::filter(conceptId == 0) %>% dplyr::count(mappingStatus)
