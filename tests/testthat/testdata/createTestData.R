pathToFullOMOPVocabularyCSVsFolder <- "~/Documents/Repos/FinOMOP/FinOMOP_OMOP_vocabulary/OMOP_VOCABULARIES/input_omop_vocabulary"

# convert to duckdb
pathToFullOMOPVocabularyDuckDBfile <- tempfile()

connection <- DatabaseConnector::connect(
    dbms = "duckdb",
    server = pathToFullOMOPVocabularyDuckDBfile
)

omopVocabularyCSVsToDuckDB(
    pathToOMOPVocabularyCSVsFolder = pathToFullOMOPVocabularyCSVsFolder,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
)

# filter for ICD10
concept <- dplyr::tbl(connection, "CONCEPT")   
concept_ancestor <- dplyr::tbl(connection, "CONCEPT_ANCESTOR")
conceptClass <- dplyr::tbl(connection, "CONCEPT_CLASS")
conceptRelationship <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP")
conceptSynonym <- dplyr::tbl(connection, "CONCEPT_SYNONYM")
domain <- dplyr::tbl(connection, "DOMAIN")
relationship <- dplyr::tbl(connection, "RELATIONSHIP")
vocabulary <- dplyr::tbl(connection, "VOCABULARY")

conceptICD10 <- concept |> dplyr::filter(vocabulary_id == "ICD10")
conceptICD10Ancestor <- concept_ancestor |>
    dplyr::filter(FALSE)
conceptClassICD10 <- conceptClass |>
    dplyr::semi_join(
        conceptICD10,
        by = c("concept_class_id" = "concept_class_id")
    )
conceptRelationshipICD10 <- conceptRelationship |>
    dplyr::filter(relationship_id == "Maps to") |>
    dplyr::semi_join(
        conceptICD10,
        by = c("concept_id_1" = "concept_id")
    )
conceptSynonymICD10 <- conceptSynonym |>
    dplyr::filter(FALSE)
domainICD10 <- domain
relationshipICD10 <- relationship |>
    dplyr::filter(FALSE)
vocabularyICD10 <- vocabulary |>
    dplyr::semi_join(
        conceptICD10,
        by = c("vocabulary_id" = "vocabulary_id")
    )

# write to csv
pathToTestDataFolder <- "tests/testthat/testdata/OMOPVocabularyICD10only"

conceptICD10 |>
    dplyr::collect() |>
    dplyr::mutate(across(where(lubridate::is.Date), ~ format(., "%Y%m%d"))) |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT.csv"), na = "")
conceptICD10Ancestor |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_ANCESTOR.csv"), na = "")
conceptClassICD10 |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_CLASS.csv"), na = "")
conceptRelationshipICD10 |>
    dplyr::collect() |>
    dplyr::mutate(across(where(lubridate::is.Date), ~ format(., "%Y%m%d"))) |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_RELATIONSHIP.csv"), na = "")
conceptSynonymICD10 |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_SYNONYM.csv"), na = "")
domainICD10 |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "DOMAIN.csv"), na = "")
relationshipICD10 |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "RELATIONSHIP.csv"), na = "")
vocabularyICD10 |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "VOCABULARY.csv"), na = "")

DatabaseConnector::disconnect(connection)

# test files are correct
pathToFullOMOPVocabularyDuckDBfile <- testthat::test_path("testdata/OMOPVocabularyICD10only/OMOPVocabularyICD10only.duckdb")

connection <- DatabaseConnector::connect(
    dbms = "duckdb",
    server = pathToFullOMOPVocabularyDuckDBfile
)

omopVocabularyCSVsToDuckDB(
    pathToOMOPVocabularyCSVsFolder = pathToTestDataFolder,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
