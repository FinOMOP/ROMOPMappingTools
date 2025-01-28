pathToFullOMOPVocabularyCSVsFolder <- "~/Documents/Repos/FinOMOP/FinOMOP_OMOP_vocabulary/OMOP_VOCABULARIES/input_omop_vocabulary"

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

# filter for ICD10
concept <- dplyr::tbl(connection, "CONCEPT")
conceptAncestor <- dplyr::tbl(connection, "CONCEPT_ANCESTOR")
conceptClass <- dplyr::tbl(connection, "CONCEPT_CLASS")
conceptRelationship <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP")
conceptSynonym <- dplyr::tbl(connection, "CONCEPT_SYNONYM")
domain <- dplyr::tbl(connection, "DOMAIN")
relationship <- dplyr::tbl(connection, "RELATIONSHIP")
vocabulary <- dplyr::tbl(connection, "VOCABULARY")


# For each table we have to add also the concepts to the concept table
# Concept
concept_codes <- concept |> dplyr::filter(vocabulary_id == "ICD10")

# Concept relationship
conceptRelationship_new <- conceptRelationship |>
    dplyr::filter(relationship_id == "Maps to") |>
    dplyr::semi_join(
        concept_codes,
        by = c("concept_id_1" = "concept_id")
    )

concept_codes <- concept_codes |>
    dplyr::union_all(
        concept |>
            dplyr::semi_join(conceptRelationship_new, by = c("concept_id" = "concept_id_2"))
    )

# Concept ancestor
conceptAncestor_new <- conceptAncestor |>
    dplyr::filter(FALSE)

# Concept class
conceptClass_new <- conceptClass |>
    dplyr::filter(concept_class_id %in% c("Domain", "Vocabulary", "Concept Class", "Relationship")) |>
    dplyr::union_all(
        conceptClass |>
            dplyr::semi_join(
                concept_codes,
                by = c("concept_class_id" = "concept_class_id")
            )
    )

concept_conceptClass <- concept |>
    dplyr::semi_join(
        conceptClass_new,
        by = c("concept_id" = "concept_class_concept_id")
    )

# Concept synonym
conceptSynonym_empty <- conceptSynonym |>
    dplyr::filter(FALSE)

# Domain
domain <- domain

concept_domain <- concept |>
    dplyr::semi_join(
        domain,
        by = c("concept_id" = "domain_concept_id")
    )

# Relationship
relationship_new <- relationship |>
    dplyr::filter(relationship_id %in% c("Maps to", "Mapped from", "Is a", "Subsumes"))

concept_relationship <- concept |>
    dplyr::semi_join(
        relationship_new,
        by = c("concept_id" = "relationship_concept_id")
    )

# Vocabulary
vocabulary_new <- vocabulary |>
    dplyr::filter(vocabulary_id %in% c("Domain", "Vocabulary", "Concept Class", "Relationship")) |>
    dplyr::union_all(
        vocabulary |>
            dplyr::semi_join(
                concept_codes,
                by = c("vocabulary_id" = "vocabulary_id")
            )
    )

concept_vocabulary <- concept |>
    dplyr::semi_join(
        vocabulary_new,
        by = c("concept_id" = "vocabulary_concept_id")
    )

# secondary concepts
concept_all <- concept_codes |>
    dplyr::union_all(concept_conceptClass) |>
    dplyr::union_all(concept_domain) |>
    dplyr::union_all(concept_relationship) |>
    dplyr::union_all(concept_vocabulary)



# write to csv
pathToTestDataFolder <- "inst/testdata/OMOPVocabularyICD10only"

concept_all |>
    dplyr::collect() |>
    dplyr::mutate(across(where(lubridate::is.Date), ~ format(., "%Y%m%d"))) |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT.csv"), na = "")
conceptAncestor_new |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_ANCESTOR.csv"), na = "")
conceptClass_new |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_CLASS.csv"), na = "")
conceptRelationship_new |>
    dplyr::collect() |>
    dplyr::mutate(across(where(lubridate::is.Date), ~ format(., "%Y%m%d"))) |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_RELATIONSHIP.csv"), na = "")
conceptSynonym_empty |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "CONCEPT_SYNONYM.csv"), na = "")
domain |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "DOMAIN.csv"), na = "")
relationship_new |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "RELATIONSHIP.csv"), na = "")
vocabulary_new |>
    dplyr::collect() |>
    readr::write_tsv(file.path(pathToTestDataFolder, "VOCABULARY.csv"), na = "")

DatabaseConnector::disconnect(connection)

# test files are correct
pathToFullOMOPVocabularyDuckDBfile <- here::here("inst/testdata/OMOPVocabularyICD10only/OMOPVocabularyICD10only.duckdb")

file.remove(pathToFullOMOPVocabularyDuckDBfile)
file.remove(paste0(pathToFullOMOPVocabularyDuckDBfile, ".wal"))

connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "duckdb",
    server = pathToFullOMOPVocabularyDuckDBfile
)

connection <- DatabaseConnector::connect(connectionDetails)

omopVocabularyCSVsToDuckDB(
    pathToOMOPVocabularyCSVsFolder = pathToTestDataFolder,
    connection = connection,
    vocabularyDatabaseSchema = "main"
)
DatabaseConnector::disconnect(connection)


# test if the data is correct
validationLogTibble <- validateCDMtablesWithDQD(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = "main",
    validationResultsFolder = tempdir()
)

validationLogTibble |> dplyr::filter(type == "ERROR") |> nrow() |> testthat::expect_equal(0)