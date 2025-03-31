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

# filter for ICD10
concept <- dplyr::tbl(connection, "CONCEPT")
conceptAncestor <- dplyr::tbl(connection, "CONCEPT_ANCESTOR")
conceptClass <- dplyr::tbl(connection, "CONCEPT_CLASS")
conceptRelationship <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP")
conceptSynonym <- dplyr::tbl(connection, "CONCEPT_SYNONYM")
domain <- dplyr::tbl(connection, "DOMAIN")
relationship <- dplyr::tbl(connection, "RELATIONSHIP")
vocabulary <- dplyr::tbl(connection, "VOCABULARY")

# Vocabularies to include
vocabulariesToInclude <- c("ICD10")

# Vocabularies to map
# we need also all the concepts that will be mapped by the usagi files
pathToICD10fiUsagiFile <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_outdated.usagi.csv", package = "ROMOPMappingTools")
ICD10fiUsagiFile <- readUsagiFile(pathToICD10fiUsagiFile)
pathToUNITfiUsagiFile <- system.file("testdata/VOCABULARIES/UNITfi/UNITfi.usagi.csv", package = "ROMOPMappingTools")
UNITfiUsagiFile <- readUsagiFile(pathToUNITfiUsagiFile)
pathToICD10fiUsagiFileWithErrors <- system.file("testdata/VOCABULARIES/ICD10fi/ICD10fi_with_errors.usagi.csv", package = "ROMOPMappingTools")
ICD10fiUsagiFileWithErrors <- readUsagiFile(pathToICD10fiUsagiFileWithErrors)

allUsagiFiles <- dplyr::bind_rows(ICD10fiUsagiFile, UNITfiUsagiFile, ICD10fiUsagiFileWithErrors)

conceptIdsToMap <- allUsagiFiles |> dplyr::pull(conceptId) |> unique()
# add extra conceptIds to map
conceptIdsToMap <- c(conceptIdsToMap, 138604, 313219)


# For each table we have to add also the concepts to the concept table
# Concept
concept_codes <- concept |> 
dplyr::filter(
    vocabulary_id %in% vocabulariesToInclude |
    concept_id %in% conceptIdsToMap
)

# Concept relationship
# conceptRelationship_new <- conceptRelationship |>
#     dplyr::filter(relationship_id == "Maps to") |>
#     dplyr::semi_join(
#         concept_codes,
#         by = c("concept_id_1" = "concept_id")
#     )

conceptRelationship_new <- conceptRelationship |> 
    dplyr::filter(relationship_id %in% c("Maps to", "Concept replaced by", "Concept same_as to", "Concept poss_eq to", "Subsumes")) |>
    dplyr::semi_join(
        concept_codes |> dplyr::filter(is.na(standard_concept)),
        by = c("concept_id_1" = "concept_id")
    )

concept_codes <- concept_codes |>
    dplyr::union_all(
        concept |>
            dplyr::semi_join(conceptRelationship_new, by = c("concept_id" = "concept_id_2"))
    ) |> dplyr::distinct()

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
    dplyr::filter(relationship_id %in% c("Maps to", "Mapped from", "Is a", "Subsumes", "Concept replaced by", "Concept same_as to", "Concept poss_eq to"))

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
pathToTestDataFolder <- "inst/testdata/OMOPVocabulary"

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
pathToFullOMOPVocabularyDuckDBfile <- here::here("inst/testdata/OMOPVocabulary/OMOPVocabulary.duckdb")

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
validationLogR6 <- validateCDMtablesWithDQD(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = "main",
    validationResultsFolder = tempdir()
)

validationLogR6 |> dplyr::filter(type == "ERROR") |> nrow() |> testthat::expect_equal(0)
