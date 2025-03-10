#' Append Usagi File to Source to Concept Map Table
#'
#' Reads a Usagi mapping file and appends its contents to a Source to Concept Map table.
#' The function supports both default and extended formats for both Usagi files and STCM tables.
#'
#' Default STCM columns:
#' - source_code, source_concept_id, source_vocabulary_id, source_code_description
#' - target_concept_id, target_vocabulary_id, valid_start_date, valid_end_date
#' - invalid_reason
#'
#' Extended STCM columns (includes all default columns plus):
#' - source_concept_class
#' - source_domain
#' - source_parents_concept_ids
#'
#' Extended Usagi columns:
#' - ADD_INFO:sourceConceptId
#' - ADD_INFO:sourceConceptClass
#' - ADD_INFO:sourceDomain
#'
#' If formats don't match (e.g., extended Usagi with default STCM), the extended information
#' will be ignored with a warning.
#'
#' @param vocabularyId String with the vocabulary ID
#' @param pathToUsagiFile Path to the Usagi file
#' @param connection Database connection object
#' @param vocabularyDatabaseSchema Schema name containing vocabulary tables
#' @param sourceToConceptMapTable Name of source to concept map table
#' @param includeMappingStatus Vector of mapping statuses to include. Must be subset of:
#'        "APPROVED", "UNCHECKED", "FLAGGED", "INEXACT", "INVALID_TARGET"
#' @param skipValidation Whether to skip validation of the Usagi file (default: TRUE)
#' @param sourceConceptIdOffset Offset to add to source concept IDs (default: 0)
#'
#' @return NULL invisibly. The function modifies the database table directly.
#'
#' @importFrom checkmate assertString assertFileExists assertSubset
#' @importFrom DBI dbListTables dbListFields
#' @importFrom dplyr transmute mutate select filter row_number left_join group_by summarise ungroup pull bind_rows collect distinct
#' @importFrom lubridate as_date ymd
#' @importFrom stringr str_split
#' @importFrom purrr flatten_chr map2
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @importFrom DatabaseConnector renderTranslateExecuteSql dbWriteTable
#'
#' @export
appendUsagiFileToSTCMtable <- function(
    vocabularyId,
    pathToUsagiFile,
    connection,
    vocabularyDatabaseSchema,
    sourceToConceptMapTable,
    includeMappingStatus = "APPROVED",
    skipValidation = TRUE,
    sourceConceptIdOffset = 0
) {
    vocabularyId |> checkmate::assertString()
    pathToUsagiFile |> checkmate::assertFileExists()
    vocabularyDatabaseSchema |> checkmate::assertString()
    includeMappingStatus |> checkmate::assertSubset(c("APPROVED", "UNCHECKED", "FLAGGED", "INEXACT", "INVALID_TARGET"))
    #
    # Validation
    #

    # check if the sourceToConceptMapTable exists
    listTables <- DBI::dbListTables(connection, vocabularyDatabaseSchema)
    if (!sourceToConceptMapTable %in% listTables) {
        stop(paste0("The sourceToConceptMapTable ", sourceToConceptMapTable, " does not exist in the vocabularyDatabaseSchema ", vocabularyDatabaseSchema))
    }

    # check the sourceToConceptMapTable has the default columns or the extended columns
    STCMTableIsExtended <- FALSE
    stcmTableColumns <- DBI::dbListFields(connection, sourceToConceptMapTable)
    stcmDefaultColumns <- c(
        "source_code", "source_concept_id", "source_vocabulary_id", "source_code_description",
        "target_concept_id", "target_vocabulary_id", "valid_start_date", "valid_end_date",
        "invalid_reason"
    )
    stcmExtendedColumns <- c(
        "source_code", "source_concept_id", "source_vocabulary_id", "source_code_description",
        "target_concept_id", "target_vocabulary_id", "valid_start_date", "valid_end_date",
        "invalid_reason", "source_concept_class", "source_domain", "source_parents_concept_ids"
    )

    if (all(stcmExtendedColumns %in% stcmTableColumns)) {
        STCMTableIsExtended <- TRUE
    }
    if (!all(stcmDefaultColumns %in% stcmTableColumns) && !STCMTableIsExtended) {
        stop(paste0("The sourceToConceptMapTable ", sourceToConceptMapTable, " does not have the correct columns. It must have the default columns or the extended columns."))
    }


    # Read the usagi file
    usagiTibble <- readUsagiFile(pathToUsagiFile)
    usagiTibbleColumns <- usagiTibble |> names()
    if (!skipValidation) {
        usagiTibble <- validateUsagiFile(pathToUsagiFile, connection, vocabularyDatabaseSchema, tempfile(), sourceConceptIdOffset)
        if (usagiTibble |> dplyr::filter(type == "ERROR") |> nrow() > 0) {
            stop("The usagi file has the following errors: ", usagiTibble |> dplyr::filter(type == "ERROR") |> dplyr::pull(message) |> paste(collapse = "\n"))
        }
    }

    usagiIsExtended <- FALSE
    usagiExtendedColumns <- c(
        "ADD_INFO:sourceConceptId",
        "ADD_INFO:sourceConceptClass",
        "ADD_INFO:sourceDomain"
    )

    if (all(usagiExtendedColumns %in% usagiTibbleColumns)) {
        usagiIsExtended <- TRUE
    }

    # if usagi is extended but stcm is not extended, warn that the usagi extended columns will be ignored
    if (usagiIsExtended && !STCMTableIsExtended) {
        warning("The usagi file is extended but the sourceToConceptMapTable is not extended. The usagi extended columns will be ignored.")
    }

    # if usagi is not extended but stcm is extended, warn that the stcm extended columns will be ignored
    if (!usagiIsExtended && STCMTableIsExtended) {
        warning("The usagi file is not extended but the sourceToConceptMapTable is extended. The stcm extended columns will be ignored.")
    }

    #
    # Function
    #
    if (!STCMTableIsExtended) {
        STCMTableToInsert <- usagiTibble |>
            dplyr::filter(mappingStatus %in% includeMappingStatus) |>
            dplyr::transmute(
                source_code = sourceCode,
                source_concept_id = 0L,
                source_vocabulary_id = {{ vocabularyId }},
                source_code_description = sourceName,
                target_concept_id = conceptId,
                target_vocabulary_id = "",
                valid_start_date = as.Date("1970-01-01"),
                valid_end_date = as.Date("2099-12-31"),
                invalid_reason = NA_character_
            )
    } else {
        # if colums `ADD_INFO:sourceValidStartDate` and `ADD_INFO:sourceValidEndDate` are not present, set them to the default values
        if (!"ADD_INFO:sourceValidStartDate" %in% usagiTibbleColumns) {
            usagiTibble <- usagiTibble |> dplyr::mutate(`ADD_INFO:sourceValidStartDate` = lubridate::as_date(ymd("1970-01-01")))
        }
        if (!"ADD_INFO:sourceValidEndDate" %in% usagiTibbleColumns) {
            usagiTibble <- usagiTibble |> dplyr::mutate(`ADD_INFO:sourceValidEndDate` = lubridate::as_date(ymd("2099-12-31")))
        }

        # if colums `ADD_INFO:sourceParents` and `ADD_INFO:sourceParentVocabulary` are present and any of the `ADD_INFO:sourceParents` is not NA
        if (all(c("ADD_INFO:sourceParents", "ADD_INFO:sourceParentVocabulary") %in% usagiTibbleColumns) &&
            any(!is.na(dplyr::pull(usagiTibble, `ADD_INFO:sourceParents`)))) {
            validVocabularyConceptCodes <- usagiTibble |>
                dplyr::transmute(vocabulary_id = "", concept_code = sourceCode, concept_id = `ADD_INFO:sourceConceptId`) |>
                dplyr::distinct()

            usedParentVocabularies <- usagiTibble |>
                dplyr::select(`ADD_INFO:sourceParentVocabulary`) |>
                dplyr::mutate(`ADD_INFO:sourceParentVocabulary` = dplyr::if_else(is.na(`ADD_INFO:sourceParentVocabulary`), "", `ADD_INFO:sourceParentVocabulary`)) |>
                dplyr::distinct() |>
                dplyr::pull(`ADD_INFO:sourceParentVocabulary`) |>
                stringr::str_split("\\|") |>
                purrr::flatten_chr() |>
                unique()                                                                                
            if (length(usedParentVocabularies) > 0) {
                parentVocabularyConceptCodes <- dplyr::tbl(connection, "CONCEPT") |>
                    dplyr::filter(vocabulary_id %in% usedParentVocabularies) |>
                    dplyr::select(vocabulary_id, concept_code, concept_id) |>
                    dplyr::collect()

                validVocabularyConceptCodes <- dplyr::bind_rows(validVocabularyConceptCodes, parentVocabularyConceptCodes)
            }

            parentConceptCodes <- usagiTibble |>
                dplyr::select(`ADD_INFO:sourceParents`, `ADD_INFO:sourceParentVocabulary`) |>
                dplyr::mutate(row = dplyr::row_number()) |>
                dplyr::mutate(
                    vocabulary_id = purrr::map2(`ADD_INFO:sourceParents`, `ADD_INFO:sourceParentVocabulary`, ~ {
                        x <- .x |>
                            stringr::str_split("\\|") |>
                            purrr::flatten_chr()

                        y <- .y |>
                            stringr::str_split("\\|") |>
                            purrr::flatten_chr()

                        if (length(x) != length(y)) {
                            stop("length of parent concept codes and parent vocabularies must be the same")
                        }

                        tibble::tibble(vocabulary_id = y, concept_code = x)
                    })
                ) |>
                tidyr::unnest(cols = c(vocabulary_id)) |>
                dplyr::select(-`ADD_INFO:sourceParentVocabulary`, -`ADD_INFO:sourceParents`)

            notValidParentConceptCodes <- parentConceptCodes |>
                dplyr::left_join(validVocabularyConceptCodes, by = c("vocabulary_id", "concept_code")) |>
                dplyr::filter(!is.na(concept_id)) |>
                dplyr::group_by(row) |>
                dplyr::summarise(sourceParentConceptIds = paste0(concept_id, collapse = "|")) |>
                dplyr::ungroup()

            usagiTibble <- usagiTibble |>
                dplyr::mutate(row = dplyr::row_number()) |>
                dplyr::left_join(notValidParentConceptCodes, by = "row") |>
                dplyr::select(-row)
        } else {
            usagiTibble <- usagiTibble |> dplyr::mutate(sourceParentConceptIds = NA_character_)
        }

        STCMTableToInsert <- usagiTibble |>
            dplyr::filter(mappingStatus %in% includeMappingStatus) |>
            dplyr::transmute(
                source_code = sourceCode,
                source_concept_id = `ADD_INFO:sourceConceptId`,
                source_vocabulary_id = {{ vocabularyId }},
                source_code_description = sourceName,
                target_concept_id = conceptId,
                target_vocabulary_id = "",
                valid_start_date = `ADD_INFO:sourceValidStartDate`,
                valid_end_date = `ADD_INFO:sourceValidEndDate`,
                invalid_reason = NA_character_,
                source_concept_class = `ADD_INFO:sourceConceptClass`,
                source_domain = `ADD_INFO:sourceDomain`,
                source_parents_concept_ids = sourceParentConceptIds
            )
    }

    # delete rows if vocabulary id exists
    DatabaseConnector::renderTranslateExecuteSql(
        connection,
        "DELETE FROM @vocabulary_database_schema.@source_to_concept_map_table WHERE source_vocabulary_id = '@vocabulary_id'",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        source_to_concept_map_table = sourceToConceptMapTable,
        vocabulary_id = vocabularyId
    )

    # insert the STCMTableToInsert into the sourceToConceptMapTable
    DatabaseConnector::dbWriteTable(connection, sourceToConceptMapTable, STCMTableToInsert, append = TRUE)
}
