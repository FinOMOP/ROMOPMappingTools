#' Validate Usagi File
#'
#' Reads a usagi file given in 'pathToUsagiFile' and performs the following checks:
#' Default Usagi columns:
#' - Check if all default Usagi columns are present:
#' - Check if sourceCode and conceptId are unique
#' - Check if sourceCode is not empty
#' - Check if sourceName is not empty
#' - Check if sourceName is less than 255 characters
#' If usagi file has C&CR columns:
#' - Check if concept_id is not 0 for APPROVED mappingStatus
#' - Check codes with mapping to more than one domain are mapped to compatible domains
#' - Check if sourceValidStartDate is before sourceValidEndDate
#' - Check if ADD_INFO:sourceParents is a valid concept code in the ADD_INFO:sourceParentVocabulary
#'
#' @param pathToUsagiFile Path to the Usagi mapping file to validate
#' @param connection A DatabaseConnector connection object
#' @param vocabularyDatabaseSchema Schema name where the vocabulary tables are stored
#' @param pathToValidatedUsagiFile Path where to save the validated Usagi file
#'
#' @importFrom checkmate assertFileExists assertSubset
#' @importFrom DBI dbConnect dbListTables
#' @importFrom dplyr mutate
#' @importFrom readr write_csv
#'
#' @export
validateUsagiFile <- function(
    pathToUsagiFile,
    connection,
    vocabularyDatabaseSchema,
    pathToValidatedUsagiFile,
    sourceConceptIdOffset) {
    #
    # Parameter validation
    #
    checkmate::assertFileExists(pathToUsagiFile)
    # vocabularyDatabaseSchema exists in the connection
    checkmate::assertCharacter(vocabularyDatabaseSchema, len = 1, any.missing = FALSE)

    # Check if required tables exist
    # tables <- DatabaseConnector::getTableNames(condlnection, vocabularyDatabaseSchema)
    # TEMP untill solved https://github.com/OHDSI/DatabaseConnector/issues/299
    tableNames <- DatabaseConnector::dbListTables(connection, vocabularyDatabaseSchema)
    c("concept", "concept_relationship", "domain") |>
        checkmate::assertSubset(tableNames)

    # Read the usagi file
    usagiTibble <- readUsagiFile(pathToUsagiFile)
    usagiTibbleColumns <- usagiTibble |> names()

    # Add tmpvalidationMessages message column
    usagiTibble <- usagiTibble |>
        dplyr::mutate(tmpvalidationMessages = "")

    #
    # Checks
    #

    validationLogTibble <- LogTibble$new()

    # - Check default usagi columns
    missingColumns <-
        c(
            "sourceCode",
            "sourceName",
            "sourceFrequency",
            "sourceAutoAssignedConceptIds",
            "matchScore",
            "mappingStatus",
            "equivalence",
            "statusSetBy",
            "statusSetOn",
            "conceptId",
            "conceptName",
            "domainId",
            "mappingType",
            "comment",
            "createdBy",
            "createdOn",
            "assignedReviewer"
        ) |>
        setdiff(usagiTibbleColumns)

    if (length(missingColumns) > 0) {
        validationLogTibble$ERROR(
            "Missing default columns",
            paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
        )
        return(validationLogTibble$logTibble)
    } else {
        validationLogTibble$SUCCESS("Missing default columns", "")
    }

    validationRules <- validate::validator(
        SourceCode.is.empty = is_complete(sourceCode),
        SourceCode.and.conceptId.are.not.unique = is_unique(sourceCode, conceptId),
        SourceName.is.empty = is_complete(sourceName),
        SourceName.is.more.than.255.characters = field_length(sourceName, min = 0, max = 255),
        SourceFrequency.is.not.empty = is_complete(sourceFrequency),
        MappingStatus.is.empty = is_complete(mappingStatus),
        MappingStatus.is.one.of.the.following = mappingStatus %in% c("APPROVED", "UNCHECKED", "FLAGGED", "INEXACT", "INVALID_TARGET"),
        Concept_id.is.0.for.APPROVED.mappingStatus = if (mappingStatus == "APPROVED") conceptId != 0
    )
    validations <- validate::confront(usagiTibble, validationRules)

    result <- .applyValidationRules(usagiTibble, validations, validationLogTibble)
    usagiTibble <- result$fileTibble
    validationLogTibble <- result$validationLogTibble

    # - check if conceptIds exists in the database
    # - check if the mappings are up to date
    # domain, name, concept class or standard is different than in the concept table of the database

    mappedConceptIds <- usagiTibble |>
        dplyr::filter(conceptId != 0) |>
        dplyr::distinct(conceptId) |>
        dplyr::pull(conceptId)

    mappedConcepts <- dplyr::tbl(connection, "CONCEPT") |>
        dplyr::filter(concept_id %in% mappedConceptIds) |>
        dplyr::select(concept_id, domain_id, concept_name, standard_concept) |>
        dplyr::collect() |>
        SqlRender::snakeCaseToCamelCaseNames()

    outdatedConcepts <- usagiTibble |>
        dplyr::filter(conceptId != 0) |>
        dplyr::distinct(conceptId, domainId, conceptName) |>
        dplyr::left_join(mappedConcepts, by = c("conceptId")) |>
        dplyr::mutate(
            errorMessage = dplyr::case_when(
                is.na(domainId.y) | is.na(conceptName.y) ~ paste0("ERROR: conceptId ", conceptId, " does not exist on the target vocabularies"),
                domainId.x != domainId.y ~ paste0("ERROR: OUTDATED: domainId for conceptId ", conceptId, " is different in the target vocabularies"),
                conceptName.x != conceptName.y ~ paste0("ERROR: OUTDATED: conceptName for conceptId ", conceptId, " is different in the target vocabularies"),
                is.na(standardConcept) ~ paste0("ERROR: OUTDATED: standard_concept for conceptId ", conceptId, " has changed to non-standard"),
                TRUE ~ ""
            )
        ) |>
        dplyr::filter(errorMessage != "") |> 
        dplyr::distinct(conceptId, errorMessage)

    if (nrow(outdatedConcepts) > 0) {
        n <- outdatedConcepts |> dplyr::filter(stringr::str_detect(errorMessage, "does not exist on the target vocabularies")) |> nrow()
        if (n > 0) {
            validationLogTibble$ERROR(
                "ConceptIds not in vocabularies",
                paste0("Found ", n, " conceptIds that do not exist on the target vocabularies")
            )
        } else {
            validationLogTibble$SUCCESS("ConceptIds not in vocabularies", "")
        }
        
        n <- outdatedConcepts |> dplyr::filter(!stringr::str_detect(errorMessage, "does not exist on the target vocabularies")) |> nrow()
        if (n > 0) {
            validationLogTibble$ERROR(
                "ConceptIds outdated",
                paste0("Found ", n, " conceptIds that are outdated. Use ROMOPMappingTool::updateUsagiFile() to update the usagi file")
            )
        } else {
            validationLogTibble$SUCCESS("ConceptIds outdated", "")
        }
    
        usagiTibble <- usagiTibble |>
            dplyr::left_join(outdatedConcepts, by = c("conceptId")) |>
            dplyr::group_by(sourceCode) |>
            dplyr::mutate(errorMessage = paste(na.omit(errorMessage), collapse = ", ")) |>
            dplyr::ungroup() |>
            dplyr::mutate(tmpvalidationMessages = dplyr::if_else(errorMessage != "", paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
            dplyr::select(-errorMessage)

    }


    # if it has the ADD_INFO:sourceConceptId and ADD_INFO:sourceConceptClass and ADD_INFO:sourceDomain will be used as C&CR
    # if it has any of the 3 columns it must have the other 2
    if ("ADD_INFO:sourceConceptId" %in% usagiTibbleColumns || "ADD_INFO:sourceConceptClass" %in% usagiTibbleColumns || "ADD_INFO:sourceDomain" %in% usagiTibbleColumns ||
        "ADD_INFO:sourceValidStartDate" %in% usagiTibbleColumns || "ADD_INFO:sourceValidEndDate" %in% usagiTibbleColumns ||
        "ADD_INFO:sourceParents" %in% usagiTibbleColumns || "ADD_INFO:sourceParentVocabulary" %in% usagiTibbleColumns) {
        missingColumns <-
            c(
                "ADD_INFO:sourceConceptId",
                "ADD_INFO:sourceConceptClass",
                "ADD_INFO:sourceDomain"
            ) |>
            setdiff(usagiTibbleColumns)

        if (length(missingColumns) > 0) {
            validationLogTibble$ERROR(
                "Missing C&CR columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogTibble$logTibble)
        } else {
            validationLogTibble$SUCCESS("Missing C&CR columns", "")
        }

        validDomains <- connection |>
            DBI::dbGetQuery("SELECT domain_id FROM DOMAIN") |>
            dplyr::pull(domain_id)
        validationRules <- validate::validator(
            SourceConceptId.is.empty = is_complete(`ADD_INFO:sourceConceptId`),
            SourceConceptId.is.not.a.number.on.the.range = `ADD_INFO:sourceConceptId` > sourceConceptIdOffset & `ADD_INFO:sourceConceptId` < sourceConceptIdOffset + 100000,
            SourceConceptClass.is.empty = is_complete(`ADD_INFO:sourceConceptClass`),
            SourceConceptClass.is.more.than.20.characters = field_length(`ADD_INFO:sourceConceptClass`, min = 0, max = 20),
            SourceDomain.is.empty = is_complete(`ADD_INFO:sourceDomain`),
            SourceDomain.is.not.a.valid.domain = `ADD_INFO:sourceDomain` %in% validDomains
        )

        validations <- validate::confront(usagiTibble, validationRules, ref = list(validDomains = validDomains, sourceConceptIdOffset = sourceConceptIdOffset))

        result <- .applyValidationRules(usagiTibble, validations, validationLogTibble)
        usagiTibble <- result$fileTibble
        validationLogTibble <- result$validationLogTibble

        # check if when the code maps to more than one concept the combined domain is valid
        invalidDomainCombinations <- usagiTibble |>
            dplyr::select(sourceCode, domainId) |>
            dplyr::group_by(sourceCode) |>
            dplyr::summarise(
                recalcualted_domainId = stringr::str_c(sort(unique(domainId)), collapse = " "),
                n_domains = dplyr::n(),
                .groups = "drop"
            ) |>
            dplyr::filter(n_domains > 1) |>
            # recalculate domains
            dplyr::mutate(recalcualted_domainId = dplyr::case_when(
                recalcualted_domainId == "Condition Device" ~ "Condition/Device",
                recalcualted_domainId == "Condition Measurement" ~ "Condition/Meas",
                recalcualted_domainId == "Condition Observation" ~ "Condition/Obs",
                recalcualted_domainId == "Condition Procedure" ~ "Condition/Procedure",
                recalcualted_domainId == "Device Drug" ~ "Device/Drug",
                recalcualted_domainId == "Device Procedure" ~ "Device/Procedure",
                recalcualted_domainId == "Drug Procedure" ~ "Drug/Procedure",
                recalcualted_domainId == "Measurement Procedure" ~ "Meas/Procedure",
                recalcualted_domainId == "Observation Procedure" ~ "Obs/Procedure",
                TRUE ~ recalcualted_domainId
            )) |>
            dplyr::filter(!(recalcualted_domainId %in% validDomains)) |>
            dplyr::mutate(errorMessage = paste0("ERROR: this code is mapped to more than one domains that are not compatible: ", recalcualted_domainId)) |>
            dplyr::select(sourceCode, errorMessage)

        if (nrow(invalidDomainCombinations) > 0) {
            validationLogTibble$ERROR(
                "Invalid domain combination",
                paste0("Found ", nrow(invalidDomainCombinations), " codes with invalid domain combinations")
            )

            usagiTibble <- usagiTibble |>
                dplyr::left_join(invalidDomainCombinations, by = c("sourceCode")) |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-errorMessage)
        } else {
            validationLogTibble$SUCCESS("Invalid domain combination", "")
        }
    }

    # - Check if ADD_INFO:sourceValidStartDate and ADD_INFO:sourceValidEndDate are used
    if ("ADD_INFO:sourceValidStartDate" %in% usagiTibbleColumns || "ADD_INFO:sourceValidEndDate" %in% usagiTibbleColumns) {
        missingColumns <-
            c(
                "ADD_INFO:sourceValidStartDate",
                "ADD_INFO:sourceValidEndDate"
            ) |>
            setdiff(usagiTibbleColumns)

        if (length(missingColumns) > 0) {
            validationLogTibble$ERROR(
                "Missing date columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogTibble)
        } else {
            validationLogTibble$SUCCESS("Missing date columns", "")
        }

        validationRules <- validate::validator(
            SourceValidStartDate.is.after.SourceValidEndDate = `ADD_INFO:sourceValidStartDate` <= `ADD_INFO:sourceValidEndDate`
        )
        validations <- validate::confront(usagiTibble, validationRules)

        result <- .applyValidationRules(usagiTibble, validations, validationLogTibble)
        usagiTibble <- result$fileTibble
        validationLogTibble <- result$validationLogTibble
    }

    # Check ADD_INFO:sourceParents,ADD_INFO:sourceParentVocabulary
    if ("ADD_INFO:sourceParents" %in% usagiTibbleColumns || "ADD_INFO:sourceParentVocabulary" %in% usagiTibbleColumns) {
        missingColumns <-
            c(
                "ADD_INFO:sourceParents",
                "ADD_INFO:sourceParentVocabulary"
            ) |>
            setdiff(usagiTibbleColumns)

        if (length(missingColumns) > 0) {
            validationLogTibble$ERROR(
                "Missing parent columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogTibble)
        } else {
            validationLogTibble$SUCCESS("Missing parent columns", "")
        }

        # check if sourceParents is a valid conceptCode in the ParentVocabulary
        validVocabularyConceptCodes <- usagiTibble |>
            dplyr::transmute(vocabulary_id = NA_character_, concept_code = sourceCode)

        usedParentVocabularies <- usagiTibble |>
            dplyr::select(`ADD_INFO:sourceParentVocabulary`) |>
            dplyr::mutate(`ADD_INFO:sourceParentVocabulary` = dplyr::if_else(is.na(`ADD_INFO:sourceParentVocabulary`), "", `ADD_INFO:sourceParentVocabulary`)) |>
            dplyr::distinct() |>
            dplyr::pull(`ADD_INFO:sourceParentVocabulary`) |>
            stringr::str_split("\\|") |>
            purrr::flatten_chr() |>
            unique() |>
            setdiff("")
        if (length(usedParentVocabularies) > 0) {
            parentVocabularyConceptCodes <- dplyr::tbl(connection, "CONCEPT") |>
                dplyr::filter(vocabulary_id %in% usedParentVocabularies) |>
                dplyr::select(vocabulary_id, concept_code) |>
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
            dplyr::filter(!is.na(vocabulary_id) & !is.na(concept_code)) |>
            dplyr::mutate(vocabulary_id = dplyr::if_else(vocabulary_id == "", NA_character_, vocabulary_id)) |>
            dplyr::anti_join(validVocabularyConceptCodes, by = c("vocabulary_id", "concept_code")) |>
            dplyr::mutate(errorMessage = dplyr::if_else(is.na(vocabulary_id),
                paste0("ERROR: ", concept_code, " is not a valid concept in this vocabulary"),
                paste0("ERROR: ", concept_code, " is not a valid concept code in vocabulary ", vocabulary_id)
            )) |>
            dplyr::group_by(row) |>
            dplyr::summarise(errorMessage = paste(errorMessage, collapse = " | ")) |>
            dplyr::ungroup()

        if (nrow(notValidParentConceptCodes) > 0) {
            validationLogTibble$ERROR(
                "Invalid parent concept code",
                paste0("Found ", nrow(notValidParentConceptCodes), " codes with invalid parent concept codes")
            )

            usagiTibble <- usagiTibble |>
                dplyr::mutate(row = dplyr::row_number()) |>
                dplyr::left_join(notValidParentConceptCodes, by = "row") |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-row, -errorMessage)
        } else {
            validationLogTibble$SUCCESS("Invalid parent concept code", "")
        }
    }


    # end
    usagiTibble |>
        dplyr::mutate(
            tmpvalidationMessages = stringr::str_replace(tmpvalidationMessages, "^\\s*\\|\\s*", ""),
            mappingStatus = dplyr::case_when(
                tmpvalidationMessages != "" ~ "FLAGGED",
                TRUE ~ mappingStatus
            )
        ) |>
        dplyr::rename(`ADD_INFO:validationMessages` = tmpvalidationMessages) |>
        readr::write_csv(pathToValidatedUsagiFile, na = "")

    return(validationLogTibble$logTibble)
}



.applyValidationRules <- function(fileTibble, validations, validationLogTibble) {
    validationSummary <- validate::summary(validations) |> tibble::as_tibble()
    if ("name" %in% names(validationSummary)) {
        validationSummary <- validationSummary |>
            dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " "))
    }

    for (i in 1:nrow(validationSummary)) {
        row <- validationSummary[i, ]
        if (row$error) {
            validationLogTibble$FATAL(
                row$name,
                "Validation failed"
            )
        } else if (row$fails > 0) {
            validationLogTibble$ERROR(
                row$name,
                paste("Number of failed rules: ", row$fails)
            )
        } else {
            validationLogTibble$SUCCESS(
                row$name,
                ""
            )
        }
    }

    # get failed rules with row numbers
    failedRulesRows <- validate::values(validations) |> tibble::as_tibble()
    if (nrow(failedRulesRows) > 0) {
        failedRulesRows <- failedRulesRows |>
            dplyr::mutate_all(~ dplyr::if_else(!.x, dplyr::row_number(), as.integer(NA))) |>
            tidyr::pivot_longer(cols = tidyr::everything(), names_to = "name", values_to = "row", values_drop_na = TRUE) |>
            dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " ")) |>
            dplyr::mutate(errorMessage = paste0("ERROR: ", name)) |>
            dplyr::select(row, errorMessage) |>
            dplyr::group_by(row) |>
            dplyr::mutate(errorMessage = paste(errorMessage, collapse = " | ")) |>
            dplyr::ungroup()

        if (nrow(failedRulesRows) > 0) {
            fileTibble <- fileTibble |>
                dplyr::mutate(row = dplyr::row_number()) |>
                dplyr::left_join(failedRulesRows, by = "row") |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-row, -errorMessage)
        }
    }

    return(list(fileTibble = fileTibble, validationLogTibble = validationLogTibble))
}
