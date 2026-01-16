#' Validate Usagi File
#'
#' Reads a usagi file given in 'pathToUsagiFile' and performs the following checks:
#' Default Usagi columns:
#' - Check if all default Usagi columns are present:
#' - Check if sourceCode and conceptId are unique
#' - Check if sourceCode is not empty
#' - Check if sourceCode is less than 50 characters
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
#' @param sourceConceptIdOffset Integer offset to add to source concept IDs. Default is 0
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
    sourceConceptIdOffset,
    pathToValidUnitsFile = NULL,
    pathToUnitConversionFile = NULL,
    pathToValidatedUnitConversionFile = NULL) {
    #
    # Parameter validation
    #
    checkmate::assertFileExists(pathToUsagiFile)
    # vocabularyDatabaseSchema exists in the connection
    checkmate::assertCharacter(vocabularyDatabaseSchema, len = 1, any.missing = FALSE)

    # Check if required tables exist
    # tables <- DatabaseConnector::getTableNames(condlnection, vocabularyDatabaseSchema)
    # TEMP untill solved https://github.com/OHDSI/DatabaseConnector/issues/299
    tableNames <- DatabaseConnector::getTableNames(connection, vocabularyDatabaseSchema)
    c("concept", "concept_relationship", "domain") |>
        checkmate::assertSubset(tableNames)

    # Read the usagi file
    usagiTibble <- readUsagiFile(pathToUsagiFile)
    usagiTibbleOriginal <- usagiTibble
    usagiTibbleColumns <- usagiTibble |> names()

    # Add tmpvalidationMessages message column
    usagiTibble <- usagiTibble |>
        dplyr::mutate(tmpvalidationMessages = "")

    #
    # Checks
    #

    validationLogR6 <- LogTibble$new()

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
        validationLogR6$ERROR(
            "Missing default columns",
            paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
        )
        return(validationLogR6$logTibble)
    } else {
        validationLogR6$SUCCESS("Missing default columns", "")
    }

    validationRules <- validate::validator(
        SourceCode.is.empty = is_complete(sourceCode),
        SourceCode.and.conceptId.are.not.unique = is_unique(sourceCode, conceptId),
        SourceCode.is.more.than.50.characters = field_length(sourceCode, min = 0, max = 50),
        SourceName.is.empty = is_complete(sourceName),
        SourceName.is.more.than.255.characters = field_length(sourceName, min = 0, max = 255),
        SourceFrequency.is.not.empty = is_complete(sourceFrequency),
        MappingStatus.is.empty = is_complete(mappingStatus),
        MappingStatus.is.not.valid = mappingStatus %in% c("APPROVED", "UNCHECKED", "FLAGGED", "INVALID_TARGET"),
        APPROVED.mappingStatus.conceptId.is.0 = if (mappingStatus == "APPROVED") conceptId != 0
    )
    validations <- validate::confront(usagiTibble, validationRules)

    result <- .applyValidationRules(usagiTibble, validations, validationLogR6)
    usagiTibble <- result$fileTibble
    validationLogR6 <- result$validationLogR6

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
        dplyr::filter(mappingStatus != "INVALID_TARGET") |>
        dplyr::distinct(conceptId, domainId, conceptName) |>
        dplyr::left_join(mappedConcepts, by = c("conceptId")) |>
        dplyr::mutate(
            errorMessage = dplyr::case_when(
                is.na(domainId.y) | is.na(conceptName.y) ~ paste0("OUTDATED conceptId: conceptId ", conceptId, " does not exist on the target vocabularies"),
                domainId.x != domainId.y ~ paste0("OUTDATED domainId: domainId for conceptId ", conceptId, " is different in the target vocabularies"),
                conceptName.x != conceptName.y ~ paste0("OUTDATED conceptName: conceptName for conceptId ", conceptId, " is different in the target vocabularies"),
                is.na(standardConcept) ~ paste0("OUTDATED standardConcept: standardConcept for conceptId ", conceptId, " has changed to non-standard"),
                TRUE ~ ""
            )
        ) |>
        dplyr::filter(errorMessage != "") |>
        dplyr::distinct(conceptId, errorMessage)

    if (nrow(outdatedConcepts) > 0) {
        usagiTibble <- usagiTibble |>
            dplyr::left_join(outdatedConcepts, by = c("conceptId")) |>
            dplyr::mutate(errorMessage = dplyr::case_when(
                !is.na(errorMessage) & mappingStatus %in% c("FLAGGED", "APPROVED") ~ paste0("ERROR ", errorMessage),
                !is.na(errorMessage) & mappingStatus %in% c("UNCHECKED") ~ paste0("WARNING ", errorMessage),
                TRUE ~ errorMessage
            )) |>
            dplyr::group_by(sourceCode) |>
            dplyr::mutate(errorMessage = paste(na.omit(errorMessage), collapse = ", ")) |>
            dplyr::ungroup() |>
            dplyr::mutate(tmpvalidationMessages = dplyr::if_else(errorMessage != "", paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
            dplyr::select(-errorMessage)
    }

    # conceptIds not in vocabularies
    totalOutdatedErrors <- usagiTibble |>
        dplyr::mutate(
            error = stringr::str_extract(tmpvalidationMessages, "\\w+\\s+OUTDATED\\s+\\w+:"),
            errorLevel = stringr::str_extract(error, "\\w+"),
            errorType = stringr::str_extract(error, "\\w+(?=:)")
        ) |>
        dplyr::filter(!is.na(error)) |>
        dplyr::count(errorLevel, errorType) |>
        dplyr::mutate(errorMessage = dplyr::case_when(
            errorType == "conceptId" ~ paste0(n, " conceptIds do not exist on the target vocabularies"),
            errorType == "domainId" ~ paste0(n, " domainIds are outdated"),
            errorType == "conceptName" ~ paste0(n, " conceptNames are outdated"),
            errorType == "standardConcept" ~ paste0(n, " standardConcepts have changed to non-standard"),
            TRUE ~ ""
        )) |>
        dplyr::group_by(errorLevel) |>
        dplyr::summarise(message = paste(errorMessage, collapse = ", "))

    if (totalOutdatedErrors |> dplyr::filter(errorLevel == "ERROR") |> nrow() > 0) {
        validationLogR6$ERROR(
            "APPROVED mappingStatus with concepts outdated",
            totalOutdatedErrors |> dplyr::filter(errorLevel == "ERROR") |> dplyr::pull(message)
        )
    } else {
        validationLogR6$SUCCESS("APPROVED mappingStatus with concepts outdated", "")
    }

    if (totalOutdatedErrors |> dplyr::filter(errorLevel == "WARNING") |> nrow() > 0) {
        validationLogR6$WARNING(
            "Not APPROVED mappingStatus with concepts outdated",
            totalOutdatedErrors |> dplyr::filter(errorLevel == "WARNING") |> dplyr::pull(message)
        )
    } else {
        validationLogR6$SUCCESS("Not APPROVED mappingStatus with concepts outdated", "")
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
            validationLogR6$ERROR(
                "Missing C&CR columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogR6$logTibble)
        } else {
            validationLogR6$SUCCESS("Missing C&CR columns", "")
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

        result <- .applyValidationRules(usagiTibble, validations, validationLogR6)
        usagiTibble <- result$fileTibble
        validationLogR6 <- result$validationLogR6

        # check if when the code maps to more than one concept the combined domain is valid
        invalidDomainCombinations <- usagiTibble |>
            dplyr::filter(mappingStatus != "INVALID_TARGET") |>
            dplyr::select(sourceCode, domainId, mappingStatus) |>
            dplyr::group_by(sourceCode, mappingStatus) |>
            dplyr::summarise(
                recalcualted_domainId = stringr::str_c(sort(unique(domainId)), collapse = " "),
                n_domains = dplyr::n(),
                .groups = "drop"
            ) |>
            dplyr::filter(n_domains > 1) |>
            # recalculate domains
            dplyr::mutate(recalcualted_domainId = dplyr::case_when(
                recalcualted_domainId == "Condition Device" ~ "Condition/Device",
                recalcualted_domainId == "Condition Drug" ~ "Condition/Drug",
                recalcualted_domainId == "Condition Measurement" ~ "Condition/Meas",
                recalcualted_domainId == "Condition Observation" ~ "Condition/Obs",
                recalcualted_domainId == "Condition Procedure" ~ "Condition/Procedure",
                recalcualted_domainId == "Device Drug" ~ "Device/Drug",
                recalcualted_domainId == "Device Observation" ~ "Device/Obs",
                recalcualted_domainId == "Device Procedure" ~ "Device/Procedure",
                recalcualted_domainId == "Drug Measurement" ~ "Drug/Measurement",
                recalcualted_domainId == "Drug Observation" ~ "Drug/Obs",
                recalcualted_domainId == "Drug Procedure" ~ "Drug/Procedure",
                recalcualted_domainId == "Measurement Observation" ~ "Measurement/Obs",
                recalcualted_domainId == "Measurement Procedure" ~ "Meas/Procedure",
                recalcualted_domainId == "Observation Procedure" ~ "Obs/Procedure",
                TRUE ~ recalcualted_domainId
            )) |>
            dplyr::filter(!(recalcualted_domainId %in% validDomains)) |>
            dplyr::mutate(
                errorMessage = paste0("this code is mapped to more than one domains that are not compatible: ", recalcualted_domainId),
                errorMessage = dplyr::if_else(mappingStatus == "UNCHECKED", paste0("WARNING: ", errorMessage), paste0("ERROR: ", errorMessage))
            ) |>
            dplyr::select(sourceCode, errorMessage)


        if (nrow(invalidDomainCombinations) > 0) {
            usagiTibble <- usagiTibble |>
                dplyr::left_join(invalidDomainCombinations, by = c("sourceCode")) |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-errorMessage)
        }

        n <- invalidDomainCombinations |>
            dplyr::filter(stringr::str_detect(errorMessage, "WARNING")) |>
            nrow()
        if (n > 0) {
            validationLogR6$WARNING(
                "Not APPROVED mappingStatus with valid domain combination",
                paste0("Found ", n, " codes with invalid domain combinations")
            )
        } else {
            validationLogR6$SUCCESS("Not APPROVED mappingStatus with valid domain combination", "")
        }
        n <- invalidDomainCombinations |>
            dplyr::filter(stringr::str_detect(errorMessage, "ERROR")) |>
            nrow()
        if (n > 0) {
            validationLogR6$ERROR(
                "APPROVED mappingStatus with valid domain combination",
                paste0("Found ", n, " codes with invalid domain combinations")
            )
        } else {
            validationLogR6$SUCCESS("APPROVED mappingStatus with valid domain combination", "")
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
            validationLogR6$ERROR(
                "Missing date columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogR6)
        } else {
            validationLogR6$SUCCESS("Missing date columns", "")
        }

        validationRules <- validate::validator(
            SourceValidStartDate.is.after.SourceValidEndDate = `ADD_INFO:sourceValidStartDate` <= `ADD_INFO:sourceValidEndDate`
        )
        validations <- validate::confront(usagiTibble, validationRules)

        result <- .applyValidationRules(usagiTibble, validations, validationLogR6)
        usagiTibble <- result$fileTibble
        validationLogR6 <- result$validationLogR6
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
            validationLogR6$ERROR(
                "Missing parent columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogR6)
        } else {
            validationLogR6$SUCCESS("Missing parent columns", "")
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

                    if (!any(is.na(y)) && length(x) != length(y)) {
                        stop("length of parent concept codes and parent vocabularies must be the same")
                    }

                    tibble::tibble(vocabulary_id = y, concept_code = x)
                })
            ) |>
            tidyr::unnest(cols = c(vocabulary_id)) |>
            dplyr::select(-`ADD_INFO:sourceParentVocabulary`, -`ADD_INFO:sourceParents`) |>
            dplyr::mutate(concept_code = dplyr::if_else(concept_code == "", NA_character_, concept_code))

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
            validationLogR6$ERROR(
                "Invalid parent concept code",
                paste0("Found ", nrow(notValidParentConceptCodes), " codes with invalid parent concept codes")
            )

            usagiTibble <- usagiTibble |>
                dplyr::mutate(row = dplyr::row_number()) |>
                dplyr::left_join(notValidParentConceptCodes, by = "row") |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-row, -errorMessage)
        } else {
            validationLogR6$SUCCESS("Invalid parent concept code", "")
        }
    }


    # Checks for Lab Value Usagi file
    if (!is.null(pathToValidUnitsFile) && !is.null(pathToUnitConversionFile)) {
        # Prepare files for lab values checks
        # - validUnitsList:
        validUnitsTibble <- readUsagiFile(pathToValidUnitsFile) 

        if ("ADD_INFO:UniqueForLab" %in% names(validUnitsTibble)) {
            validUnitsTibble <- validUnitsTibble |>
                dplyr::filter(`ADD_INFO:UniqueForLab` == TRUE)
        }

        validUnitsList <- validUnitsTibble |>
            dplyr::distinct(sourceCode) |>
            dplyr::pull(sourceCode)

        # - usagiTibble:
        usagiTibble <- usagiTibble |>
            dplyr::mutate(
                testName = stringr::str_extract(sourceCode, "^.*(?=\\[)"),
                testUnit = stringr::str_extract(sourceCode, "(?<=\\[)[^\\]]+(?=\\])")
            )
        conceptIds <- usagiTibble |>
            dplyr::pull(conceptId) |>
            unique()
        conceptIdToQuantity <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP") |>
            dplyr::filter(concept_id_1 %in% conceptIds) |>
            dplyr::filter(relationship_id == "Has property") |>
            dplyr::select(concept_id_1, concept_id_2) |>
            left_join(
                dplyr::tbl(connection, "CONCEPT") |>
                    dplyr::select(concept_id, concept_name),
                by = c("concept_id_2" = "concept_id")
            ) |>
            dplyr::transmute(
                conceptId = concept_id_1,
                omop_quantity = dplyr::if_else(!is.na(concept_name), concept_name, "-")
            ) |>
            dplyr::collect()

        usagiTibble <- usagiTibble |>
            dplyr::left_join(conceptIdToQuantity, by = "conceptId")
        
        # - unitConversionTibble:
        unitConversionTibble <- readUnitConversionFile(pathToUnitConversionFile)
        result <- validateUnitConversionTibble(unitConversionTibble, validUnitsList, validQuantitiesList)
        unitConversionTibble <- result$unitConversionTibble
        unitConversionValidationLogR6 <- result$validationLogR6
        if (unitConversionValidationLogR6$logTibble |> dplyr::filter(type == "ERROR") |> nrow() > 0) {
            unitConversionTibble |> readr::write_tsv(pathToValidatedUnitConversionFile, na = "")
            return(unitConversionValidationLogR6$logTibble)
        }

        # Check that all the sourceCodes in usagiTibble are of the format {testName}[{testUnit}]
        invalidLabSourceCodes <- usagiTibble |>
            dplyr::distinct(sourceCode) |>
            dplyr::filter(!stringr::str_detect(sourceCode, "^.+\\[.*\\]$")) |>
            dplyr::mutate(errorMessage = paste0("ERROR: ", sourceCode, " is not a valid lab source code, it must be of the format testName[testUnit]"))|>
            dplyr::select(sourceCode, errorMessage)
        if (nrow(invalidLabSourceCodes) > 0) {
            validationLogR6$ERROR("LAB: Invalid lab source code format", paste0("Found ", nrow(invalidLabSourceCodes), " codes where source code is not of the format testName[testUnit]"))

            usagiTibble <- usagiTibble |>
                dplyr::left_join(invalidLabSourceCodes, by = "sourceCode") |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-errorMessage)
        } else {
            validationLogR6$SUCCESS("LAB: Invalid lab source code format", "")
        }
browser()
        # check if usagiTibble testUnit is in validUnitsList or na
        invalidLabUnits <- usagiTibble |>
            dplyr::distinct(sourceCode, testUnit, mappingStatus) |>
            dplyr::filter(!(is.na(testUnit) | (testUnit %in% validUnitsList))) |>
            dplyr::mutate(
                errorType = dplyr::if_else(mappingStatus == "UNCHECKED", "WARNING: not APPROVED", "ERROR: APPROVED"),
                errorMessage = paste0(errorType, ": ", sourceCode, " is not a valid unit: ", testUnit)
            )|>
            dplyr::select(sourceCode, errorType, errorMessage)
        if (nrow(invalidLabUnits ) > 0) {
            usagiTibble <- usagiTibble |>
                dplyr::left_join(invalidLabUnits, by = "sourceCode") |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-errorMessage, -errorType)
        } 

        nErrors <- invalidLabUnits |> dplyr::filter(errorType == "ERROR: APPROVED") |> nrow()
        if (nErrors > 0) {
            validationLogR6$ERROR("LAB: APPROVED Invalid lab unit", paste0("Found ", nErrors, " APPROVED lab source codes where unit is not in validUnitsList or is NA"))
        } else {
            validationLogR6$SUCCESS("LAB: APPROVED Invalid lab unit", "")
        }

        nWarnings <- invalidLabUnits |> dplyr::filter(errorType == "WARNING: not APPROVED") |> nrow()
        if (nWarnings > 0) {
            validationLogR6$WARNING("LAB: not APPROVED Invalid lab unit", paste0("Found ", nWarnings, " not APPROVED lab source codes where unit is not in validUnitsList or is NA"))
        } else {
            validationLogR6$SUCCESS("LAB: not APPROVED Invalid lab unit", "")
        }

        # check if domain of the mapped concept is 'Measurement'
        invalidLabDomains <- usagiTibble |>
            dplyr::distinct(sourceCode, conceptId, domainId, conceptName) |>
            dplyr::filter(domainId != "Measurement") |>
            dplyr::mutate(errorMessage = paste0("WARNING: ", sourceCode, " is mapped to incorrect domain: ",domainId, " for concept ", conceptId, " (", conceptName, ")"))|>
            dplyr::select(sourceCode, conceptId, errorMessage)
        if (nrow(invalidLabDomains) > 0) {
            validationLogR6$WARNING("LAB: Invalid lab mapped domain", paste0("Found ", nrow(invalidLabDomains), " mapped lab source codes where domain is not 'Measurement'"))

            usagiTibble <- usagiTibble |>
                dplyr::left_join(invalidLabDomains, by = c("sourceCode", "conceptId")) |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-errorMessage)
        } else {
            validationLogR6$SUCCESS("LAB: Invalid lab mapped domain", "")
        }

        # check if the quantity of the mapped concept is in the unitConversionTibble
        invalidLabQuantities <- usagiTibble |>
            dplyr::distinct(sourceCode, conceptId, conceptName, omop_quantity, testUnit, mappingStatus) |>
            dplyr::filter(!is.na(omop_quantity) & !is.na(testUnit)) |>
            dplyr::left_join(unitConversionTibble, by = c("omop_quantity", "testUnit"="source_unit_valid")) |>
            dplyr::filter(is.na(conversion)) |>
            dplyr::mutate(
                errorType = dplyr::if_else(mappingStatus == "UNCHECKED", "WARNING: not APPROVED", "ERROR: APPROVED"),
                errorMessage = paste0(errorType, ": test unit [", testUnit, "] does not agree with omop_quantity '", omop_quantity, "' for mapped concept ", conceptId, " (", conceptName, ")")
            )|>
            dplyr::select(sourceCode, conceptId, errorType, errorMessage)
        if (nrow(invalidLabQuantities) > 0) {
             usagiTibble <- usagiTibble |>
                dplyr::left_join(invalidLabQuantities, by = c("sourceCode", "conceptId")) |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-errorMessage, -errorType)
        } 

        nErrors <- invalidLabQuantities |> dplyr::filter(errorType == "ERROR: APPROVED") |> nrow()
        if (nErrors > 0) {
            validationLogR6$ERROR("LAB: APPROVED Invalid lab quantity", paste0("Found ", nErrors, " APPROVED lab source codes where test unit does not agree with omop_quantity"))
        } else {
            validationLogR6$SUCCESS("LAB: APPROVED Invalid lab quantity", "")
        }

        nWarnings <- invalidLabQuantities |> dplyr::filter(errorType == "WARNING: not APPROVED") |> nrow()
        if (nWarnings > 0) {
            validationLogR6$WARNING("LAB: not APPROVED Invalid lab quantity", paste0("Found ", nWarnings, " not APPROVED lab source codes where test unit does not agree with omop_quantity"))
        } else {
            validationLogR6$SUCCESS("LAB: not APPROVED Invalid lab quantity", "")
        }

        # check if the same testName with same quantity maps to different concept ids
        warningTestNameQuantityCombinations <- usagiTibble |>
            dplyr::filter(mappingStatus == "APPROVED") |>
            dplyr::distinct(testName, omop_quantity, conceptId, conceptName) |>
            dplyr::group_by(testName, omop_quantity) |>
            dplyr::summarise(
                conceptIds_list = list(conceptId), message = paste(paste0(conceptId, " (", conceptName, ")"), collapse = " | "), n = dplyr::n()) |>
            dplyr::ungroup() |> 
            dplyr::filter(n > 1) |> 
            dplyr::mutate(errorMessage = paste0("WARNING: testName '", testName, "' with omop_quantity '", omop_quantity, "' maps to different concept ids: ", message)) |>
            tidyr::unnest(conceptIds_list) |> dplyr::rename(conceptId = conceptIds_list) |>
            dplyr::select(testName, omop_quantity, conceptId, errorMessage)
        if (nrow(warningTestNameQuantityCombinations) > 0) {
            validationLogR6$WARNING("LAB: TestName with same quantity maps to different concept ids", paste0("Found ", nrow(warningTestNameQuantityCombinations), " codes with testName with same quantity maps to different concept ids"))
            usagiTibble <- usagiTibble |>
                dplyr::left_join(warningTestNameQuantityCombinations, by = c("testName", "omop_quantity", "conceptId")) |>
                dplyr::mutate(tmpvalidationMessages = dplyr::if_else(!is.na(errorMessage), paste0(tmpvalidationMessages, " | ", errorMessage), tmpvalidationMessages)) |>
                dplyr::select(-errorMessage)
        } else {
            validationLogR6$SUCCESS("LAB: TestName with same quantity maps to different concept ids", "")
        }

        # Clean up
        usagiTibble <- usagiTibble |>
            dplyr::select(-testName, -testUnit, -omop_quantity)
    }

    #
    # end
    #
    usagiTibble <- usagiTibble |>
        dplyr::mutate(
            `ADD_INFO:validationMessages` = stringr::str_replace(tmpvalidationMessages, "^\\s*\\|\\s*", ""),
            # Change to FLAGGED if there is an error not due to outdated concepts, this means there is string with 'ERROR:'
            mappingStatus = dplyr::if_else(
                stringr::str_detect(tmpvalidationMessages, "ERROR:"),
                "FLAGGED",
                mappingStatus
            )
        ) |>
        dplyr::select(-tmpvalidationMessages)

    # Kill switch, if other than `mappingStatus` and `ADD_INFO:validationMessages` are different then error
    byNames <- usagiTibble |>
        names() |>
        setdiff(c("mappingStatus", "ADD_INFO:validationMessages"))
    differences <- usagiTibbleOriginal |>
        dplyr::select(dplyr::all_of(byNames)) |>
        dplyr::anti_join(usagiTibble |> dplyr::select(dplyr::all_of(byNames)), by = byNames)
    if (differences |> nrow() > 0) {
        validationLogR6$ERROR("Validation failed", "Validation failed")
        return(validationLogR6$logTibble)
    }

    usagiTibble |>
        writeUsagiFile(pathToValidatedUsagiFile)

    return(validationLogR6$logTibble)
}



.applyValidationRules <- function(fileTibble = NULL, validations, validationLogR6) {
    validationSummary <- validate::summary(validations) |> tibble::as_tibble()
    if ("name" %in% names(validationSummary)) {
        validationSummary <- validationSummary |>
            dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " "))
    }

    for (i in 1:nrow(validationSummary)) {
        row <- validationSummary[i, ]
        if (row$error) {
            validationLogR6$FATAL(
                row$name,
                "Validation failed"
            )
        } else if (row$fails > 0) {
            validationLogR6$ERROR(
                row$name,
                paste("Number of failed rules: ", row$fails)
            )
        } else {
            validationLogR6$SUCCESS(
                row$name,
                ""
            )
        }
    }

    # get failed rules with row numbers
    failedRulesRows <- validate::values(validations) |> tibble::as_tibble()
    if (nrow(failedRulesRows) > 0 && !is.null(fileTibble)) {
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
                dplyr::select(-row, -errorMessage) |> 
                dplyr::distinct()
        }
    }

    return(list(fileTibble = fileTibble, validationLogR6 = validationLogR6))
}
