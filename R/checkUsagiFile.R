validateUsagiFile <- function(
    pathToUsagiFile,
    pathToOMOPVocabularyDuckDBfile,
    pathToEditedUsagiFile) {
    #
    # Parameter validation
    #
    checkmate::assertFileExists(pathToUsagiFile)
    checkmate::assertFileExists(pathToOMOPVocabularyDuckDBfile)

    con <- DBI::dbConnect(duckdb::duckdb(), pathToOMOPVocabularyDuckDBfile)
    listTables <- DBI::dbListTables(con)
    c("CONCEPT", "CONCEPT_RELATIONSHIP", "DOMAIN") |>
        checkmate::assertSubset(listTables)
    #
    # Read the usagi file
    #
    cols <- readr::cols(
        sourceCode = readr::col_character(),
        sourceName = readr::col_character(),
        sourceFrequency = readr::col_integer(),
        sourceAutoAssignedConceptIds = readr::col_integer(),
        matchScore = readr::col_double(),
        mappingStatus = readr::col_character(),
        equivalence = readr::col_character(),
        statusSetBy = readr::col_character(),
        statusSetOn = readr::col_double(),
        conceptId = readr::col_integer(),
        conceptName = readr::col_character(),
        domainId = readr::col_character(),
        mappingType = readr::col_character(),
        comment = readr::col_character(),
        createdBy = readr::col_character(),
        createdOn = readr::col_double(),
        assignedReviewer = readr::col_character(),
        #
        `ADD_INFO:sourceConceptId` = readr::col_double(),
        `ADD_INFO:sourceConceptClass` = readr::col_character(),
        `ADD_INFO:sourceDomain` = readr::col_character(),
        `ADD_INFO:sourceValidStartDate` = readr::col_date(),
        `ADD_INFO:sourceValidEndDate` = readr::col_date(),
        `ADD_INFO:sourceParents` = readr::col_character(),
        `ADD_INFO:sourceParentVocabulary` = readr::col_character(),
        `ADD_INFO:validationMessages` = readr::col_character(),
        #
        .default = readr::col_character()
    )

    usagiTibble <- readr::read_csv(pathToUsagiFile, col_types = cols)
    usagiTibbleColumns <- usagiTibble |> names()

    # Add tmpValidationMessage message column
    usagiTibble <- usagiTibble |>
        dplyr::mutate(tmpValidationMessage = NA_character_)



    #
    # Checks
    #

    validationLogTibble <- LogTibble$new()

    # - Check default usagi columns
    missingColumns <- usagiTibbleColumns |>
        setdiff(c(
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
        ))

    if (length(missingColumns) > 0) {
        validationLogTibble$ERROR(
            "Missing default columns",
            paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
        )
        return(validationLogTibble)
    }

    validationRules <- validate::validator(
        sourceCode.and.conceptId.are.not.unique = is_unique(sourceCode, conceptId),
        sourceCode.is.empty = is_complete(sourceCode),
        sourceName.is.empty = is_complete(sourceName),
        sourceName.morethan.255char = field_length(sourceName, min = 0, max = 255),
        concept_id.is.0.for.ACCEPTED.mappingStatus = if (mappingStatus == "APPROVED") conceptId != 0
    )

    result <- .applyValidationRules(usagiTibble, validationRules, validationLogTibble)
    usagiTibble <- result$usagiTibble
    validationLogTibble <- result$validationLogTibble

    # if it has the ADD_INFO:sourceConceptId and ADD_INFO:sourceConceptClass and ADD_INFO:sourceDomain will be used as C&CR
    # if it has any of the 3 columns it must have the other 2
    if ("ADD_INFO:sourceConceptId" %in% usagiTibbleColumns || "ADD_INFO:sourceConceptClass" %in% usagiTibbleColumns || "ADD_INFO:sourceDomain" %in% usagiTibbleColumns) {
        missingColumns <- usagiTibbleColumns |>
            setdiff(c(
                "ADD_INFO:sourceConceptId",
                "ADD_INFO:sourceConceptClass",
                "ADD_INFO:sourceDomain"
            ))

        if (length(missingColumns) > 0) {
            validationLogTibble$ERROR(
                "Missing C&CR columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogTibble)
        }

        validDomains <- con |>
            DBI::dbGetQuery("SELECT domain_id FROM DOMAIN") |>
            dplyr::pull(domain_id)
        validationRules <- validate::validator(
            sourceCode.and.sourceConceptId.are.not.unique = is_unique(sourceCode, sourceConceptId),
            sourceConceptClass.morethan.20char = field_length(sourceConceptClass, min = 0, max = 20),
            sourceDomain.is.not.a.valid.domain = sourceDomain %in% validDomains
        )

        result <- .applyValidationRules(usagiTibble, validationRules, validationLogTibble)
        usagiTibble <- result$usagiTibble
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
                dplyr::mutate(tmpValidationMessage = ifelse(is.na(tmpValidationMessage), errorMessage, paste0(tmpValidationMessage, " | ", errorMessage)))
        }
    }

    # - Check if ADD_INFO:sourceValidStartDate and ADD_INFO:sourceValidEndDate are used
    if ("ADD_INFO:sourceValidStartDate" %in% usagiTibbleColumns || "ADD_INFO:sourceValidEndDate" %in% usagiTibbleColumns) {
        missingColumns <- usagiTibbleColumns |>
            setdiff(c(
                "ADD_INFO:sourceValidStartDate",
                "ADD_INFO:sourceValidEndDate"
            ))

        if (length(missingColumns) > 0) {
            validationLogTibble$ERROR(
                "Missing date columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogTibble)
        }

        validationRules <- validate::validator(
            sourceValidStartDate.is.before.sourceValidEndDate = `ADD_INFO:sourceValidStartDate` <= `ADD_INFO:sourceValidEndDate`
        )

        result <- .applyValidationRules(usagiTibble, validationRules, validationLogTibble)
        usagiTibble <- result$usagiTibble
        validationLogTibble <- result$validationLogTibble
    }

    # Check ADD_INFO:sourceParents,ADD_INFO:sourceParentVocabulary
    if ("ADD_INFO:sourceParents" %in% usagiTibbleColumns || "ADD_INFO:sourceParentVocabulary" %in% usagiTibbleColumns) {
        missingColumns <- usagiTibbleColumns |>
            setdiff(c(
                "ADD_INFO:sourceParents",
                "ADD_INFO:sourceParentVocabulary"
            ))

        if (length(missingColumns) > 0) {
            validationLogTibble$ERROR(
                "Missing parent columns",
                paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
            )
            return(validationLogTibble)
        }

        # check if sourceParents is a valid conceptCode in the ParentVocabulary
        validVocabularyConceptCodes <- usagiTibble |>
            dplyr::transmute(vocabulary_id = NA_character_, concept_code = sourceCode)

        usedParentVocabularies <- usagiTibble |>
            dplyr::select(`ADD_INFO:sourceParentVocabulary`) |>
            dplyr::distinct() |>
            dplyr::pull(`ADD_INFO:sourceParentVocabulary`) |>
            stringr::str_split("\\|") |>
            purrr::flatten_chr() |>
            unique()
        if (length(usedParentVocabularies) > 0) {
            parentVocabularyConceptCodes <- dplyr::tbl(con, "CONCEPT") |>
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

                    tibble::tibble(vocabulary_id = y, concept_code = x)
                })
            ) |>
            tidyr::unnest(cols = c(vocabulary_id)) |>
            dplyr::select(-`ADD_INFO:sourceParentVocabulary`, -`ADD_INFO:sourceParents`)

        notValidParentConceptCodes <- parentConceptCodes |>
            dplyr::anti_join(validVocabularyConceptCodes, by = c("vocabulary_id", "concept_code")) |>
            dplyr::mutate(errorMessage = if_else(is.na(vocabulary_id),
                paste0("ERROR: ", concept_code, " is not a valid concept in this vocabulary"),
                paste0("ERROR: ", concept_code, " is not a valid concept code in vocabulary ", vocabulary_id)
            )) |>
            dplyr::group_by(row) |>
            dplyr::summarise(errorMessage = paste(errorMessage, collapse = " | ")) |>
            dplyr::ungroup()

        usagiTibble <- usagiTibble |>
            dplyr::left_join(notValidParentConceptCodes, by = "row") |>
            dplyr::mutate(tmpValidationMessage = ifelse(is.na(tmpValidationMessage), errorMessage, paste0(tmpValidationMessage, " | ", errorMessage)))
    }


    # end
    usagiTibble |>
        dplyr::mutate(
            mappingStatus = case_when(
                tmpValidationMessage != "" ~ "FLAGGED",
                tmpValidationMessage == "" & conceptId != 0 ~ "APPROVED",
                TRUE ~ mappingStatus
            ),
            comment = case_when(
                comment != "" & tmpValidationMessage != "" ~ paste0(comment, " | ", tmpValidationMessage),
                comment == "" & tmpValidationMessage != "" ~ tmpValidationMessage,
                TRUE ~ comment
            )
        ) |>
        readr::write_csv(pathToEditedUsagiFile, na = "")


    DBI::dbDisconnect(con)
    return(validationLogTibble)
}



.applyValidationRules <- function(usagiTibble, validationRules, validationLogTibble) {
    validations <- validate::confront(usagiTibble, validationRules)

    validationSummary <- validate::summary(validations) |> tibble::as_tibble()
    if ("name" %in% names(validationSummary)) {
        validationSummary <- validationSummary |>
            dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " "))
    }

    for (i in 1:nrow(validationSummary)) {
        row <- validationSummary[i, ]
        if (row$error) {
            validationLogTibble$ERROR(
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
    if (ncol(failedRulesRows) > 0) {
        failedRulesRows <- failedRulesRows |>
            dplyr::mutate_all(~ dplyr::if_else(!.x, dplyr::row_number(), as.integer(NA))) |>
            tidyr::pivot_longer(cols = tidyr::everything(), names_to = "name", values_to = "row", values_drop_na = TRUE) |>
            dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " ")) |>
            dplyr::mutate(errorMessage = paste0("ERROR: ", name)) |>
            dplyr::select(row, errorMessage)
    } else {
        failedRulesRows <- tibble::tibble(row = as.integer(), errorMessage = as.character(NA), .rows = 0)
    }

    failedRulesRows |>
        dplyr::group_by(row) |>
        dplyr::mutate(errorMessage = paste(errorMessage, collapse = " | ")) |>
        dplyr::ungroup()

    usagiTibble <- usagiTibble |>
        dplyr::mutate(row = dplyr::row_number()) |>
        dplyr::left_join(failedRulesRows, by = "row") |>
        dplyr::mutate(tmpValidationMessage = ifelse(is.na(tmpValidationMessage), errorMessage, paste0(tmpValidationMessage, " | ", errorMessage)))

    return(list(usagiTibble = usagiTibble, validationLogTibble = validationLogTibble))
}
