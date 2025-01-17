appendUsagiFileToSTCMtable <- function(vocabularyId, pathToUsagiFile, connection, vocabularyDatabaseSchema, sourceToConceptMapTable, skipValidation = TRUE) {
    vocabularyId |> checkmate::assertString()
    pathToUsagiFile |> checkmate::assertFileExists()
    vocabularyDatabaseSchema |> checkmate::assertString()
    sourceToConceptMapTable |> checkmate::assertString()

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
        usagiTibble <- validateUsagiFile(pathToUsagiFile, connection, vocabularyDatabaseSchema, tempfile())
        if (usagiTibble |> dplyr::filter(type == "ERROR") |> dplyr::nrow() > 0) {
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
        dplyr::transmute(
            source_code = sourceCode,
            source_concept_id = 0L,
            source_vocabulary_id = {{vocabularyId}},
            source_code_description = sourceName,
            target_concept_id = conceptId,
            target_vocabulary_id = "",
            valid_start_date = as.Date('1970-01-01'),
            valid_end_date = as.Date('2099-12-31'),
            invalid_reason = NA_character_
        )


    }else{

        # if colums `ADD_INFO:sourceValidStartDate` and `ADD_INFO:sourceValidEndDate` are not present, set them to the default values
        if (!"ADD_INFO:sourceValidStartDate" %in% usagiTibbleColumns) {
            usagiTibble <- usagiTibble |> dplyr::mutate(`ADD_INFO:sourceValidStartDate` = lubridate::as_date(ymd('1970-01-01')))
        }
        if (!"ADD_INFO:sourceValidEndDate" %in% usagiTibbleColumns) {
            usagiTibble <- usagiTibble |> dplyr::mutate(`ADD_INFO:sourceValidEndDate` = lubridate::as_date(ymd('2099-12-31')))
        }

        # if colums `ADD_INFO:sourceParentsConceptIds` are not present, set them to the default values
        if (!"ADD_INFO:sourceParentsConceptIds" %in% usagiTibbleColumns) {
            usagiTibble <- usagiTibble |> dplyr::mutate(`ADD_INFO:sourceParentsConceptIds` = NA_character_)
        }

        STCMTableToInsert <- usagiTibble |> 
        dplyr::transmute(
            source_code = sourceCode,
            source_concept_id = `ADD_INFO:sourceConceptId`,
            source_vocabulary_id = {{vocabularyId}},
            source_code_description = sourceName,
            target_concept_id = conceptId,
            target_vocabulary_id = "",
            valid_start_date = `ADD_INFO:sourceValidStartDate`,
            valid_end_date = `ADD_INFO:sourceValidEndDate`,
            invalid_reason = NA_character_,
            source_concept_class = `ADD_INFO:sourceConceptClass`,
            source_domain = `ADD_INFO:sourceDomain`,
            source_parents_concept_ids = `ADD_INFO:sourceParentsConceptIds`
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
