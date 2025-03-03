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
updateUsagiFile <- function(
    pathToUsagiFile,
    connection,
    vocabularyDatabaseSchema,
    pathToUpdatedUsagiFile,
    updateLevelTibble = NULL,
    appendOrClearAutoUpdatingInfo = "append",
    skipValidation = FALSE) {
    #
    # Parameter validation
    #
    checkmate::assertFileExists(pathToUsagiFile)
    # vocabularyDatabaseSchema exists in the connection
    checkmate::assertCharacter(vocabularyDatabaseSchema, len = 1, any.missing = FALSE)
    checkmate::assertChoice(appendOrClearAutoUpdatingInfo, choices = c("append", "clear"))
    # Check if required tables exist
    # tables <- DatabaseConnector::getTableNames(condlnection, vocabularyDatabaseSchema)
    # TEMP untill solved https://github.com/OHDSI/DatabaseConnector/issues/299
    tableNames <- DatabaseConnector::dbListTables(connection, vocabularyDatabaseSchema)
    c("concept", "concept_relationship", "domain") |>
        checkmate::assertSubset(tableNames)

    if (is.null(updateLevelTibble)) {
        updateLevelTibble <- tibble::tribble(
            ~relationshipId, ~level, ~needsReview,
            "Maps to", 1, FALSE,
            "Concept replaced by", 2, TRUE,
            "Concept same_as to", 3, TRUE,
            "Concept poss_eq to", 4, TRUE
        )
    }

    # Read the usagi file
    usagiTibble <- readUsagiFile(pathToUsagiFile)
    usagiTibbleColumns <- usagiTibble |> names()

    # validate the usagi file
    if (!skipValidation) {
        usagiTibble <- validateUsagiFile(pathToUsagiFile, connection, vocabularyDatabaseSchema, tempfile())
        if (usagiTibble |> dplyr::filter(type == "ERROR") |> nrow() > 0) {
            stop("The usagi file has the following errors: ", usagiTibble |> dplyr::filter(type == "ERROR") |> dplyr::pull(message) |> paste(collapse = "\n"))
        }
    }


    #
    # Function
    #

    updateLogTibble <- LogTibble$new()

    #
    # update
    #
    # Check if auto updating info column exists and rename it
    if ("ADD_INFO:autoUpdatingInfo" %in% usagiTibbleColumns) {
        usagiTibble <- usagiTibble |>
            dplyr::rename(autoUpdatingInfo = `ADD_INFO:autoUpdatingInfo`)
    } else {
        usagiTibble <- usagiTibble |>
            dplyr::mutate(autoUpdatingInfo = "")
    }

    if (appendOrClearAutoUpdatingInfo == "clear") {
        usagiTibble <- usagiTibble |>
            dplyr::mutate(
                autoUpdatingInfo = ""
            )
    }


    # - update conceptIds: Find possible updates for the conceps that became non-standard
    # Find codes that are mapped to a non-standard concept
    mappedConceptIds <- usagiTibble |>
        dplyr::filter(conceptId != 0) |>
        dplyr::distinct(conceptId) |>
        dplyr::pull(conceptId)

    mappedConcepts <- dplyr::tbl(connection, "CONCEPT") |>
        dplyr::filter(concept_id %in% mappedConceptIds) |>
        dplyr::filter(is.na(standard_concept)) |>
        dplyr::select(concept_id, domain_id, concept_name) |>
        dplyr::collect() |>
        SqlRender::snakeCaseToCamelCaseNames()

    outdatedStandardConcepts <- usagiTibble |>
        dplyr::inner_join(mappedConcepts, by = c("conceptId")) |>
        dplyr::distinct(sourceCode, conceptId)

    # Use the relationships to find possible updates for the conceptIds
    relationshipsToUse <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP") |>
        dplyr::filter(concept_id_1 %in% outdatedStandardConcepts$conceptId) |>
        dplyr::filter(relationship_id %in% updateLevelTibble$relationshipId) |>
        dplyr::collect() |>
        dplyr::select(oldConceptId = concept_id_1, newConceptId = concept_id_2, relationshipId = relationship_id) |>
        dplyr::left_join(
            updateLevelTibble,
            by = "relationshipId"
        ) |>
        dplyr::group_by(oldConceptId) |>
        dplyr::arrange(level) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::select(-level)

    outdatedStandardConcepts <- outdatedStandardConcepts |>
        dplyr::rename(oldConceptId = conceptId) |>
        dplyr::left_join(
            relationshipsToUse,
            by = c("oldConceptId")
        ) |>
        dplyr::mutate(
            action = dplyr::case_when(
                needsReview ~ "needsReview",
                !needsReview ~ "noReview",
                TRUE ~ "needsRemapping"
            )
        )

    usagiTibble <- usagiTibble |>
        # join to sourceCode, modify only the affected conceptName, but modify all the autoUpdatingInfo
        dplyr::left_join(outdatedStandardConcepts |> dplyr::mutate(conceptId = oldConceptId), by = c("sourceCode", "conceptId")) |>
        dplyr::mutate(
            conceptId = dplyr::if_else(!is.na(newConceptId), newConceptId, conceptId),
            mappingStatus = dplyr::if_else(!is.na(needsReview) & needsReview, "UNCHECKED", mappingStatus),
            infoTmp = dplyr::case_when(
                action == "needsRemapping" ~ paste0("conceptId ", oldConceptId, " could not be updated automatically, remapping needed"),
                action == "needsReview" ~ paste0("conceptId changed from ", oldConceptId, " to ", newConceptId, " based on relationship :", relationshipId, ", needs reviewing"),
                action == "noReview" ~ paste0("conceptId changed from ", oldConceptId, " to ", newConceptId, " based on relationship :", relationshipId, ", does not need reviewing"),
                TRUE ~ NA_character_
            ),
            hasChangendConceptId = !is.na(newConceptId)
        ) |>
        dplyr::group_by(sourceCode) |>
        dplyr::mutate(infoTmp = paste0(infoTmp |> na.omit() |> unique(), collapse = " | ")) |>
        dplyr::ungroup() |>
        dplyr::mutate(autoUpdatingInfo = dplyr::if_else(infoTmp == "", autoUpdatingInfo, paste0(autoUpdatingInfo, " | ", infoTmp))) |>
        dplyr::select(-oldConceptId, -newConceptId, -relationshipId, -needsReview, -infoTmp)


    n <- usagiTibble |>
        dplyr::filter(action == "noReview") |>
        dplyr::distinct(sourceCode) |>
        nrow()
    if (n > 0) {
        updateLogTibble$INFO("Updated conceptIds", paste0("Updated ", n, " conceptIds that don't need review"))
    }

    n <- usagiTibble |>
        dplyr::filter(action == "needsReview") |>
        dplyr::distinct(sourceCode) |>
        nrow()
    if (n > 0) {
        updateLogTibble$WARNING("Updated conceptIds", paste0("Updated ", n, " conceptIds that need review"))
    }

    n <- usagiTibble |>
        dplyr::filter(action == "needsRemapping") |>
        dplyr::distinct(sourceCode) |>
        nrow()
    if (n > 0) {
        updateLogTibble$WARNING("Updated conceptIds", paste0(n, " conceptIds could not be updated automatically, remapping needed"))
    }


    # - update domains
    mappedConceptIds <- usagiTibble |>
        dplyr::filter(conceptId != 0) |>
        dplyr::distinct(conceptId) |>
        dplyr::pull(conceptId)

    mappedConcepts <- dplyr::tbl(connection, "CONCEPT") |>
        dplyr::filter(concept_id %in% mappedConceptIds) |>
        dplyr::select(concept_id, domain_id, concept_name) |>
        dplyr::collect() |>
        SqlRender::snakeCaseToCamelCaseNames()

    outdatedDomains <- usagiTibble |>
        dplyr::inner_join(mappedConcepts, by = c("conceptId")) |>
        dplyr::rename(oldDomainId = domainId.x, newDomainId = domainId.y) |>
        dplyr::filter(oldDomainId != newDomainId) |>
        dplyr::distinct(sourceCode, conceptId, oldDomainId, newDomainId)

    if (outdatedDomains |> nrow() > 0) {
        usagiTibble <- usagiTibble |>
            # join to sourceCode, modify only the affected domain, but modify all the autoUpdatingInfo
            dplyr::left_join(outdatedDomains, by = c("sourceCode", "conceptId")) |>
            dplyr::mutate(
                domainId = dplyr::if_else(!is.na(newDomainId), newDomainId, domainId),
                autoUpdatingInfo = dplyr::if_else(!is.na(newDomainId), paste0(autoUpdatingInfo, " | domainId updated from ", oldDomainId, " to ", newDomainId), autoUpdatingInfo),
                hasChangedDomainId = !is.na(newDomainId)
            ) |>
            dplyr::select(-oldDomainId, -newDomainId)


        n <- usagiTibble |>
            dplyr::filter(hasChangedDomainId & !hasChangendConceptId) |>
            dplyr::distinct(sourceCode) |>
            nrow()
        if (n > 0) {
            updateLogTibble$INFO("Updated domains", paste0("Updated ", n, " domains"))
        }
        usagiTibble <- usagiTibble |>
            dplyr::select(-hasChangedDomainId)
    }

    # - update concept names
    outdatedNames <- usagiTibble |>
        dplyr::inner_join(mappedConcepts, by = c("conceptId")) |>
        dplyr::rename(oldConceptName = conceptName.x, newConceptName = conceptName.y) |>
        dplyr::filter(oldConceptName != newConceptName) |>
        dplyr::distinct(sourceCode, conceptId, oldConceptName, newConceptName)

    if (outdatedNames |> nrow() > 0) {
        usagiTibble <- usagiTibble |>
            # join to sourceCode, modify only the affected conceptName, but modify all the autoUpdatingInfo
            dplyr::left_join(outdatedNames, by = c("sourceCode", "conceptId")) |>
            dplyr::mutate(
                conceptName = dplyr::if_else(!is.na(newConceptName), newConceptName, conceptName),
                autoUpdatingInfo = dplyr::if_else(!is.na(newConceptName), paste0(autoUpdatingInfo, " | conceptName changed from ", oldConceptName, " to ", newConceptName), autoUpdatingInfo),
                hasChangedConceptName = !is.na(newConceptName)
            ) |>
            dplyr::select(-oldConceptName, -newConceptName)

        n <- usagiTibble |>
            dplyr::filter(hasChangedConceptName & !hasChangendConceptId) |>
            dplyr::distinct(sourceCode) |>
            nrow()
        if (n > 0) {
            updateLogTibble$INFO("Updated concept names", paste0("Updated ", n, " concept names"))
        }
        usagiTibble <- usagiTibble |>
            dplyr::select(-hasChangedConceptName)
    }

    #
    # end
    #
    usagiTibble |>
        dplyr::select(-action, -hasChangendConceptId) |>
        dplyr::mutate(autoUpdatingInfo = ifelse(autoUpdatingInfo == "", autoUpdatingInfo, paste0(lubridate::today(), autoUpdatingInfo))) |>
        dplyr::rename(`ADD_INFO:autoUpdatingInfo` = autoUpdatingInfo) |>
        readr::write_csv(pathToUpdatedUsagiFile, na = "")

    return(updateLogTibble$logTibble)
}
