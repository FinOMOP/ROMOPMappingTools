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
    skipValidation = FALSE) {
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
        dplyr::filter(is.na(domainId.y) | is.na(conceptName.y) | domainId.x != domainId.y | conceptName.x != conceptName.y | is.na(standardConcept))

    missingConcepts <- outdatedConcepts |>
        dplyr::filter(is.na(domainId.y) | is.na(conceptName.y))

    if (nrow(missingConcepts) > 0) {
        updateLogTibble$ERROR("Missing concepts", paste0("Found ", nrow(missingConcepts), " missing concepts, the database needs to be updated. List of missing concepts: ", missingConcepts |> dplyr::pull(conceptId) |> paste(collapse = ", ")))
       # return(updateLogTibble$logTibble)
    }

    #
    # update
    #
    updatedConcepts <- outdatedConcepts |>
        dplyr::mutate(
            autoUpdatingInfo = lubridate::now() |> as.character()
        )

    # update domains
    updatedConcepts <- updatedConcepts |>
        dplyr::mutate(
            newDomainId = dplyr::if_else(domainId.x != domainId.y, domainId.y, domainId.x),
            autoUpdatingInfo = dplyr::if_else(domainId.x != domainId.y, paste0(autoUpdatingInfo, " | domainId changed from ", domainId.x, " to ", domainId.y), autoUpdatingInfo)
        )

    if (updatedConcepts |> dplyr::filter(domainId.x != domainId.y) |> nrow() > 0) {
        updateLogTibble$INFO("Updated domains", paste0("Updated ", updatedConcepts |> dplyr::filter(domainId.x != domainId.y) |> nrow(), " domains"))
    }

    # update concept names
    updatedConcepts <- updatedConcepts |>
        dplyr::mutate(
            newConceptName = dplyr::if_else(conceptName.x != conceptName.y, conceptName.y, conceptName.x),
            autoUpdatingInfo = dplyr::if_else(conceptName.x != conceptName.y, paste0(autoUpdatingInfo, " | conceptName changed from ", conceptName.x, " to ", conceptName.y), autoUpdatingInfo)
        )

    if (updatedConcepts |> dplyr::filter(conceptName.x != conceptName.y) |> nrow() > 0) {
        updateLogTibble$INFO("Updated concept names", paste0("Updated ", updatedConcepts |> dplyr::filter(conceptName.x != conceptName.y) |> nrow(), " concept names"))
    }

    browser()
    # update conceptIds
    # Find possible updates for the conceps that became non-standard
    nonStandardConceptIds <- outdatedConcepts |>
        dplyr::filter(is.na(standardConcept)) |>
        dplyr::pull(conceptId)

    mappedConcepts <- dplyr::tbl(connection, "CONCEPT_RELATIONSHIP") |>
        dplyr::filter(concept_id_1 %in% nonStandardConceptIds) |>
        dplyr::filter(relationship_id %in% c("Maps to", "Concept replaced by", "Concept same_as to", "Concept poss_eq to")) |>
        dplyr::select(concept_id_1, concept_id_2, relationship_id) |>
        dplyr::collect() |>
        SqlRender::snakeCaseToCamelCaseNames() |>
        dplyr::left_join(
            tibble::tribble(
                ~relationshipId, ~level, ~needsReview,
                "Maps to", 1, FALSE,
                "Concept replaced by", 2, TRUE,
                "Concept same_as to", 3, TRUE,
                "Concept poss_eq to", 4, TRUE
            ),
            by = "relationshipId"
        ) |>
        dplyr::group_by(conceptId1) |>
        dplyr::arrange(level) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::select(conceptId = conceptId1, newConceptId = conceptId2, newRelationshipId = relationshipId, needsReview)

    updatedConcepts <- updatedConcepts |>
        dplyr::left_join(mappedConcepts, by = c("conceptId"))

    updatedConcepts <- updatedConcepts |>
        dplyr::mutate(
            autoUpdatingInfo = dplyr::if_else(is.na(standardConcept) & !is.na(newConceptId),
                paste0(autoUpdatingInfo, " | conceptId changed from ", conceptId, " to ", newConceptId, " based on new relationship :", newRelationshipId),
                autoUpdatingInfo
            )
        )

    if (updatedConcepts |> dplyr::filter(!needsReview) |> nrow() > 0) {
        updateLogTibble$INFO("Updated conceptIds", paste0("Updated ", updatedConcepts |> dplyr::filter(!needsReview) |> nrow(), " that don't need review"))
    }

    if (updatedConcepts |> dplyr::filter(needsReview) |> nrow() > 0) {
        updateLogTibble$WARNING("Updated conceptIds", paste0("Updated ", updatedConcepts |> dplyr::filter(needsReview) |> nrow(), " that need review"))
    }


    # update the usagi file
    usagiTibble <- usagiTibble |>
        

    # end
    usagiTibble |>
        dplyr::mutate(
            tmpvalidationMessages = stringr::str_replace(tmpvalidationMessages, "^\\s*\\|\\s*", ""),
            mappingStatus = case_when(
                tmpvalidationMessages != "" ~ "FLAGGED",
                TRUE ~ mappingStatus
            )
        ) |>
        dplyr::rename(`ADD_INFO:validationMessages` = tmpvalidationMessages) |>
        readr::write_csv(pathToValidatedUsagiFile, na = "")

    return(updateLogTibble$logTibble)

}
