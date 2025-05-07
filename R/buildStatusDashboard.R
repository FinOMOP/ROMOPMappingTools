#' Build Status Dashboard
#'
#' This function generates a status dashboard for mapping status.
#'
#' @param pathToCodeCountsFolder Path to folder containing code counts files
#' @param connectionDetails DatabaseConnector connection details object
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables
#' @param output_file_html The path to the output HTML file
#'
#' @return A validation log tibble
#'
#' @importFrom dplyr select mutate filter if_else
#' @importFrom rmarkdown
#' @importFrom reactable reactable
#' @importFrom shiny div
#'
#' @export
buildStatusDashboard <- function(
    pathToCodeCountsFolder,
    pathToVocabularyFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    output_file_html = file.path(tempdir(), "MappingStatusDashboard.html")) {
  #
  # Validate parameters
  #

  pathToCodeCountsFolder |> checkmate::assert_directory_exists()
  connectionDetails |> checkmate::assert_class(c("ConnectionDetails"))
  vocabularyDatabaseSchema |> checkmate::assert_character()
  output_file_html |> checkmate::assert_character()


  #
  # Function
  #

  # validate code counts folder
  validationLogTibble <- validateCodeCountsFolder(
    pathToCodeCountsFolder = pathToCodeCountsFolder
  )

  if (validationLogTibble |> dplyr::filter(type == "ERROR") |> nrow() > 0) {
    return(validationLogTibble)
  }

  # calculate mapping status
  mappingStatus <- calculateMappingStatus(
    pathToCodeCountsFolder = pathToCodeCountsFolder,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

  rmarkdown::render(system.file("reports", "mapping_status_dashboard.Rmd", package = "ROMOPMappingTools"),
    params = list(
      mapping_status = mappingStatus
    ),
    output_file = output_file_html
  )


  return(validationLogTibble)
}



.pageSummaryTableForVocabularyAndDatabase <- function(
  summaryTableForVocabularyAndDatabase, 
  targetVocabularyId,
  databaseName
  ) {

  inputTemplate <- system.file("reports", "pageCoverageVocabularyDatabase.Rmd", package = "ROMOPMappingTools")
  outputCoverageVocabularyDatabaseHtml <- file.path(tempdir(), "pageCoverageVocabularyDatabase.html")

  rmarkdown::render(
    input = inputTemplate,
    params = list(
      summaryTableForVocabularyAndDatabase = summaryTableForVocabularyAndDatabase,
      targetVocabularyId = targetVocabularyId, 
      databaseName = databaseName
    ),
    output_file = outputCoverageVocabularyDatabaseHtml
  )

  return(outputCoverageVocabularyDatabaseHtml)
  
}

.plotSummaryTableForVocabularyAndDatabase <- function(summaryTableForVocabularyAndDatabase) {

  greyColor <- "#AAAAAA"
  athenaUrl <- "https://athena.ohdsi.org/search-terms/terms/"

  toPlot <- summaryTableForVocabularyAndDatabase |>
  dplyr::mutate(
    n_events = dplyr::if_else(is.na(n_events) | n_events < 0, 0, n_events),
    p_events = n_events / sum(n_events) * 100,
    statusColor = dplyr::case_when(
      status == "INVALID" ~ "#EC6173",
      status == "UNMAPPED" ~ "#F1AE4A",
      status == "MAPS TO" ~ "#51A350"
    ),
  ) |>
    dplyr::group_by(source_code) |>
    dplyr::mutate(
      nMapsTo = dplyr::n_distinct(target_concept_id)
    ) |>
    dplyr::ungroup() |>
  dplyr::transmute(
    sourceCode = paste0(source_code, " <br><span style='color:", greyColor,"'> (", source_vocabulary_id, ")</span>"),
    sourceName = source_concept_name,
    status = dplyr::if_else(status == "MAPS TO", paste0(status, " ", nMapsTo), status),
    status = paste0("<span style='color: ",statusColor,";'>",status,"</span>", dplyr::if_else(!is.na(equivalence), paste0(" <br><span style='color:", greyColor,"'> (", equivalence, ")</span>"), "")),
    targetCode = dplyr::if_else(!is.na(target_concept_id), paste0(target_code, " <br><span style='color:", greyColor,"'> (", target_vocabulary_id, ")</span>"), ""),
    targetName = dplyr::if_else(!is.na(target_concept_name), paste0("<a href='", athenaUrl, target_concept_id, "' target='_blank'>", target_concept_name, "</a>"), ""),
    n_events = paste0(n_events, " <br><span style='color:", greyColor,"'> (", round(p_events, 2), "%)</span>"),
    statusSetBy = statusSetBy
  ) |> 
  dplyr::arrange(dplyr::desc(n_events))

  columns <- list(
    sourceCode = reactable::colDef(
      name = paste0("Source Code<br><span style='color:", greyColor,"'>(Vocabulary)</span>"),
      maxWidth = 120,
      html = TRUE
    ),
    sourceName = reactable::colDef(
      name = "Source Name",
    ),
    status = reactable::colDef(
      name = paste0("Status<br><span style='color:", greyColor,"'>(Equivalence)</span>"),
      maxWidth = 150,
      html = TRUE
    ),
    targetCode = reactable::colDef(
      name = paste0("Target Code<br><span style='color:", greyColor,"'>(Vocabulary)</span>"),
      maxWidth = 120,
      html = TRUE
    ),
    targetName = reactable::colDef(
      name = "Target Name",
      html = TRUE
    ),
    n_events = reactable::colDef(
      name = paste0("N Events<br><span style='color:", greyColor,"'>(%)</span>"),
      maxWidth = 100,
      html = TRUE
    ),
    statusSetBy = reactable::colDef(
      name = "Status Set By",
      maxWidth = 200,
    )
  )

  reactable::reactable(
    toPlot,
    columns = columns,
    resizable = TRUE,
    filterable = TRUE,
    defaultPageSize = 10
  )

}

.getSummaryTableForVocabularyAndDatabase <- function(
    pathToCodeCountsFolder,
    pathToVocabularyFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    sourceVocabularyId,
    targetVocabularyIds,
    databaseName
) {

  databaseSummary <- .getDatabaseSummaryForVocabulary(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    targetVocabularyIds = targetVocabularyIds
  )

  codeCounts <- .getCodeCountsForVocabularyAndDatabase(
    pathToCodeCountsFolder = pathToCodeCountsFolder,
    sourceVocabularyId = sourceVocabularyId,
    databaseName = databaseName
  )

  usagiSummary <- .getUsagiSummaryForVocabulary(
    pathToVocabularyFolder = pathToVocabularyFolder,
    sourceVocabularyId = sourceVocabularyId
  )

  summaryTableForVocabularyAndDatabase <- databaseSummary |>
    dplyr::full_join(codeCounts, by = c("source_vocabulary_id" = "source_vocabulary_id", "source_code" = "source_code")) |>
    dplyr::left_join(usagiSummary, by = c("source_concept_id" = "source_concept_id", "target_concept_id" = "target_concept_id")) |>
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(source_concept_id) ~ "INVALID",
        is.na(target_concept_id) ~ "UNMAPPED",
        TRUE ~ "MAPS TO"
      )
    )

  return(summaryTableForVocabularyAndDatabase)


}


.getCodeCountsForVocabularyAndDatabase <- function(
    pathToCodeCountsFolder,
    sourceVocabularyId,
    databaseName
) {

  databaseCoverageTibble <- file.path(pathToCodeCountsFolder, "databases_coverage.csv") |>
    readr::read_csv()

  databaseCoverageTibble <- databaseCoverageTibble |>
    dplyr::filter(database_name == databaseName)

  codeCountsTibble <- file.path(pathToCodeCountsFolder, databaseCoverageTibble |> dplyr::pull(path_to_code_counts_file)) |>
    readr::read_csv()

  codeCountsTibble <- codeCountsTibble |>
    dplyr::filter(source_vocabulary_id == sourceVocabularyId)

  # for (i in 1:nrow(databaseCoverageTibble)) {
  #   databaseName <- databaseCoverageTibble$database_name[i]
  #   codeCountsTibble <- file.path(pathToCodeCountsFolder, databaseCoverageTibble$path_to_code_counts_file[i]) |>
  #     readr::read_csv()

  #   codeCountsTibble <- codeCountsTibble |>
  #     dplyr::filter(source_vocabulary_id == sourceVocabularyId) |>
  #     dplyr::mutate(database_name = databaseName) |>
  #     dplyr::select(database_name, source_vocabulary_id, source_code, n_events)

  #   vocabularyCodeCountsTibble <- vocabularyCodeCountsTibble |>
  #     dplyr::bind_rows(codeCountsTibble)
  # }

  # vocabularyCodeCountsTibble <- vocabularyCodeCountsTibble |>
  #   dplyr::mutate(n_events = dplyr::if_else(n_events < 0, 0, n_events)) |>
  #   dplyr::group_by(database_name) |>
  #   dplyr::mutate(p_events = n_events / sum(n_events) * 100) |>
  #   dplyr::ungroup() |>
  #   dplyr::group_by(source_vocabulary_id, source_code) |>
  #   dplyr::mutate(p_events_all = sum(p_events) / dplyr::n_distinct(database_name)) |>
  #   dplyr::ungroup() |>
  #   dplyr::nest_by(source_vocabulary_id, source_code, p_events_all)

  return(codeCountsTibble)
}

.getDatabaseSummaryForVocabulary <- function(
    connectionDetails,
    vocabularyDatabaseSchema,
    targetVocabularyIds
  ) {
  connection <- DatabaseConnector::connect(connectionDetails)

  sql <- "
      SELECT
        c.vocabulary_id AS source_vocabulary_id,
        c.concept_code AS source_code,
        c.concept_name AS source_concept_name,
        c.concept_id AS source_concept_id,
        c2.vocabulary_id AS target_vocabulary_id,
        c2.concept_code AS target_code,
        c2.concept_name AS target_concept_name,
        c2.concept_id AS target_concept_id
      FROM (
        SELECT * FROM @vocabulary_database_schema.CONCEPT
        WHERE vocabulary_id IN (@list_vocabulary_ids)
      ) AS c
      LEFT JOIN (
        SELECT concept_id_1, concept_id_2  FROM @vocabulary_database_schema.CONCEPT_RELATIONSHIP
        WHERE relationship_id = 'Maps to'
      ) AS cr
        ON c.concept_id = cr.concept_id_1
      LEFT JOIN  @vocabulary_database_schema.CONCEPT AS c2
        ON cr.concept_id_2 = c2.concept_id
  "

  sql <- SqlRender::render(
    sql = sql,
    vocabulary_database_schema = vocabularyDatabaseSchema,
    list_vocabulary_ids = paste0("'", targetVocabularyIds, "'") |> paste0(collapse = ", ")
  )
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  vocabulariesDBInfo <- DatabaseConnector::dbGetQuery(connection, sql) |> tibble::as_tibble()

  DatabaseConnector::disconnect(connection)

  return(vocabulariesDBInfo)
}

.getUsagiSummaryForVocabulary <- function(
    pathToVocabularyFolder,
    sourceVocabularyId) {
  vocabulariesTibble <- file.path(pathToVocabularyFolder, "vocabularies.csv") |>
    readr::read_csv()

  usagiFilePath <- vocabulariesTibble |>
    dplyr::filter(source_vocabulary_id == sourceVocabularyId)  |>
    dplyr::pull(path_to_usagi_file)

  usagiTibble <- readUsagiFile(file.path(pathToVocabularyFolder, usagiFilePath))

  if (!("ADD_INFO:validationMessages" %in% colnames(usagiTibble))) {
    usagiTibble <- usagiTibble |>
      dplyr::mutate(`ADD_INFO:validationMessages` = "")
  }

  if(  !("ADD_INFO:autoUpdatingInfo" %in% colnames(usagiTibble))) {
    usagiTibble <- usagiTibble |>
      dplyr::mutate(`ADD_INFO:autoUpdatingInfo` = "")
  }

  usagiSummaryTibble <- usagiTibble |>
    dplyr::select(
      source_concept_id = `ADD_INFO:sourceConceptId`,
      target_concept_id = conceptId,
      statusSetBy,
      statusSetOn,
      mappingStatus,
      equivalence,
      validationMessages = `ADD_INFO:validationMessages`,
      autoUpdatingInfo = `ADD_INFO:autoUpdatingInfo`
    )

  return(usagiSummaryTibble)
}

