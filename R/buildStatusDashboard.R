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
    outputFileHtmlPath = file.path(tempdir(), "MappingStatusDashboard.html")) {
  #
  # Validate parameters
  #

  pathToCodeCountsFolder |> checkmate::assert_directory_exists()
  connectionDetails |> checkmate::assert_class(c("ConnectionDetails"))
  vocabularyDatabaseSchema |> checkmate::assert_character()
  outputFileHtmlPath |> checkmate::assert_character()


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
    output_file = outputFileHtmlPath
  )


  return(validationLogTibble)
}



.pageCoverageVocabularyDatabase <- function(
    summaryTableForVocabularyAndDatabase,
    sourceVocabularyId,
    databaseName) {
  inputTemplate <- system.file("reports", "pageCoverageVocabularyDatabase.Rmd", package = "ROMOPMappingTools")
  outputFileHtmlPath <- file.path(tempdir(), "pageCoverageVocabularyDatabase.html")

  rmarkdown::render(
    input = inputTemplate,
    params = list(
      summaryTableForVocabularyAndDatabase = summaryTableForVocabularyAndDatabase,
      sourceVocabularyId = sourceVocabularyId,
      databaseName = databaseName
    ),
    output_file = outputFileHtmlPath
  )


  modalWithMermaidPath <- system.file("reports", "modalWithMermaid.html", package = "ROMOPMappingTools")  
  modalWithMermaid <- readChar(modalWithMermaidPath, file.info(modalWithMermaidPath)$size)
  outputFileHtml <- readChar(outputFileHtmlPath, file.info(outputFileHtmlPath)$size)
  outputFileHtml <- outputFileHtml |>
    stringr::str_remove("</body>") |>
    stringr::str_remove("</html>") |>
    paste0(modalWithMermaid) |>
    paste0("</body>") |>
    paste0("</html>")

  writeLines(outputFileHtml, outputFileHtmlPath)

  return(outputFileHtmlPath)
}

.plotSummaryTableForVocabularyAndDatabase <- function(
    summaryTableForVocabularyAndDatabase,
    colors = list(
      invalid = "#EC6173",
      unmapped = "#F1AE4A",
      mapsTo = "#51A350",
      grey = "#AAAAAA"
    )) {
  toPlot <- summaryTableForVocabularyAndDatabase |>
    dplyr::mutate(
      statusColor = dplyr::case_when(
        status == "INVALID" ~ colors$invalid,
        status == "UNMAPPED" ~ colors$unmapped,
        status == "MAPS TO" ~ colors$mapsTo
      ),
    ) |>
    dplyr::group_by(sourceCode) |>
    dplyr::mutate(
      nMapsTo = dplyr::n_distinct(targetConceptId)
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(sourceCode, .keep_all = TRUE) |>
    dplyr::mutate(
      status = dplyr::if_else(status == "MAPS TO", paste0(status, " ", nMapsTo), status)
    ) |>
    dplyr::group_by(status, statusColor) |>
    dplyr::summarize(
      n = dplyr::n(),
      nEvents = sum(nEvents, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pEvents = nEvents / sum(nEvents) * 100,
      order = dplyr::case_when(
        stringr::str_detect(status, "MAPS TO") ~ as.numeric(stringr::str_extract(status, "\\d+$")),
        stringr::str_detect(status, "UNMAPPED") ~ 200,
        stringr::str_detect(status, "INVALID") ~ 300,
        TRUE ~ 0
      )
    ) |>
    dplyr::arrange(order) |>
    dplyr::select(order, status, statusColor, n, nEvents, pEvents)

  columns <- list(
    # Status
    order = reactable::colDef(
      name = "Status",
      maxWidth = 150,
      html = TRUE,
      cell = function(value, index) {
        status <- toPlot$status[index]
        statusColor <- toPlot$statusColor[index]
        paste0("<span style='color: ", statusColor, ";'>", status, "</span>")
      }
    ),
    status = reactable::colDef(
      show = FALSE
    ),
    statusColor = reactable::colDef(
      show = FALSE
    ),
    # N codes
    n = reactable::colDef(
      name = "N codes",
      maxWidth = 100
    ),
    nEvents = reactable::colDef(
      name = "N Events",
    ),
    pEvents = reactable::colDef(
      name = "% Events",
      format = reactable::colFormat(digits = 2, suffix = "%")
    )
  )

  toPlot |> reactable::reactable(
    columns = columns,
    resizable = TRUE,
    sortable = TRUE,
    defaultPageSize = 10
  )
}


.plotTableForVocabularyAndDatabase <- function(
    summaryTableForVocabularyAndDatabase,
    colors = list(
      invalid = "#EC6173",
      unmapped = "#F1AE4A",
      mapsTo = "#51A350",
      grey = "#AAAAAA"
    )) {
  athenaUrl <- "https://athena.ohdsi.org/search-terms/terms/"

  toPlot <- summaryTableForVocabularyAndDatabase |>
    dplyr::mutate(
      nEvents = dplyr::if_else(is.na(nEvents) | nEvents < 0, 0, nEvents),
      pEvents = nEvents / sum(nEvents) * 100,
      statusColor = dplyr::case_when(
        status == "INVALID" ~ colors$invalid,
        status == "UNMAPPED" ~ colors$unmapped,
        status == "MAPS TO" ~ colors$mapsTo
      ),
    ) |>
    dplyr::group_by(sourceCode) |>
    dplyr::mutate(
      nMapsTo = dplyr::n_distinct(targetConceptId)
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      sourceCode = sourceCode,
      sourceVocabularyId = sourceVocabularyId,
      sourceName = sourceConceptName,
      status = dplyr::if_else(status == "MAPS TO", paste0(status, " ", nMapsTo), status),
      statusColor = statusColor,
      targetCode = targetCode,
      equivalence = equivalence,
      targetVocabularyId = targetVocabularyId,
      targetName = dplyr::if_else(!is.na(targetConceptName), paste0("<a href='", athenaUrl, targetConceptId, "' target='_blank'>", targetConceptName, "</a>"), ""),
      nEvents = nEvents,
      pEvents = pEvents,
      statusSetBy = statusSetBy,
      #
      mermaidPlot = "graph TD<br>  A[Start] --> B{Is it working?}<br>  B -- Yes --> C[Great!]<br>  B -- No --> D[Check again]"
    ) |>
    dplyr::arrange(dplyr::desc(nEvents))

  columns <- list(
    # Source Code
    sourceCode = reactable::colDef(
      name = paste0("Source Code<br><span style='color:", colors$grey, "'>(Vocabulary)</span>"),
      maxWidth = 120,
      html = TRUE,
      cell = function(value, index) {
        sourceCode <- toPlot$sourceCode[index]
        sourceVocabularyId <- toPlot$sourceVocabularyId[index]
        paste0(sourceCode, " <br><span style='color:", colors$grey, "'> (", sourceVocabularyId, ")</span>")
      }
    ),
    sourceVocabularyId = reactable::colDef(show = FALSE),
    # Source Name
    sourceName = reactable::colDef(
      name = "Source Name",
    ),
    # Status
    status = reactable::colDef(
      name = paste0("Status<br><span style='color:", colors$grey, "'>(Equivalence)</span>"),
      maxWidth = 150,
      html = TRUE,
      cell = function(value, index) {
        status <- toPlot$status[index]
        statusColor <- toPlot$statusColor[index]
        equivalence <- toPlot$equivalence[index]
        toPrint <- paste0("<span style='color: ", statusColor, ";'>", status, "</span>")
        if (!is.na(equivalence)) {
          toPrint <- paste0(toPrint, "<br><span style='color:", colors$grey, "'> (", equivalence, ")</span>")
        }
        toPrint
      }
    ),
    statusColor = reactable::colDef(
      show = FALSE
    ),
    equivalence = reactable::colDef(
      show = FALSE
    ),
    # Target Code
    targetCode = reactable::colDef(
      name = paste0("Target Code<br><span style='color:", colors$grey, "'>(Vocabulary)</span>"),
      maxWidth = 150,
      html = TRUE,
      cell = function(value, index) {
        targetCode <- toPlot$targetCode[index]
        targetVocabularyId <- toPlot$targetVocabularyId[index]
        if (is.na(targetCode)) {
          return("")
        }
        paste0(targetCode, " <br><span style='color:", colors$grey, "'> (", targetVocabularyId, ")</span>")
      }
    ),
    targetVocabularyId = reactable::colDef(
      show = FALSE
    ),
    # Target Name
    targetName = reactable::colDef(
      name = "Target Name",
      html = TRUE
    ),
    # N Events
    nEvents = reactable::colDef(
      name = paste0("N Events<br><span style='color:", colors$grey, "'>(%)</span>"),
      maxWidth = 100,
      html = TRUE,
      cell = function(value, index) {
        nEvents <- toPlot$nEvents[index]
        pEvents <- toPlot$pEvents[index]
        paste0(nEvents, " <br><span style='color:", colors$grey, "'> (", round(pEvents, 2), "%)</span>")
      }
    ),
    pEvents = reactable::colDef(
      show = FALSE
    ),
    # Status Set By
    statusSetBy = reactable::colDef(
      name = "Status Set By",
      maxWidth = 200,
    ),
    # Mermaid Plot
    mermaidPlot = reactable::colDef(
      name = "Mermaid Plot",
      html = TRUE,
      cell = function(value, index) {
        btn_id <- paste0("show-mermaid-", index)
        # Escape the Mermaid code for JS
        mermaid_code <- gsub("'", "\\\\'", toPlot$mermaidPlot[index])
        sprintf(
          "<a href='#' id='%s' onclick=\"showMermaidModal('%s')\">Show Mermaid Plot</a>",
          btn_id, mermaid_code
        )
      }
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
    databaseName) {
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
    dplyr::full_join(codeCounts |> dplyr::select(-sourceVocabularyId), by = c("sourceCode" = "sourceCode")) |>
    dplyr::left_join(usagiSummary, by = c("sourceConceptId" = "sourceConceptId", "targetConceptId" = "targetConceptId")) |>
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(sourceConceptId) ~ "INVALID",
        is.na(targetConceptId) ~ "UNMAPPED",
        TRUE ~ "MAPS TO"
      )
    )

  return(summaryTableForVocabularyAndDatabase)
}


.getCodeCountsForVocabularyAndDatabase <- function(
    pathToCodeCountsFolder,
    sourceVocabularyId,
    databaseName) {
  databaseCoverageTibble <- file.path(pathToCodeCountsFolder, "databases_coverage.csv") |>
    readr::read_csv()

  databaseCoverageTibble <- databaseCoverageTibble |>
    dplyr::filter(database_name == databaseName)

  codeCountsTibble <- file.path(pathToCodeCountsFolder, databaseCoverageTibble |> dplyr::pull(path_to_code_counts_file)) |>
    readr::read_csv() |>
    dplyr::rename_with(SqlRender::snakeCaseToCamelCase)

  codeCountsTibble <- codeCountsTibble |>
    dplyr::filter(sourceVocabularyId == {{ sourceVocabularyId }})

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
    targetVocabularyIds) {
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

  vocabulariesDBInfo <- DatabaseConnector::dbGetQuery(connection, sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(SqlRender::snakeCaseToCamelCase)

  DatabaseConnector::disconnect(connection)

  return(vocabulariesDBInfo)
}

.getUsagiSummaryForVocabulary <- function(
    pathToVocabularyFolder,
    sourceVocabularyId) {
  vocabulariesTibble <- file.path(pathToVocabularyFolder, "vocabularies.csv") |>
    readr::read_csv()

  usagiFilePath <- vocabulariesTibble |>
    dplyr::filter(source_vocabulary_id == sourceVocabularyId) |>
    dplyr::pull(path_to_usagi_file)

  usagiTibble <- readUsagiFile(file.path(pathToVocabularyFolder, usagiFilePath))

  if (!("ADD_INFO:validationMessages" %in% colnames(usagiTibble))) {
    usagiTibble <- usagiTibble |>
      dplyr::mutate(`ADD_INFO:validationMessages` = "")
  }

  if (!("ADD_INFO:autoUpdatingInfo" %in% colnames(usagiTibble))) {
    usagiTibble <- usagiTibble |>
      dplyr::mutate(`ADD_INFO:autoUpdatingInfo` = "")
  }

  usagiSummaryTibble <- usagiTibble |>
    dplyr::select(
      sourceConceptId = `ADD_INFO:sourceConceptId`,
      targetConceptId = conceptId,
      statusSetBy,
      statusSetOn,
      mappingStatus,
      equivalence,
      validationMessages = `ADD_INFO:validationMessages`,
      autoUpdatingInfo = `ADD_INFO:autoUpdatingInfo`
    )

  return(usagiSummaryTibble)
}
