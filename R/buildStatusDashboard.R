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

  databaseSummary <- DatabaseConnector::dbGetQuery(connection, sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(SqlRender::snakeCaseToCamelCase)

  # get children
  sql <- "
    SELECT
      c.concept_id,
      crp.child_concept_id
    FROM (
      SELECT * FROM @vocabulary_database_schema.CONCEPT
      WHERE vocabulary_id IN (@list_vocabulary_ids)
    ) AS c
    LEFT JOIN (
      SELECT
          concept_id_1 AS child_concept_id,
          concept_id_2 AS concept_id
      FROM @vocabulary_database_schema.CONCEPT_RELATIONSHIP
      WHERE relationship_id = 'Subsumes'
    ) AS crp
    ON c.concept_id = crp.concept_id
  "

  sql <- SqlRender::render(
    sql = sql,
    vocabulary_database_schema = vocabularyDatabaseSchema,
    list_vocabulary_ids = paste0("'", targetVocabularyIds, "'") |> paste0(collapse = ", ")
  )
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  conceptChild <- DatabaseConnector::dbGetQuery(connection, sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(SqlRender::snakeCaseToCamelCase)

    


  # merge
  databaseSummary <- databaseSummary |>
    dplyr::left_join(conceptParent, by = c("sourceConceptId" = "conceptId")) |>
    dplyr::left_join(conceptChild, by = c("sourceConceptId" = "conceptId"))

  DatabaseConnector::disconnect(connection)

  return(databaseSummary)
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

get_full_family_tree_with_distance <- function(relations, conceptId) {
  # relations: a tibble with columns conceptId (parent) and childConceptId (child)
  # conceptId: the root conceptId to start from

  # Downward (children)
  result <- tibble::tibble(
    parentConceptId = numeric(),
    childConceptId = numeric(),
    distance = integer()
  )
  queue <- list(list(id = conceptId, dist = 0))
  visited <- c()
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    if (current$id %in% visited) next
    visited <- c(visited, current$id)
    children <- relations %>%
      dplyr::filter(conceptId == current$id) %>%
      dplyr::pull(childConceptId)
    if (length(children) > 0) {
      result <- dplyr::bind_rows(
        result,
        tibble::tibble(
          parentConceptId = current$id,
          childConceptId = children,
          distance = current$dist + 1
        )
      )
      queue <- c(queue, lapply(children, function(child) list(id = child, dist = current$dist + 1)))
    }
  }

  # Upward (parents)
  result_up <- tibble::tibble(
    parentConceptId = numeric(),
    childConceptId = numeric(),
    distance = integer()
  )
  queue <- list(list(id = conceptId, dist = 0))
  visited <- c()
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    if (current$id %in% visited) next
    visited <- c(visited, current$id)
    parents <- relations %>%
      dplyr::filter(childConceptId == current$id) %>%
      dplyr::pull(conceptId)
    if (length(parents) > 0) {
      result_up <- dplyr::bind_rows(
        result_up,
        tibble::tibble(
          parentConceptId = parents,
          childConceptId = current$id,
          distance = current$dist - 1
        )
      )
      queue <- c(queue, lapply(parents, function(parent) list(id = parent, dist = current$dist - 1)))
    }
  }

  # Combine, and add the root node itself
  result <- dplyr::bind_rows(
    result_up,
    result,
    tibble::tibble(
      parentConceptId = NA,
      childConceptId = conceptId,
      distance = 0
    )
  )
  return(result)
}




#' Create a Mermaid plot from a family tree tibble
#'
#' @param family_tree A tibble with columns parentConceptId, childConceptId, and distance
#' @return A string containing the Mermaid diagram code
family_tree_to_mermaid <- function(family_tree, info, conceptId) {

  # Start the Mermaid graph definition
  mermaid_lines <- c("graph TD")

  # For each row, add an edge from parent to child
  for (i in seq_len(nrow(family_tree))) {
    parent <- family_tree$parentConceptId[i]
    child <- family_tree$childConceptId[i]
    mermaid_lines <- c(mermaid_lines, sprintf("  %s --> %s", parent, child))
  }

  # Combine into a single string
  mermaid_code <- paste(mermaid_lines, collapse = "\n")

  # Add info to the graph
  conceptIds  <- c(family_tree$parentConceptId, family_tree$childConceptId) |> unique()
  info <- databaseSummary |> 
    dplyr::filter(sourceConceptId %in% conceptIds) |> 
    dplyr::select(sourceConceptId,sourceCode, sourceConceptName, sourceVocabularyId) |> 
    dplyr::distinct() |> 
    dplyr::mutate(
      label = dplyr::if_else(
        sourceConceptId == {{ conceptId }},
        paste0(sourceConceptId, "{{", sourceCode,  "<br>", sourceVocabularyId, "}}"),
        paste0(sourceConceptId, "[", sourceCode,  "<br>", sourceVocabularyId, "]")
      )
    )

  info_labels <- info |> dplyr::pull(label) |> paste0(collapse = "\n") 

  mermaid_code <- paste0(mermaid_code, "\n", info_labels)

  return(mermaid_code)
}

code <- 45533778

family_tree  <- get_full_family_tree_with_distance(conceptChild, code)
family_tree  <- family_tree |>
dplyr::filter(!is.na(childConceptId) & !is.na(parentConceptId)) |>
dplyr::mutate(distance = -distance) 

mermaid_code <- family_tree_to_mermaid(family_tree, databaseSummary, code)

mermaid_code |> cat()
