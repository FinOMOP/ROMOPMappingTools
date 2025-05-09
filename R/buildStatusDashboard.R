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
    summaryTableForVocabularyAndDatabaseList,
    usagiTibble,
    sourceVocabularyId) {
  inputTemplate <- system.file("reports", "pageVocabulary.Rmd", package = "ROMOPMappingTools")
  cleanSourceVocabularyId <- sourceVocabularyId |> stringr::str_replace_all("[^[:alnum:]]", "")
  outputFileHtmlPath <- file.path(tempdir(), paste0(cleanSourceVocabularyId, ".html"))

  # TEMP: the result='asis' is not working for the for loop, make here
  # 1. Read the template file
  template_content <- readLines(inputTemplate)

  # 2. Define your code block as a string (beAdded)
  for (database in names(summaryTableForVocabularyAndDatabaseList)) {
    template_content <- template_content |>
      append(database) |>
      append(("=====================================\n")) |>
      append(("```{r}\n")) |>
      append(paste0("summaryTableForVocabularyAndDatabase <- summaryTableForVocabularyAndDatabaseList[['", database, "']]\n")) |>
      append(("```\n")) |>
      append(("Column {.tabset}")) |>
      append(("-----------------------------------------------------------------------\n")) |>
      append(("### Summary\n")) |>
      append(("```{r}\n")) |>
      append((".plotSummaryTableForVocabularyAndDatabase(summaryTableForVocabularyAndDatabase)\n")) |>
      append(("```\n")) |>
      append(("### Coverage\n")) |>
      append(("```{r}\n")) |>
      append((".plotTableForVocabularyAndDatabase(summaryTableForVocabularyAndDatabase)\n")) |>
      append(("```\n"))
  }

  # 4. Write the modified template to a new file
  tempTemplatePath <- file.path(tempdir(), paste0(cleanSourceVocabularyId, ".Rmd"))
  writeLines(template_content, tempTemplatePath)

  rmarkdown::render(
    input = tempTemplatePath,
    params = list(
      summaryTableForVocabularyAndDatabaseList = summaryTableForVocabularyAndDatabaseList,
      usagiTibble = usagiTibble,
      sourceVocabularyId = sourceVocabularyId
    ),
    output_file = outputFileHtmlPath
  )

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


.plotTableForUsagiFile <- function(
    usagiTibble,
    colors = list(
      flagged = "#EC6173",
      unchecked = "#F1AE4A",
      approved = "#51A350",
      inexact = "#AAAAAA"
    )) {
  toPlot <- usagiTibble |>
    dplyr::mutate(
      statusColor = dplyr::case_when(
        mappingStatus == "FLAGGED" ~ colors$flagged,
        mappingStatus == "UNCHECKED" ~ colors$unchecked,
        mappingStatus == "APPROVED" ~ colors$approved,
        mappingStatus == "INEXACT" ~ colors$inexact
      )
    )

  # Always include required columns
  columns <- list(
    mappingStatus = reactable::colDef(
      name = "Mapping Status",
      maxWidth = 120,
      html = TRUE,
      cell = function(value, index) {
        statusColor <- toPlot$statusColor[index]
        paste0("<span style='color: ", statusColor, ";'>", value, "</span>")
      }
    ),
    statusColor = reactable::colDef(show = FALSE)
  )

  reactable::reactable(
    toPlot,
    columns = columns,
    resizable = TRUE,
    filterable = TRUE,
    defaultPageSize = 10
  )
}

.plotSummaryTableForUsagiFile <- function(
    usagiTibble,
    colors = list(
      flagged = "#EC6173",
      unchecked = "#F1AE4A",
      approved = "#51A350",
      inexact = "#AAAAAA"
    )) {
  toPlot <- usagiTibble |>
    dplyr::group_by(mappingStatus, statusSetBy) |>
    dplyr::summarise(
      n = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      p = n / sum(n) * 100,
      statusColor = dplyr::case_when(
        mappingStatus == "FLAGGED" ~ colors$flagged,
        mappingStatus == "UNCHECKED" ~ colors$unchecked,
        mappingStatus == "APPROVED" ~ colors$approved,
        mappingStatus == "INEXACT" ~ colors$inexact
      )
    ) |>
    dplyr::arrange(dplyr::desc(n))
  columns <- list(
    mappingStatus = reactable::colDef(
      name = "Mapping Status",
      maxWidth = 120,
      html = TRUE,
      cell = function(value, index) {
        statusColor <- toPlot$statusColor[index]
        paste0("<span style='color: ", statusColor, ";'>", value, "</span>")
      }
    ),
    statusColor = reactable::colDef(show = FALSE),
    statusSetBy = reactable::colDef(
      name = "Status Set By",
      maxWidth = 120
    ),
    n = reactable::colDef(
      name = "N Codes",
      maxWidth = 100
    ),
    p = reactable::colDef(
      name = "% Codes",
      maxWidth = 100,
      format = reactable::colFormat(digits = 2, suffix = "%")
    )
  )

  reactable::reactable(
    toPlot,
    columns = columns,
    resizable = TRUE,
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

  # # get children
  # sql <- "
  #   SELECT
  #     c.concept_id,
  #     crp.child_concept_id
  #   FROM (
  #     SELECT * FROM @vocabulary_database_schema.CONCEPT
  #     WHERE vocabulary_id IN (@list_vocabulary_ids)
  #   ) AS c
  #   LEFT JOIN (
  #     SELECT
  #         concept_id_1 AS concept_id,
  #         concept_id_2 AS child_concept_id
  #     FROM @vocabulary_database_schema.CONCEPT_RELATIONSHIP
  #     WHERE relationship_id = 'Subsumes'
  #   ) AS crp
  #   ON c.concept_id = crp.concept_id
  # "

  # sql <- SqlRender::render(
  #   sql = sql,
  #   vocabulary_database_schema = vocabularyDatabaseSchema,
  #   list_vocabulary_ids = paste0("'", targetVocabularyIds, "'") |> paste0(collapse = ", ")
  # )
  # sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  # conceptChild <- DatabaseConnector::dbGetQuery(connection, sql) |>
  #   tibble::as_tibble() |>
  #   dplyr::rename_with(SqlRender::snakeCaseToCamelCase)

  # # merge
  # databaseSummary <- databaseSummary |>
  #   dplyr::left_join(conceptParent, by = c("sourceConceptId" = "conceptId")) |>
  #   dplyr::left_join(conceptChild, by = c("sourceConceptId" = "conceptId"))

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

# get_full_family_tree_with_distance_recursive <- function(relations, conceptId) {
#   # General recursive helper
#   traverse_tree <- function(relations, node, dist, visited, from_col, to_col, dist_step) {
#     if (node %in% visited) return(tibble::tibble(parentConceptId = numeric(), childConceptId = numeric(), distance = integer()))
#     next_nodes <- relations %>%
#       dplyr::filter(.data[[from_col]] == node) %>%
#       dplyr::pull(.data[[to_col]])
#     if (length(next_nodes) == 0) return(tibble::tibble(parentConceptId = numeric(), childConceptId = numeric(), distance = integer()))
#     if (from_col == "conceptId") {
#       # Downward: parent -> child
#       result <- tibble::tibble(parentConceptId = node, childConceptId = next_nodes, distance = dist)
#     } else {
#       # Upward: parent <- child
#       result <- tibble::tibble(parentConceptId = next_nodes, childConceptId = node, distance = dist)
#     }
#     for (n in next_nodes) {
#       result <- dplyr::bind_rows(result, traverse_tree(relations, n, dist + dist_step, c(visited, node), from_col, to_col, dist_step))
#     }
#     return(result)
#   }

#   # Upward (parents): from childConceptId to conceptId, distance -1
#   result_up <- traverse_tree(relations, conceptId, -1, c(), "childConceptId", "conceptId", -1)
#   # Downward (children): from conceptId to childConceptId, distance +1
#   result_down <- traverse_tree(relations, conceptId, 1, c(), "conceptId", "childConceptId", 1)

#   # Combine
#   result <- dplyr::bind_rows(result_up, result_down)
#   return(result)
# }


# family_tree  <- get_full_family_tree_with_distance_recursive(conceptChild, code)

# #' Create a Mermaid plot from a family tree tibble
# #'
# #' @param family_tree A tibble with columns parentConceptId, childConceptId, and distance
# #' @return A string containing the Mermaid diagram code
# family_tree_to_mermaid <- function(family_tree, info, conceptId) {

#   # Start the Mermaid graph definition
#   mermaid_lines <- c("graph TD")

#   # For each row, add an edge from parent to child
#   for (i in seq_len(nrow(family_tree))) {
#     parent <- family_tree$parentConceptId[i]
#     child <- family_tree$childConceptId[i]
#     mermaid_lines <- c(mermaid_lines, sprintf("  %s --> %s", parent, child))
#   }

#   # Combine into a single string
#   mermaid_code <- paste(mermaid_lines, collapse = "\n")

#   # Add info to the graph
#   conceptIds  <- c(family_tree$parentConceptId, family_tree$childConceptId) |> unique()
#   info <- databaseSummary |>
#     dplyr::filter(sourceConceptId %in% conceptIds) |>
#     dplyr::select(sourceConceptId,sourceCode, sourceConceptName, sourceVocabularyId) |>
#     dplyr::distinct() |>
#     dplyr::mutate(
#       label = dplyr::if_else(
#         sourceConceptId == {{ conceptId }},
#         paste0(sourceConceptId, "{{", sourceCode,  "<br>", sourceVocabularyId, "}}"),
#         paste0(sourceConceptId, "[", sourceCode,  "<br>", sourceVocabularyId, "]")
#       )
#     )

#   info_labels <- info |> dplyr::pull(label) |> paste0(collapse = "\n")

#   mermaid_code <- paste0(mermaid_code, "\n", info_labels)

#   return(mermaid_code)
# }

# code <- 45533778

# family_tree  <- get_full_family_tree_with_distance_recursive(conceptChild, code)


# mermaid_code <- family_tree_to_mermaid(family_tree, databaseSummary, code)

# mermaid_code |> cat()


# # Install required packages
# install.packages("jsonlite")
# install.packages("base64enc")

# library(jsonlite)
# library(base64enc)

# # Define your Mermaid diagram
# mermaid_code <- "
# graph TD
#   A[Start] --> B{Decision}
#   B -->|Yes| C[Do something]
#   B -->|No| D[Do something else]
#   C --> E[End]
#   D --> E
# "

# # Compress using raw DEFLATE
# compressed <- memCompress(charToRaw(mermaid_code), type = "gzip")

# # Drop the GZIP header/footer (only use raw DEFLATE)
# # Header is 10 bytes, footer is 8 bytes
# deflate_raw <- compressed[11:(length(compressed) - 8)]

# # Base64 encode
# b64 <- base64encode(deflate_raw)

# # Make URL-safe (Base64url encoding)
# b64url <- chartr("+/", "-_", gsub("=+$", "", b64))

# # Create final URL
# url <- paste0("https://mermaid.live/edit#pako:", b64url)

# # Output the URL
# cat("Open this URL:\n", url, "\n")




# # Install required package
# install.packages("V8")

# library(V8)
# ctx <- v8()

# # Load pako.js (needed for deflate)
# ctx$source("https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js")

# # Mermaid code
# mermaid_code <- "
# graph TD
#   A[Start] --> B{Decision}
#   B -->|Yes| C[Do something]
#   B -->|No| D[Do something else]
#   C --> E[End]
#   D --> E
# "

# # Define JS to compress and base64 encode in URL-safe format
# js_code <- sprintf("
#   const input = `%s`;
#   const compressed = pako.deflate(input, { level: 9 });
#   const base64 = btoa(String.fromCharCode.apply(null, compressed));
#   const urlSafe = base64.replace(/\\+/g, '-').replace(/\\//g, '_').replace(/=+$/, '');
#   urlSafe;
# ", gsub("`", "\\`", mermaid_code))  # Escape backticks

# # Run JS and get result
# compressed_encoded <- ctx$eval(js_code)

# # Final Mermaid Live URL
# url <- paste0("https://mermaid.live/edit#pako:", compressed_encoded)
# cat("Mermaid Live Editor URL:\n", url, "\n")
