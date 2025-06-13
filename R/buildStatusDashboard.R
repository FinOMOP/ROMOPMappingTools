#' Build Status Dashboard
#'
#' This function generates a status dashboard for mapping status.
#'
#' @param pathToCodeCountsFolder Path to folder containing code counts files
#' @param pathToVocabularyFolder Path to folder containing vocabulary files
#' @param connectionDetails DatabaseConnector connection details object
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables
#' @param outputFolderPath The path to the output folder for HTML output
#' @param fileIssueRepo The repository to file issues to
#'
#' @return Path to the output HTML file
#'
#' @importFrom checkmate assert_directory_exists assert_class assert_character
#' @importFrom readr read_csv
#' @importFrom dplyr filter distinct bind_rows select mutate left_join full_join rename_with case_when group_by ungroup summarize transmute pull arrange if_else n_distinct
#' @importFrom tibble tibble as_tibble
#' @importFrom reactable reactable colDef colFormat
#' @importFrom shiny div
#' @importFrom stringr str_split str_c str_detect str_extract
#' @importFrom scales number percent cut_si
#' @importFrom tippy tippy
#' @importFrom SqlRender snakeCaseToCamelCase render translate
#' @importFrom DatabaseConnector connect disconnect dbGetQuery
#' @export
buildStatusDashboard <- function(
    pathToCodeCountsFolder,
    pathToVocabularyFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    outputFolderPath = tempdir(),
    fileIssueRepo = "") {
  #
  # Validate parameters
  #

  pathToCodeCountsFolder |> checkmate::assert_directory_exists()
  pathToVocabularyFolder |> checkmate::assert_directory_exists()
  connectionDetails |> checkmate::assert_class(c("ConnectionDetails"))
  vocabularyDatabaseSchema |> checkmate::assert_character()
  if (!dir.exists(outputFolderPath)) {
    dir.create(outputFolderPath, recursive = TRUE)
  }


  #
  # Function
  #
  vocabulariesCoverageTibble <- readr::read_csv(pathToCodeCountsFolder |> file.path("vocabularies_coverage.csv"), show_col_types = FALSE)
  vocabulariesCoverageTibble <- vocabulariesCoverageTibble |>
    dplyr::filter(!ignore)

  vocabulariesTibble <- readr::read_csv(pathToVocabularyFolder |> file.path("vocabularies.csv"), show_col_types = FALSE)

  summaryAllVocabularies <- tibble::tibble()
  for (i in 1:nrow(vocabulariesCoverageTibble)) {
    message("Processing vocabulary: ", vocabulariesCoverageTibble$source_vocabulary_id[i])

    sourceVocabularyId <- vocabulariesCoverageTibble$source_vocabulary_id[i]
    targetVocabularyIds <- vocabulariesCoverageTibble$target_vocabulary_ids[i]
    targetVocabularyIds <- stringr::str_split(targetVocabularyIds, "\\|") |> unlist()
    mantainedBy <- vocabulariesCoverageTibble$mantained_by[i]

    vocabularyTibble <- vocabulariesTibble |>
      dplyr::filter(source_vocabulary_id == sourceVocabularyId)

    # get usagi tibble and make summary string
    usagiTibble <- NULL
    pathToNewsFile <- NULL
    strMapped <- NA_character_
    if (nrow(vocabularyTibble) != 0) {
      usagiTibble <- readUsagiFile(file.path(pathToVocabularyFolder, vocabularyTibble$path_to_usagi_file[1]))
      pathToNewsFile <- file.path(pathToVocabularyFolder, vocabularyTibble$path_to_news_file[1])
      nMapped <- usagiTibble |>
        dplyr::distinct(sourceCode, .keep_all = TRUE) |>
        dplyr::filter(mappingStatus == "APPROVED") |>
        nrow()
      nUnmapped <- usagiTibble |>
        dplyr::distinct(sourceCode, .keep_all = TRUE) |>
        dplyr::filter(mappingStatus != "APPROVED") |>
        nrow()
      strMapped <- paste0(nMapped, "-", nUnmapped)
    }

    # get summary table for all databases
    summaryTableForVocabularyAndDatabaseList <- .getSummaryListForVocabulary(
      pathToCodeCountsFolder = pathToCodeCountsFolder,
      pathToVocabularyFolder = pathToVocabularyFolder,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      sourceVocabularyId = sourceVocabularyId,
      targetVocabularyIds = targetVocabularyIds
    )

    # build page for coverage of all databases
    .pageCoverageVocabularyDatabases(
      summaryTableForVocabularyAndDatabaseList = summaryTableForVocabularyAndDatabaseList,
      usagiTibble = usagiTibble,
      sourceVocabularyId = sourceVocabularyId,
      outputFolderPath = outputFolderPath,
      pathToNewsFile = pathToNewsFile,
      fileIssueRepo = fileIssueRepo
    )

    # create summary for all databases
    summaryVocabulary <- tibble::tibble(
      vocabularyId = sourceVocabularyId,
      mantainedBy = mantainedBy,
      strMapped = strMapped
    )

    for (databaseName in names(summaryTableForVocabularyAndDatabaseList)) {
      message("Processing database: ", databaseName)

      summaryTableForVocabularyAndDatabase <- summaryTableForVocabularyAndDatabaseList[[databaseName]]

      nMapped <- summaryTableForVocabularyAndDatabase |>
        dplyr::distinct(sourceCode, .keep_all = TRUE) |>
        dplyr::filter(status == "MAPS TO") |>
        dplyr::pull(nEvents) |>
        sum(na.rm = TRUE)

      nUnmapped <- summaryTableForVocabularyAndDatabase |>
        dplyr::distinct(sourceCode, .keep_all = TRUE) |>
        dplyr::filter(status == "UNMAPPED") |>
        dplyr::pull(nEvents) |>
        sum(na.rm = TRUE)

      nInvalid <- summaryTableForVocabularyAndDatabase |>
        dplyr::distinct(sourceCode, .keep_all = TRUE) |>
        dplyr::filter(status == "INVALID") |>
        dplyr::pull(nEvents) |>
        sum(na.rm = TRUE)

      strCoverage <- paste0(nMapped, "-", nUnmapped, "-", nInvalid)

      summaryVocabulary[[databaseName]] <- strCoverage
    }

    # add summary for all databases to summary for all vocabularies
    summaryAllVocabularies <- summaryAllVocabularies |>
      dplyr::bind_rows(summaryVocabulary)
  }

  outputFileHtmlPath <- .pageSummaryAllVocabularies(summaryAllVocabularies, outputFolderPath)

  return(outputFileHtmlPath)
}


#' Render summary page for all vocabularies
#'
#' @param summaryAllVocabularies Tibble with summary for all vocabularies
#' @param outputFolderPath Output folder path
#' @return Path to the output HTML file
.pageSummaryAllVocabularies <- function(
    summaryAllVocabularies,
    outputFolderPath) {
  inputTemplate <- system.file("reports", "pageAllVocabularies.Rmd", package = "ROMOPMappingTools")
  outputFileHtmlPath <- file.path(outputFolderPath, paste0("index.html"))

  rmarkdown::render(
    input = inputTemplate,
    params = list(summaryAllVocabularies = summaryAllVocabularies),
    output_file = outputFileHtmlPath
  )

  return(outputFileHtmlPath)
}

#' Plot summary table for all vocabularies
#'
#' @param summaryAllVocabularies Tibble with summary for all vocabularies
#' @param colors List of colors for bar chart
#' @return A reactable table object
#' @importFrom reactable reactable colDef
#' @importFrom shiny div
.plotTableSummaryAllVocabularies <- function(
    summaryAllVocabularies,
    colors = list(
      invalid = "#EC6173",
      unmapped = "#F1AE4A",
      mapsTo = "#51A350",
      grey = "#AAAAAA"
    )) {
  # build columns

  bar_colors <- c("#51A350", "#F1AE4A", "#EC6173")
  row_names <- c("standar code: ", "non-standar code: ", "not found code: ")
  unit_names <- rep(" events", 3)

  columns_list <- list(
    vocabularyId = reactable::colDef(
      name = "Vocabulary",
      maxWidth = 150,
      cell = function(value) {
        paste0("<a href='./", value, ".html' target='_blank'>", value, "</a>")
      },
      html = TRUE
    ),
    mantainedBy = reactable::colDef(name = "Maintained by"),
    strMapped = reactable::colDef(
      name = "Mapping progress",
      cell = function(value, index) {
        vocabularyId <- summaryAllVocabularies[index, "vocabularyId"]
        paste0(
          "<a href='./", vocabularyId, ".html#usagi-file' target='_blank' style='text-decoration: none;'>",
          .tip_reactable_cell(value, row_names = c("mapped: ", "no mapped: "), unit_names = rep(" codes", 2)),
          "</a>"
        )
      },
      style = function(value) {
        .background_bar_reactable_style(value, colors = c("#0000CC", "#E0E0E0"), height = "30%")
      },
      align = "left",
      html = TRUE
    )
  )

  databaseNames <- setdiff(names(summaryAllVocabularies), c("mantainedBy", "vocabularyId", "strMapped"))
  for (db_name in databaseNames) {
    columns_list[[db_name]] <- reactable::colDef(
      name = paste("Coverage", db_name),
      cell = function(value) {
        .tip_reactable_cell(value, row_names = row_names, unit_names = unit_names)
      },
      style = function(value) {
        .background_bar_reactable_style(value, colors = bar_colors)
      },
      align = "left"
    )
  }

  # columns_list[["all_databases"]] <- reactable::colDef(
  #   name = "TOTAL",
  #   cell = function(value) {
  #     .tip_reactable_cell(value, row_names = row_names, unit_names = unit_names, only_percent = TRUE)
  #   },
  #   style = function(value) {
  #     .background_bar_reactable_style(value, colors = bar_colors)
  #   },
  #   align = "left"
  # )


  # create table
  table <- summaryAllVocabularies |>
    reactable::reactable(
      pagination = FALSE,
      #
      columns = columns_list
    )

  return(table)
}




# Render a bar chart in the background of the cell
#' Render a bar chart in the background of the cell
#'
#' @param string_values String with values separated by '-'
#' @param colors Vector of colors
#' @param height Height of the bar
#' @param width Width of the bar
#' @param background_color Background color
#' @param text_color Text color
#' @return A list of CSS style properties
#' @importFrom stringr str_split
.background_bar_reactable_style <- function(
    string_values, colors = NULL,
    height = "60%", width = "95%",
    background_color = "#FFFFFF", text_color = "#FFFFFF") {
  if (is.na(string_values)) {
    return("")
  }
  values <- stringr::str_split(string_values, "-")[[1]] |> as.double()
  per_values <- values / sum(values) * 100
  per_values <- c(0, per_values)

  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(length(string_values), "Set3")
  }
  colors <- c(colors, background_color)

  gradient_str <- ""
  for (i in 1:(length(per_values) - 1)) {
    gradient_str <- paste0(
      gradient_str,
      colors[i], " ", per_values[i], "%, ",
      colors[i], " ", sum(per_values[1:(i + 1)]), "%, "
    )
  }
  gradient_str <- paste(gradient_str, background_color)

  list(
    backgroundImage = paste0("linear-gradient(to right, ", gradient_str, ")"),
    backgroundSize = paste(width, height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = text_color
  )
}


#' Tooltip for reactable cell
#'
#' @param string_values String with values separated by '-'
#' @param row_names Row names for tooltip
#' @param unit_names Unit names for tooltip
#' @param only_percent Logical, show only percent
#' @return A shiny div with tippy tooltip
#' @importFrom stringr str_split str_c
#' @importFrom shiny div
#' @importFrom tippy tippy
#' @importFrom scales number percent
.tip_reactable_cell <- function(
    string_values,
    row_names = NULL, unit_names = NULL, only_percent = FALSE) {
  if (is.na(string_values)) {
    return("")
  }
  values <- stringr::str_split(string_values, "-")[[1]] |> as.double()
  per_values <- values / sum(values)

  n_rows <- length(values)

  if (is.null(row_names)) {
    row_names <- rep("", n_rows)
  }

  if (is.null(unit_names)) {
    unit_names <- rep("", n_rows)
  }


  if (only_percent == FALSE) {
    text <- stringr::str_c(
      row_names,
      scales::number(values, scale_cut = scales::cut_si("")),
      unit_names,
      rep(" (", n_rows),
      scales::percent(per_values, accuracy = 0.01),
      rep(") ", n_rows)
    )
  } else {
    text <- stringr::str_c(
      row_names,
      scales::percent(per_values, accuracy = 0.01)
    )
  }

  text <- text |> stringr::str_c(collapse = "<br>")

  shiny::div(
    tippy::tippy(
      text = paste(rep("&nbsp;", 36), collapse = " "),
      tooltip = paste0("<span style='font-size:16px;'>", text, "<span>"),
      allowHTML = TRUE,
      theme = "light",
      arrow = TRUE
    )
  )
}


#' Render coverage page for a vocabulary and its databases
#'
#' @param summaryTableForVocabularyAndDatabaseList List of summary tables
#' @param usagiTibble Usagi tibble
#' @param sourceVocabularyId Source vocabulary ID
#' @param outputFolderPath Output folder path
#' @param pathToNewsFile Path to the NEWS.md file
#' @param fileIssueRepo The repository to file issues to
#' @return Path to the output HTML file
.pageCoverageVocabularyDatabases <- function(
    summaryTableForVocabularyAndDatabaseList,
    usagiTibble,
    sourceVocabularyId,
    outputFolderPath,
    pathToNewsFile,
    fileIssueRepo = "") {
  summaryTableForVocabularyAndDatabaseList |> checkmate::assert_list()
  usagiTibble |> checkmate::assert_tibble(null.ok = TRUE)
  sourceVocabularyId |> checkmate::assert_string()
  outputFolderPath |> checkmate::assert_directory()
  if (!is.null(pathToNewsFile)) {
    pathToNewsFile |> checkmate::assert_file_exists()
  }

  inputTemplate <- system.file("reports", "pageVocabulary.Rmd", package = "ROMOPMappingTools")
  outputFileHtmlPath <- file.path(outputFolderPath, paste0(sourceVocabularyId, ".html"))

  # TEMP: the result='asis' is not working for the for loop, make here
  # 1. Read the template file
  template_content <- readLines(inputTemplate)

  template_content[2] <- paste0("title: '", sourceVocabularyId, "'")

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
      append(paste0(".plotTableForVocabularyAndDatabase(summaryTableForVocabularyAndDatabase, databaseName = '", database, "', vocabularyId = '", sourceVocabularyId, "', fileIssueRepo = '", fileIssueRepo, "')\n")) |>
      append(("```\n"))
  }

  # 4. Write the modified template to a new file
  tempTemplatePath <- tempfile(fileext = ".Rmd")
  writeLines(template_content, tempTemplatePath)

  # read news file
  if (!is.null(pathToNewsFile)) {
    newsFile <- readLines(pathToNewsFile) |>
      paste0(collapse = "\n")
  } else {
    newsFile <- NULL
  }

  rmarkdown::render(
    input = tempTemplatePath,
    params = list(
      summaryTableForVocabularyAndDatabaseList = summaryTableForVocabularyAndDatabaseList,
      usagiTibble = usagiTibble,
      sourceVocabularyId = sourceVocabularyId,
      newsFile = newsFile,
      fileIssueRepo = fileIssueRepo
    ),
    output_file = outputFileHtmlPath
  )

  return(outputFileHtmlPath)
}

#' Plot summary table for a vocabulary and database
#'
#' @param summaryTableForVocabularyAndDatabase Tibble with summary
#' @param colors List of colors
#' @return A reactable table object
#' @importFrom reactable reactable colDef colFormat
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


#' Plot detailed table for a vocabulary and database
#'
#' @param summaryTableForVocabularyAndDatabase Tibble with summary
#' @param colors List of colors
#' @param fileIssueRepo The repository to file issues to
#' @return A reactable table object
#' @importFrom reactable reactable colDef colFormat
.plotTableForVocabularyAndDatabase <- function(
    summaryTableForVocabularyAndDatabase,
    databaseName,
    vocabularyId,
    colors = list(
      invalid = "#EC6173",
      unmapped = "#F1AE4A",
      mapsTo = "#51A350",
      grey = "#AAAAAA"
    ),
    fileIssueRepo = "") {
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
    # Create a string of target concept ids and names
    dplyr::group_by(sourceCode) |>
    dplyr::mutate(
      targetConceptIds_str = paste0(targetConceptId, collapse = " | "),
      targetConceptNames_str = paste0(targetConceptName, collapse = " | "),
      targetVocabularyIds_str = paste0(targetVocabularyId, collapse = " | ")
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
      targetConceptId = targetConceptId,
      targetName = targetConceptName,
      nEvents = nEvents,
      pEvents = pEvents,
      statusSetBy = statusSetBy,
      fileIssue = paste0(
        "<a href='",
        URLencode(paste0("https://github.com/", fileIssueRepo, "/issues/new?title=Issue with", sourceCode, " (", sourceVocabularyId, ")&body=Source code: ", sourceCode, "\nSource vocabulary: ", sourceVocabularyId, "\nSource concept name: ", sourceConceptName, "\nSource concept id: ", sourceConceptId, "\n\nStatus: ", status, "\n\nTarget concept ids: ", targetConceptIds_str, "\nTarget concept names: ", targetConceptNames_str, "\nTarget vocabulary: ", targetVocabularyIds_str, "\n\nPlease describe the issue hee: ")),
        "' target='_blank'>File issue</a>"
      )
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
    # Target Concept Id
    targetConceptId = reactable::colDef(
      show = FALSE
    ),
    # Target Name
    targetName = reactable::colDef(
      name = "Target Name",
      html = TRUE,
      cell = function(value, index){
        targetConceptId <- toPlot$targetConceptId[index]
        targetName <- toPlot$targetName[index]
        if (!is.na(targetConceptId)) {
          paste0("<a href='", athenaUrl, targetConceptId, "' target='_blank'>", targetName, "</a>")
        } else {
          targetName
        }
      }
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
    )
    # Mermaid Plot
    # mermaidPlot = reactable::colDef(
    #   name = "Mermaid Plot",
    #   html = TRUE,
    #   cell = function(value, index) {
    #     btn_id <- paste0("show-mermaid-", index)
    #     # Escape the Mermaid code for JS
    #     mermaid_code <- gsub("'", "\\\\'", toPlot$mermaidPlot[index])
    #     sprintf(
    #       "<a href='#' id='%s' onclick=\"showMermaidModal('%s')\">Show Mermaid Plot</a>",
    #       btn_id, mermaid_code
    #     )
    #   }
    # )
  )

  if (fileIssueRepo == "") {
    columns$fileIssue <- reactable::colDef(
      show = FALSE
    )
  } else {
    columns$fileIssue <- reactable::colDef(
      name = "File issue",
      maxWidth = 100,
      html = TRUE,
    )
  }

  elementId <- paste0(vocabularyId, "_for_", databaseName, "_mappings_status")

  htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$button(
        htmltools::tagList(fontawesome::fa("download"), "Download as CSV"),
        onclick = paste0("Reactable.downloadDataCSV('", elementId, "', '", elementId, ".csv')")
      ),
      reactable::reactable(
        toPlot,
        columns = columns,
        resizable = TRUE,
        filterable = TRUE,
        defaultPageSize = 10,
        elementId = elementId
      )
    )
  )
}


#' Plot Usagi file table
#'
#' @param usagiTibble Usagi tibble
#' @param colors List of colors
#' @return A reactable table object
#' @importFrom reactable reactable colDef
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

#' Plot summary table for Usagi file
#'
#' @param usagiTibble Usagi tibble
#' @param colors List of colors
#' @return A reactable table object
#' @importFrom reactable reactable colDef colFormat
.plotSummaryTableForUsagiFile <- function(
    usagiTibble,
    colors = list(
      flagged = "#EC6173",
      unchecked = "#F1AE4A",
      approved = "#51A350",
      inexact = "#AAAAAA"
    )) {
  usagiTibble |> checkmate::assert_tibble()
  colors |> checkmate::assert_list()
  colors |>
    names() |>
    checkmate::assert_subset(c("flagged", "unchecked", "approved", "inexact"))

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

#' Get summary list for a vocabulary
#'
#' @param pathToCodeCountsFolder Path to code counts folder
#' @param pathToVocabularyFolder Path to vocabulary folder
#' @param connectionDetails Connection details
#' @param vocabularyDatabaseSchema Vocabulary database schema
#' @param sourceVocabularyId Source vocabulary ID
#' @param targetVocabularyIds Target vocabulary IDs
#' @return List of summary tables for each database
#' @importFrom readr read_csv
#' @importFrom dplyr filter
.getSummaryListForVocabulary <- function(
    pathToCodeCountsFolder,
    pathToVocabularyFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    sourceVocabularyId,
    targetVocabularyIds) {
  summaryTableForVocabularyAndDatabaseList <- list()

  databaseCoverageTibble <- file.path(pathToCodeCountsFolder, "databases_coverage.csv") |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::filter(!ignore)

  for (databaseName in databaseCoverageTibble$database_name) {
    summaryTableForVocabularyAndDatabase <- .getSummaryTableForVocabularyAndDatabase(
      pathToCodeCountsFolder = pathToCodeCountsFolder,
      pathToVocabularyFolder = pathToVocabularyFolder,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      sourceVocabularyId = sourceVocabularyId,
      targetVocabularyIds = targetVocabularyIds,
      databaseName = databaseName
    )

    if (!is.null(summaryTableForVocabularyAndDatabase)) {
      summaryTableForVocabularyAndDatabaseList[[databaseName]] <- summaryTableForVocabularyAndDatabase
    }
  }

  return(summaryTableForVocabularyAndDatabaseList)
}


#' Get summary table for a vocabulary and database
#'
#' @param pathToCodeCountsFolder Path to code counts folder
#' @param pathToVocabularyFolder Path to vocabulary folder
#' @param connectionDetails Connection details
#' @param vocabularyDatabaseSchema Vocabulary database schema
#' @param sourceVocabularyId Source vocabulary ID
#' @param targetVocabularyIds Target vocabulary IDs
#' @param databaseName Database name
#' @return Summary table as a tibble
#' @importFrom dplyr full_join left_join mutate case_when
.getSummaryTableForVocabularyAndDatabase <- function(
    pathToCodeCountsFolder,
    pathToVocabularyFolder,
    connectionDetails,
    vocabularyDatabaseSchema,
    sourceVocabularyId,
    targetVocabularyIds,
    databaseName) {
  pathToCodeCountsFolder |> checkmate::assert_directory()
  pathToVocabularyFolder |> checkmate::assert_directory()
  connectionDetails |> checkmate::assert_class("ConnectionDetails")
  vocabularyDatabaseSchema |> checkmate::assert_string()
  sourceVocabularyId |> checkmate::assert_string()
  targetVocabularyIds |> checkmate::assert_character()
  databaseName |> checkmate::assert_string()

  codeCounts <- .getCodeCountsForVocabularyAndDatabase(
    pathToCodeCountsFolder = pathToCodeCountsFolder,
    sourceVocabularyId = sourceVocabularyId,
    databaseName = databaseName
  )

  if (nrow(codeCounts) == 0) {
    return(NULL)
  }

  databaseSummary <- .getDatabaseSummaryForVocabulary(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    targetVocabularyIds = targetVocabularyIds
  )

  usagiSummary <- .getUsagiSummaryForVocabulary(
    pathToVocabularyFolder = pathToVocabularyFolder,
    sourceVocabularyId = sourceVocabularyId
  )

  summaryTableForVocabularyAndDatabase <- codeCounts |>
    dplyr::full_join(databaseSummary |> dplyr::select(-sourceVocabularyId), by = c("sourceCode" = "sourceCode")) |>
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


#' Get code counts for a vocabulary and database
#'
#' @param pathToCodeCountsFolder Path to code counts folder
#' @param sourceVocabularyId Source vocabulary ID
#' @param databaseName Database name
#' @return Code counts tibble
#' @importFrom readr read_csv
#' @importFrom dplyr filter rename_with pull
.getCodeCountsForVocabularyAndDatabase <- function(
    pathToCodeCountsFolder,
    sourceVocabularyId,
    databaseName) {
  pathToCodeCountsFolder |> checkmate::assert_directory()
  sourceVocabularyId |> checkmate::assert_string()
  databaseName |> checkmate::assert_string()

  databaseCoverageTibble <- file.path(pathToCodeCountsFolder, "databases_coverage.csv") |>
    readr::read_csv(show_col_types = FALSE)

  databaseCoverageTibble <- databaseCoverageTibble |>
    dplyr::filter(database_name == databaseName)

  codeCountsTibble <- file.path(pathToCodeCountsFolder, databaseCoverageTibble |> dplyr::pull(path_to_code_counts_file)) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::rename_with(SqlRender::snakeCaseToCamelCase)

  a <- sourceVocabularyId
  codeCountsTibble <- codeCountsTibble |>
    dplyr::filter(sourceVocabularyId == a)


  return(codeCountsTibble)
}

#' Get database summary for a vocabulary
#'
#' @param connectionDetails Connection details
#' @param vocabularyDatabaseSchema Vocabulary database schema
#' @param targetVocabularyIds Target vocabulary IDs
#' @return Database summary tibble
#' @importFrom DatabaseConnector connect disconnect dbGetQuery
#' @importFrom SqlRender render translate
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename_with left_join
.getDatabaseSummaryForVocabulary <- function(
    connectionDetails,
    vocabularyDatabaseSchema,
    targetVocabularyIds) {
  connection <- DatabaseConnector::connect(connectionDetails)

  connectionDetails |> checkmate::assert_class("ConnectionDetails")
  vocabularyDatabaseSchema |> checkmate::assert_string()
  targetVocabularyIds |> checkmate::assert_character()

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

#' Get Usagi summary for a vocabulary
#'
#' @param pathToVocabularyFolder Path to vocabulary folder
#' @param sourceVocabularyId Source vocabulary ID
#' @return Usagi summary tibble
#' @importFrom readr read_csv
#' @importFrom dplyr filter select mutate pull
.getUsagiSummaryForVocabulary <- function(
    pathToVocabularyFolder,
    sourceVocabularyId) {
  pathToVocabularyFolder |> checkmate::assert_directory()
  sourceVocabularyId |> checkmate::assert_string()

  vocabulariesTibble <- file.path(pathToVocabularyFolder, "vocabularies.csv") |>
    readr::read_csv(show_col_types = FALSE)

  if (!(sourceVocabularyId %in% vocabulariesTibble$source_vocabulary_id)) {
    usagiSummaryTibble <- tibble::tibble(
      sourceConceptId = integer(),
      targetConceptId = integer(),
      statusSetBy = character(),
      statusSetOn = character(),
      mappingStatus = character(),
      equivalence = character(),
      validationMessages = character(),
      autoUpdatingInfo = character(),
      .rows = 0
    )
    return(usagiSummaryTibble)
  }

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
