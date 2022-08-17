
#' @title tableMappingStatus
#' @description Creates an interactive table from mapping_status data
#' @param mapping_status output from `mapOMOPtoCodesFrequencies`
#' @return interactive reactable table
#' @export
#' @importFrom dplyr group_by summarise n mutate_if if_else arrange distinct filter pull left_join mutate select
#' @importFrom tidyr spread gather
#' @importFrom stringr str_c
#' @importFrom reactable colDef reactable
plotTableMappingStatus <- function(mapping_status) {


  # calculate percentage of codes mapped per vocabulary
  mapping_progress <- mapping_status$concepts_to_match |>
    dplyr::group_by(source_vocabulary_id) |>
    dplyr::summarise(
      target_vocabulary_ids = unique(target_vocabulary_ids) |> paste0(collapse = "+"),
      mantained_by  = unique(mantained_by) |> paste0(collapse = "+"),
      str_mapped = paste0(sum(has_mapping), "-",dplyr::n()-sum(has_mapping)),
      .groups = "drop")

  # calculate n events as string to be used by bar and tip functions
  code_counts_progress <- mapping_status$code_counts_matched |>
    # summarize n_events per mapping_status, vocab, database
    dplyr::group_by(database_name, source_vocabulary_id, mapping_status) |>
    dplyr::summarise(n_progress = sum(n_events), .groups = "drop") |>
    # add 0 if no value
    tidyr::spread(mapping_status, n_progress) |>
    dplyr::mutate_if(is.double,~dplyr::if_else(is.na(.),0,as.double(.))) |>
    tidyr::gather("mapping_status", "n_progress", -database_name, -source_vocabulary_id) |>
    dplyr::arrange(database_name, source_vocabulary_id, mapping_status) |>
    # group to string by mapping_status,
    # force rigth order in mapping_status
    dplyr::mutate(mapping_status=dplyr::case_when(
      mapping_status=="mapped" ~ "a",
      mapping_status=="no_mapping" ~ "b",
      mapping_status=="no_code" ~ "c"
    )) |> dplyr::arrange(mapping_status) |>
    dplyr::group_by(database_name, source_vocabulary_id) |>
    dplyr::summarise( str_n_events = stringr::str_c(n_progress, collapse = "-"), .groups = "drop")

  # get database names
  database_names <- code_counts_progress |>
    dplyr::distinct(database_name) |>
    dplyr::filter(database_name != "all_databases") |>
    dplyr::pull(database_name)
  database_names <- c(database_names, "all_databases")

  # spread to database names and append mapping_progress
  code_counts_table <- code_counts_progress |>
    tidyr::spread(database_name, str_n_events) |>
    #
    dplyr::right_join(mapping_progress, by="source_vocabulary_id")|>
    dplyr::mutate(str_mapped = dplyr::if_else(mantained_by=="OMOP", as.character(NA), str_mapped)) |>
    #
    dplyr::select(source_vocabulary_id, mantained_by, str_mapped, !!!database_names)


  # build columns

  bar_colors <- c("#51A350", "#F1AE4A", "#EC6173")
  row_names <- c("standar code: ", "non-standar code: ", "not found code: ")
  unit_names <- rep(" events", 3)

  columns_list <- list()

  columns_list[["mantained_by"]] <- reactable::colDef(name = "Maintained by")

  columns_list[["source_vocabulary_id"]] <- reactable::colDef(name = "Vocabulary")


  columns_list[["str_mapped"]] <- reactable::colDef(
    name = "Mapping progress",
    cell =  function(value){.tip_reactable_cell(value, row_names = c("mapped: ", "no mapped: "), unit_names = rep(" codes", 2) )},
    style = function(value){.background_bar_reactable_style(value, colors = c("#0000CC", "#E0E0E0"), height ="30%")},
    align = "left"
  )

  for(db_name in database_names){
    columns_list[[db_name]] <- reactable::colDef(
      name = paste("Coverage", db_name),
      cell =  function(value){.tip_reactable_cell(value, row_names = row_names, unit_names = unit_names )},
      style = function(value){.background_bar_reactable_style(value, colors = bar_colors)},
      align = "left"
    )
  }

  columns_list[["all_databases"]] <- reactable::colDef(
    name = "TOTAL",
    cell =  function(value){.tip_reactable_cell(value, row_names = row_names, unit_names = unit_names, only_percent = TRUE )},
    style = function(value){.background_bar_reactable_style(value, colors = bar_colors)},
    align = "left"
  )

  # create table
  table <- code_counts_table |>
    reactable::reactable(
      pagination = FALSE,
      #
      columns = columns_list
    )

  return(table)

}






# Render a bar chart in the background of the cell
.background_bar_reactable_style <- function(
    string_values, colors = NULL,
    height ="60%", width ="95%",
    background_color = "#FFFFFF", text_color =  "#FFFFFF"){


  if(is.na(string_values)){return("")}
  values<-  stringr::str_split(string_values, "-")[[1]] |> as.double()
  per_values <- values/sum(values)*100
  per_values <- c(0, per_values)

  if(is.null(colors)){
    colors <- RColorBrewer::brewer.pal(length(string_values), "Set3")
  }
  colors <- c(colors, background_color)

  gradient_str <- ""
  for(i in 1:(length(per_values)-1)){
    gradient_str <- paste0(
      gradient_str,
      colors[i], " ", per_values[i], "%, ",
      colors[i], " ", sum(per_values[1:(i+1)]), "%, "
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


.tip_reactable_cell <- function(
    string_values,
    row_names=NULL, unit_names=NULL, only_percent =FALSE){

  if(is.na(string_values)){return("")}
  values<-  stringr::str_split(string_values, "-")[[1]] |> as.double()
  per_values <- values/sum(values)

  n_rows <- length(values)

  if(is.null(row_names)){
    row_names <- rep("", n_rows)
  }

  if(is.null(unit_names)){
    unit_names <- rep("", n_rows)
  }


  if(only_percent==FALSE){
    text <- stringr::str_c(
      row_names,
      scales::number(values, scale_cut = scales::cut_short_scale()),
      unit_names,
      rep(" (", n_rows),
      scales::percent(per_values, accuracy = 0.01 ),
      rep(") ", n_rows)
    )
  }else{
    text <- stringr::str_c(
      row_names,
      scales::percent(per_values, accuracy = 0.01 )
    )
  }

  text <- text |> stringr::str_c(collapse = "<br>")

  shiny::div(
    tippy::tippy(text = paste(rep("&nbsp;", 36), collapse = " "),
                 tooltip = paste0("<span style='font-size:16px;'>",text,"<span>"),
                 allowHTML = TRUE,
                 theme="light",
                 arrow = TRUE)
  )
}
