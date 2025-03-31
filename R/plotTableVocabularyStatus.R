


#' Plot Table Vocabulary Status
#'
#' This function plots the vocabulary status for a specific source vocabulary ID.
#'
#' @param mapping_status The mapping status data containing code counts.
#' @param source_vocabulary_id The source vocabulary ID for which to plot the status.
#'
#' @return A reactive table displaying the vocabulary status.
#'
#' @importFrom dplyr distinct pull filter select rename arrange desc
#' @importFrom tidyr spread
#' @importFrom reactable reactable
#' @export
#'
plotTableVocabularyStatus <- function(mapping_status, source_vocabulary_id) {

  link_to_athena <- "https://athena.ohdsi.org/search-terms/terms/"

  ###
  ## validate input parameters
  ###

  valid_source_vocabulary_id <- mapping_status$code_counts_matched |>
    dplyr::distinct(source_vocabulary_id) |>
    dplyr::pull(source_vocabulary_id)

  if(!(source_vocabulary_id %in% valid_source_vocabulary_id)){
    stop("source_vocabulary_id is not a valid vocabulary id in given mapping_status$code_counts_matched table")
  }




  table <- mapping_status$code_counts_matched |>
    dplyr::filter(source_vocabulary_id %in% !!source_vocabulary_id) |>
    dplyr::select(database_name , mapping_status,
                  source_code, concept_name, concept_name_fi, vocabulary_id,
                  n_events, maps_to=standard_concept_ids
    ) |>
    tidyr::spread(database_name, n_events)


  table_cols <- table |>  names() |>  dplyr::setdiff("all_databases")

  table <- table |>
    dplyr::select(!!!table_cols, all_databases) |>
    dplyr::rename(vocabulary_id = vocabulary_id ) |>
    dplyr::mutate(all_databases = all_databases/sum(all_databases)) |>
    dplyr::arrange(dplyr::desc(all_databases))


  table <- table |> reactable::reactable(
    filterable = TRUE,
    #
    columns = list(
      mapping_status = reactable::colDef(
        style = function(value) {
          if (value == "mapped") {
            color <- "#51A350"
          } else if (value == "no_mapping") {
            color <- "#F1AE4A"
          } else {
            color <- "#EC6173"
          }
          list(color = color, fontWeight = "bold")
        }
      ),
      maps_to = reactable::colDef(cell = function(value, index) {
        if(is.na(value)){
         return("")
        }
         # values <- str_split_1(value, ", ")
         # result <- c()
         # for(value in values){
         #   url <- paste0(link_to_athena, value)
         #   result <- c(result, list(htmltools::tags$a(href = url, target = "_blank", as.character(value))), ", ")
         # }
         # result <- result[1:(length(result)-1)]
         # htmltools::tags$div(result)
        value
      }),
      all_databases = reactable::colDef(
        format = reactable::colFormat(percent = TRUE, digits = 4)
      )
    )
  )

  return(table)

}
