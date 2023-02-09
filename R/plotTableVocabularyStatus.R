

#' plotTableVocabularyStatus
#'
#' @param mapping_status
#' @param source_vocabulary_id
#'
#' @return
#' @export
#'
#' @examples
plotTableVocabularyStatus <- function(mapping_status, source_vocabulary_id) {


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
                  source_code, concept_name, concept_name_fi, source_vocabulary_id,
                  n_events
    ) |>
    tidyr::spread(database_name, n_events)


  table_cols <- table |>  names() |>  dplyr::setdiff("all_databases")

  table <- table |>
    dplyr::select(!!!table_cols, all_databases) |>
    dplyr::rename(vocabulary_id = source_vocabulary_id ) |>
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

      all_databases = reactable::colDef(
        format = reactable::colFormat(percent = TRUE, digits = 4)
      )
    )
  )

  return(table)

}

