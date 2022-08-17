

autoFixDatabaseCodeCountTable <- function(database_name, database_count_table, keep_only_source_vocabulary_ids=NULL) {




  #
  # remove not used vocabs
  #
  if(!is.null(keep_only_source_vocabulary_ids)){

    vocabularies_not_valid <- database_count_table |>
      dplyr::distinct(source_vocabulary_id) |>
      dplyr::filter( !(source_vocabulary_id %in% keep_only_source_vocabulary_ids))

    database_count_table <- database_count_table |>
      dplyr::filter( source_vocabulary_id %in% keep_only_source_vocabulary_ids)

    warning(
      "AutoFixing Database ", database_name,
      "Removed  ", nrow(vocabularies_not_valid)," invalid source_vocabulary_id : \n",
      vocabularies_not_valid |> dplyr::pull(source_vocabulary_id)  |>  stringr::str_c(collapse = "\n")
    )

  }

  #
  # sum repeated values
  #
  repeated_codes <- database_count_table |>
    dplyr::count(source_vocabulary_id, source_code, sort = T) |>
    dplyr::filter(n>1)

    if(nrow(repeated_codes)>0){
    database_count_table <- database_count_table |>
      dplyr::group_by(source_vocabulary_id, source_code) |>
      dplyr::summarise(n_events = sum(n_events, na.rm = TRUE), .groups = "drop")

    warning(
      "AutoFixing Database ", database_name,
      "File has ", nrow(repeated_codes)," repeated codes. These were join and n_events added together"
    )
  }

  #
  # remove events less than 5
  #
  n_more_that_5 <- nrow(database_count_table |> dplyr::filter(n_events>0 & n_events<5))
  if(n_more_that_5 > 0){

    database_count_table <- database_count_table |>
      dplyr::filter(n_events<0 & n_events>5)

    warning(
      "AutoFixing Database ", database_name,
      "File has ", n_more_that_5," n_events with less than 5 !!!! REMOVE FROM SOURCE !!!!"
    )

  }

  return(database_count_table)

}

