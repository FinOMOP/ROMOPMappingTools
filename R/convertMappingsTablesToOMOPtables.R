
#' convertMappingsTablesToOMOPtables
#'
#' @param usagi_mapping_tables
#' @param vocabulary_info_mapping_tables
#' @param path_to_temp_omop_vocabulary_folder
#' @param ignore_failed_rules
#'
#' @return
#' @export
#'
#' @examples
convertMappingsTablesToOMOPtables <- function(
    usagi_mapping_tables,
    vocabulary_info_mapping_tables = NULL,
    path_to_temp_omop_vocabulary_folder,
    ignore_failed_rules = FALSE
) {



  ###
  ## validate input parameters
  ###
  usagi_mapping_tables |> checkmate::assertTibble()
  checkmate::assertSubset(c("name", "table", "mapping_type", "mapping_version", "n_failed_rules"), usagi_mapping_tables |> names())

  if(!is.null(vocabulary_info_mapping_tables)){
    vocabulary_info_mapping_tables |> checkmate::assertTibble()
    checkmate::assertSubset(c("name", "table", "n_failed_rules"), vocabulary_info_mapping_tables |> names())
  }

  path_to_temp_omop_vocabulary_folder |> checkmate::assertDirectoryExists()

  ignore_failed_rules |>  checkmate::assertLogical()

  ###
  ## function
  ###

  # remove tables with errors
  if(ignore_failed_rules==FALSE){
    # find names with no errors in both tables
    valid_vocabs_names <- usagi_mapping_tables |> dplyr::filter(n_failed_rules==0) |> dplyr::pull(name)

    if(!is.null(vocabulary_info_mapping_tables)){
      valid_vocabs_names <- union(
        valid_vocabs_names,
        vocabulary_info_mapping_tables |> dplyr::filter(n_failed_rules==0) |> dplyr::pull(name)
      )
    }

    # remove tables with errors
    usagi_mapping_tables <- usagi_mapping_tables |> dplyr::filter(name %in% valid_vocabs_names)

    if(!is.null(vocabulary_info_mapping_tables)){
      vocabulary_info_mapping_tables <- vocabulary_info_mapping_tables |> dplyr::filter(name %in% valid_vocabs_names)
    }
  }


  ##
  # CONCEPT & CONCEPT_RELATIONSHIP mode
  ##
  usagi_mapping_tables_CCR <- usagi_mapping_tables |> dplyr::filter(mapping_type == "CCR")

  if(nrow(usagi_mapping_tables_CCR)>0 & nrow(vocabulary_info_mapping_tables)>0){

    code_mappings_for_CCR <- usagi_mapping_tables_CCR |> dplyr::select(vocabName=name, table) |>  tidyr::unnest(table)
    vocabularies_info_for_CCR <- vocabulary_info_mapping_tables |> dplyr::select(vocabName=name, mapping_version, table) |>  tidyr::unnest(table)

    ##
    # VOCABULARY table
    ##
    vocabularies_info_for_CCR |>  dplyr::filter(type=="Vocabulary") |>
      dplyr::transmute(
        vocabulary_id = text_id,
        vocabulary_name = text_name,
        vocabulary_reference = paste("FinOMOP mapping of", text_id),
        vocabulary_version = paste("FinOMOP", text_id, mapping_version, sep = "-"),
        vocabulary_concept_id = concept_id
      ) |>
      readr::write_tsv(
        file.path(path_to_temp_omop_vocabulary_folder, "VOCABULARY.csv"),
        col_names = FALSE,
        na = "")

    ##
    # CONCEPT_CLASS table
    ##
    vocabularies_info_for_CCR |>  dplyr::filter(type=="Concept Class") |>
      dplyr::transmute(
        concept_class_id = text_id,
        concept_class_name = text_name,
        concept_class_concept_id = concept_id
      )|>
      readr::write_tsv(
        file.path(path_to_temp_omop_vocabulary_folder, "CONCEPT_CLASS.csv"),
        col_names = FALSE,
        na = "")

    ##
    # CONCEPT_SYNONYM table
    ##
    #- 4181730 # Finnish language
    concept_synonym_fi <- tibble::tibble()
    if("ADD_INFO:sourceName_fi" %in% names(code_mappings_for_CCR)){

      concept_synonym_fi <- code_mappings_for_CCR |>
        dplyr::filter(!is.na(`ADD_INFO:sourceName_fi`)) |>
        dplyr::distinct(`ADD_INFO:sourceConceptId`, `ADD_INFO:sourceName_fi`) |>
        dplyr::transmute(
          concept_id = `ADD_INFO:sourceConceptId`,
          concept_synonym_name = `ADD_INFO:sourceName_fi`,
          language_concept_id = 4181730
        )

    }
    #- 4175777 # Swedish language
    concept_synonym_se <- tibble::tibble()
    if("ADD_INFO:sourceName_se" %in% names(code_mappings_for_CCR)){

      concept_synonym_se <- code_mappings_for_CCR |>
        dplyr::filter(!is.na(`ADD_INFO:sourceName_se`)) |>
        dplyr::distinct(`ADD_INFO:sourceConceptId`, `ADD_INFO:sourceName_se`) |>
        dplyr::transmute(
          concept_id = `ADD_INFO:sourceConceptId`,
          concept_synonym_name = `ADD_INFO:sourceName_se`,
          language_concept_id = 4175777
        )

    }

    dplyr::bind_rows(
      concept_synonym_fi,
      concept_synonym_se
    )|>
      readr::write_tsv(
        file.path(path_to_temp_omop_vocabulary_folder, "CONCEPT_SYNONYM.csv"),
        col_names = FALSE,
        na = "")

    ##
    # CONCEPT table
    ##

    # vocabulary concepts
    concept_info <- vocabularies_info_for_CCR |>
      dplyr::transmute(
        concept_id = concept_id,
        concept_name = text_name,
        domain_id  = "Metadata",
        vocabulary_id  =  type,
        concept_class_id = type,
        standard_concept = as.character(NA),
        concept_code  =   "OMOP generated",
        valid_start_date = as.Date("1970-01-01"),
        valid_end_date = as.Date("2099-12-31"),
        invalid_reason =  as.character(NA)
      )


    # code concepts
    concept_vocab <- code_mappings_for_CCR |>
      dplyr::group_by(
        vocabName ,
        `ADD_INFO:sourceConceptId`,
        sourceCode, sourceName,
        `ADD_INFO:sourceDomain`, `ADD_INFO:sourceConceptClass`,
        `ADD_INFO:sourceValidStartDate`,`ADD_INFO:sourceValidEndDate`
      ) |>
      #
      dplyr::summarise(
        recalcualted_domainId = stringr::str_c( sort(unique(domainId ))  , collapse = " "),
        .groups="drop"
      )|>
      # recalculate domains
      dplyr::mutate(recalcualted_domainId = dplyr::case_when(
        recalcualted_domainId == "Condition Device" ~      "Condition/Device",
        recalcualted_domainId == "Condition Measurement" ~ "Condition/Meas",
        recalcualted_domainId == "Condition Observation" ~ "Condition/Obs",
        recalcualted_domainId == "Condition Procedure" ~   "Condition/Procedure",
        recalcualted_domainId == "Device Drug" ~           "Device/Drug",
        recalcualted_domainId == "Device Procedure" ~      "Device/Procedure",
        recalcualted_domainId == "Drug Procedure" ~        "Drug/Procedure",
        recalcualted_domainId == "Measurement Procedure" ~ "Meas/Procedure",
        recalcualted_domainId == "Observation Procedure" ~ "Obs/Procedure",
        recalcualted_domainId == "" ~ `ADD_INFO:sourceDomain`,
        TRUE ~ recalcualted_domainId
      )) |>
      #
      dplyr::transmute(
        concept_id = `ADD_INFO:sourceConceptId`,
        concept_name = sourceName |> stringr::str_sub(1,255),
        domain_id = recalcualted_domainId, # at the moment
        vocabulary_id = vocabName,
        concept_class_id = `ADD_INFO:sourceConceptClass`,
        standard_concept = as.character(NA),
        concept_code = sourceCode,
        valid_start_date = `ADD_INFO:sourceValidStartDate`,
        valid_end_date = `ADD_INFO:sourceValidEndDate`
      )


    # add  concepts for domain and concept_class
    dplyr::bind_rows(
      concept_info,
      concept_vocab
    )|>
      dplyr::mutate(
        valid_start_date =  .date_to_character(valid_start_date),
        valid_end_date =  .date_to_character(valid_end_date)
      ) |>
      readr::write_tsv(
        file.path(path_to_temp_omop_vocabulary_folder, "CONCEPT.csv"),
        col_names = FALSE,
        na = "")

    ##
    # CONCEPT_RELATIONSHIP table
    ##
    mapsto_concept_relationship <- code_mappings_for_CCR  |>
      dplyr::filter(mappingStatus=="APPROVED") |>
      dplyr::transmute(
        concept_id_1 = `ADD_INFO:sourceConceptId`,
        concept_id_2 = conceptId,
        relationship_id = "Maps to",
        valid_start_date = `ADD_INFO:sourceValidStartDate`,#pmax(valid_start_date, valid_start_date_2),
        valid_end_date   = `ADD_INFO:sourceValidEndDate`,#pmin(valid_end_date, valid_end_date_2),
        invalid_reason = as.character(NA)
      )

    mapsfrom_concept_relationship <- code_mappings_for_CCR  |>
      dplyr::filter(mappingStatus=="APPROVED") |>
      dplyr::transmute(
        concept_id_1 = conceptId,
        concept_id_2 = `ADD_INFO:sourceConceptId`,
        relationship_id = "Mapped from",
        valid_start_date = `ADD_INFO:sourceValidStartDate`,#pmax(valid_start_date, valid_start_date_2),
        valid_end_date   = `ADD_INFO:sourceValidEndDate`,#pmin(valid_end_date, valid_end_date_2),
        invalid_reason = as.character(NA)
      )

    dplyr::bind_rows(
      mapsto_concept_relationship,
      mapsfrom_concept_relationship
    )|>
      dplyr::mutate(
        valid_start_date =  .date_to_character(valid_start_date),
        valid_end_date =  .date_to_character(valid_end_date)
      ) |>
      readr::write_tsv(
        file.path(path_to_temp_omop_vocabulary_folder, "CONCEPT_RELATIONSHIP.csv"),
        col_names = FALSE,
        na = "")

  }

  ##
  # SOURCE TO CONCEP MAP mode
  ##
  usagi_mapping_tables_CCR <- usagi_mapping_tables |> dplyr::filter(mapping_type == "STCM")

  if(nrow(usagi_mapping_tables_CCR)>0){

  }

}




.date_to_character  <- function(date){
  return(paste0(as.character(lubridate::year(date)) |> stringr::str_pad(4,"left", "0"),
                as.character(lubridate::month(date))|> stringr::str_pad(2,"left", "0"),
                as.character(lubridate::day(date)) |> stringr::str_pad(2,"left", "0"))
  )
}








