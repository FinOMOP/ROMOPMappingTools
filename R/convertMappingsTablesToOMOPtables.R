
#' convertMappingsTablesToOMOPtables
#'
#' Converts usagi an info mapping tables to OMOP tables.
#'
#' @param usagi_mapping_tables Tibble containing the mapping tables imported from Usagi.csv files.
#' @param vocabulary_info_mapping_tables Tibble containing the mapping tables imported from info.csv files.
#' @param path_to_input_omop_vocabulary_folder Path to the input folder to read CONCEPT OMOP vocabulary table. This is necesary for building hierarchies in vocabularies that inherint from an other vocabulary, like some ICD10fi parents are in ICD10who.
#' @param path_to_temp_omop_vocabulary_folder Path to the output folder where to write the OMOP vocabulary tables
#' @param ignore_failed_rules Logical value indicating whether to ignore tables with failed rules.
#'
#' @importFrom checkmate assertTibble assertSubset assertDirectoryExists assertLogical
#' @importFrom readr write_tsv
#' @importFrom tidyr unnest
#' @importFrom dplyr filter select pull mutate distinct bind_rows group_by summarise case_when transmute
#' @importFrom stringr str_c str_sub
#'
#' @return None
#'
#' @export
convertMappingsTablesToOMOPtables <- function(
    usagi_mapping_tables,
    vocabulary_info_mapping_tables = NULL,
    path_to_input_omop_vocabulary_folder,
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

  path_to_input_omop_vocabulary_folder |> checkmate::assertDirectoryExists()
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
  # Read OMOP input CONCEPT table and get ICD10 codes
  ##

  concept_omop <- readr::read_tsv(file.path(path_to_input_omop_vocabulary_folder, "CONCEPT.csv"),na = c(""))
  concept_omop <- concept_omop |> dplyr::filter(vocabulary_id == "ICD10")

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
      # clear domain the mapping has not been accepted
      dplyr::mutate(
        domainId = dplyr::if_else(mappingStatus=="APPROVED", domainId, "" )
      ) |>
      #
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
        domain_id = recalcualted_domainId,
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

    # "Is a" relationship non-standard concepts using `ADD_INFO:sourceParents` and `ADD_INFO:sourceParentVocabulary`
    # Filter out for all codes where parent is not parent that would include Chapters as well as some codes with no parent information
    # If there is a parent code present and has vocabulary missing then change to sourceParentVocabulary is changed to vocabName
    # Split the sourceParents and sourceParentVocabulary at | symbol
    # Check if the combination of sourceParents and sourceParentVocabulary is present in concept_vocab then get concept_id
    # If not in concept_vocab Check in concept_omop then get concept_id
    # Filter out for unique rows and transmute them to concept relationship table columns
    isa_concept_relationship <- code_mappings_for_CCR  |>
      dplyr::filter(!is.na(`ADD_INFO:sourceParents`)) |>
      dplyr::mutate(`ADD_INFO:sourceParentVocabulary` = dplyr::if_else(is.na(`ADD_INFO:sourceParentVocabulary`),vocabName,`ADD_INFO:sourceParentVocabulary`)) |>
      dplyr::mutate(row = row_number(),
             parentCode = stringr::str_split(`ADD_INFO:sourceParents`, "\\|"),
             parentVocab = stringr::str_split(`ADD_INFO:sourceParentVocabulary`, "\\|")) |>
      tidyr::unnest(cols = c(parentCode, parentVocab)) |>
      dplyr::left_join(concept_vocab |> dplyr::select(concept_id,concept_code,vocabulary_id),
                       by = c("parentCode" = "concept_code", "parentVocab" = "vocabulary_id")) |>
      dplyr::left_join(concept_omop |> dplyr::select(concept_id,concept_code,vocabulary_id),
                       by = c("parentCode" = "concept_code", "parentVocab" = "vocabulary_id")) |>
      dplyr::mutate(concept_id = dplyr::coalesce(as.character(concept_id.x), as.character(concept_id.y))) |>
      dplyr::select(sourceCode, `ADD_INFO:sourceConceptId`,
                    `ADD_INFO:sourceValidStartDate`,`ADD_INFO:sourceValidEndDate`,
                    `ADD_INFO:sourceParents`, parentCode, parentVocab, concept_id) |>
      dplyr::distinct() |>
      dplyr::transmute(
        concept_id_1 = `ADD_INFO:sourceConceptId`,
        concept_id_2 = as.integer(concept_id),
        relationship_id = "Is a",
        valid_start_date = `ADD_INFO:sourceValidStartDate`,#pmax(valid_start_date, valid_start_date_2),
        valid_end_date   = `ADD_INFO:sourceValidEndDate`,#pmin(valid_end_date, valid_end_date_2),
        invalid_reason = as.character(NA)
      )

    # "Subsumes" relationship, non-standard concepts using `ADD_INFO:sourceParents` and `ADD_INFO:sourceParentVocabulary`
    # Same steps as "Is a" relationship just reverse concept_id_1 and concept_id_2 in the transmute
    subsumes_concept_relationship <- isa_concept_relationship |>
      dplyr::trasnmute(
        concept_id_1 = concept_id_2,
        concept_id_2 = concept_id_1,
        relationship_id = "Subsumes",
        valid_start_date = valid_start_date,
        valid_end_date = valid_end_date,
        invalid_reason = invalid_reason
      )

    #################################### END HERE

    dplyr::bind_rows(
      mapsto_concept_relationship,
      mapsfrom_concept_relationship,
      isa_concept_relationship,
      subsumes_concept_relationship
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








