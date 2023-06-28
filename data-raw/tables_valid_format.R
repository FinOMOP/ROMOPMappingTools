## code to prepare `tables_valid_format` dataset goes here



tables_valid_format <-  tibble::tribble(
  ~table_type, ~col_type, ~validation_rules, ~table_description, ~columns_description,

  ###
  # mapping tables
  ###
  "MappingVocabulariesInfo",
  readr::cols(
    source_vocabulary_id = readr::col_character(),
    twobillionare_code = readr::col_character(),
    path_to_usagi_file = readr::col_character(),
    path_to_usagi_file = readr::col_character(),
    mapping_version = readr::col_character(),
    last_modify_date = readr::col_date(format = ""),
    mapping_type = readr::col_character(),
    ignore = readr::col_logical()
  ),
  validate::validator(
    all.fileds.have.a.value = all_complete(names(validation_rules)),
    source_vocabulary_id.is.unique = is_unique(source_vocabulary_id),
    twobillionare_code.is.unique = is_unique(twobillionare_code),
    mapping_type.valid.values.are.CCR.or.STCM = mapping_type %in% c("CCR", "STCM")
  ),
  "List of local vocabularies and paths to the mapping files.",
  list(
    source_vocabulary_id = "Vocabulary_id as it appears in the usagi and info tables.",
    twobillionare_code = " First 6 digits of the concept_ids use in all the code in the vocabulary.",
    path_to_usagi_file = "Relative path to the usagi file.",
    path_to_usagi_file = "Relative path to the info file.",
    mapping_version = "Latest version of the vocabulary.",
    last_modify_date = "Date the vocabulary was last time modified.",
    mapping_type = "'CCR' for Concept&ConceptRelationship mapping or 'STCM' for Source To Concept Map (no yet implemented).",
    ignore = "If TRUE row is ignored in the processing."
  ),
  #
  "UsagiForCCR",
  readr::cols(
    sourceCode = readr::col_character(),
    sourceName = readr::col_character(),
    sourceFrequency = readr::col_integer(),
    sourceAutoAssignedConceptIds = readr::col_integer(),
    `ADD_INFO:sourceConceptId` = readr::col_double(),
    `ADD_INFO:sourceName_fi` = readr::col_character(),
    `ADD_INFO:sourceConceptClass` = readr::col_character(),
    `ADD_INFO:sourceDomain` = readr::col_character(),
    `ADD_INFO:sourceValidStartDate` = readr::col_date(),
    `ADD_INFO:sourceValidEndDate` = readr::col_date(),
    matchScore = readr::col_double(),
    mappingStatus = readr::col_character(),
    equivalence = readr::col_character(),
    statusSetBy = readr::col_character(),
    statusSetOn = readr::col_double(),
    conceptId = readr::col_integer(),
    conceptName = readr::col_character(),
    domainId = readr::col_character(),
    mappingType = readr::col_character(),
    comment = readr::col_character(),
    createdBy = readr::col_character(),
    createdOn = readr::col_double(),
    assignedReviewer = readr::col_character(),
    .default = readr::col_character()
  ),
  validate::validator(
    sourceCode.and.conceptId.are.uniques = is_unique(sourceCode, conceptId),
    sourceCode.is.not.empty = is_complete(sourceCode),
    sourceName.is.not.empty = is_complete(sourceName),
    sourceName.lessthan.255char = field_length(sourceName, min=0, max=255),
    valid_start_date.is.lower.than.valid_end_date = `ADD_INFO:sourceValidStartDate`<=`ADD_INFO:sourceValidEndDate`,
    concept_id.is.not.0.for.ACCEPTED.mappingStatus = if (mappingStatus=="APPROVED") conceptId!=0
  ),
  "Usagi file with few added columms needed to build the OMOP vocabulary tables",
  list(
    sourceCode = "Usagui column: code in the local vocabulary.",
    sourceName = "Usagui column: code's name in English as in the local vocabulary or traslated from.",
    sourceFrequency = "Usagui column: number of times the code appears in the reference database.",
    sourceAutoAssignedConceptIds = "Usagui column:",
    `ADD_INFO:sourceConceptId` = "Usagui-extended column: over 2 billion unique concept_id.",
    `ADD_INFO:sourceName_fi` = "Usagui-extended column:  code's name in Finnish.",
    `ADD_INFO:sourceConceptClass` = "Usagui-extended column: text id of hierarchy with in the local vocabulary. This must be as it appears in the vocabulary_info file.",
    `ADD_INFO:sourceDomain` = "Usagui-extended column: default domain for the code. If the code has a mapping the domain is infered from the standard code, if not, this value is used.",
    `ADD_INFO:sourceValidStartDate` = "Usagui-extended column: code's start valididty date in dd-mm-yyyy format. If not available set to 1970-01-01",
    `ADD_INFO:sourceValidEndDate` = "Usagui-extended column: code's end valididty date in dd-mm-yyyy format. If not available set to 2099-12-31",
    matchScore = "Usagui column:",
    mappingStatus = "Usagui column:",
    equivalence = "Usagui column:",
    statusSetBy = "Usagui column:",
    statusSetOn = "Usagui column:",
    conceptId = "Usagui column:",
    conceptName = "Usagui column:",
    domainId = "Usagui column:",
    mappingType ="Usagui column:",
    comment = "Usagui column:",
    createdBy = "Usagui column:",
    createdOn = "Usagui column:",
    assignedReviewer = "Usagui column:"
  ),
  #
  "VocabularyInfo",
  readr::cols(
    concept_id = readr::col_double(),
    type = readr::col_character(),
    text_id = readr::col_character(),
    text_name = readr::col_character()
  ),
  validate::validator(
    concept_id.is.uniques = is_unique(concept_id),
    text_id.lessthan.20char = field_length(text_id , min=0, max=20)
  ),
  "Additional info to build the VOCABULARY and CONCEPT_CLASS tables.",
  list(
    concept_id = "Concept_id for the vocabulary or concept_class tag.",
    type = "If the row is for info going to the 'Vocabulary' or 'Concep Class' tables.",
    text_id = "For 'Vocabulary' type the text going to vocabulary_id column, for 'Concept Class' type the text going to concept_class_id.",
    text_id = "For 'Vocabulary' type the test going to vocabulary_name column, for 'Concept Class' type the text going to concept_class_name."
  ),
  #
  "VocabulariesCoverage",
  readr::cols(
    source_vocabulary_id = readr::col_character(),
    target_vocabulary_ids = readr::col_character(),
    mantained_by = readr::col_character(),
    ignore = readr::col_logical()
  ),
  validate::validator(
    source_vocabulary_id.is.not.na = is_complete(source_vocabulary_id),
    target_vocabulary_ids.is.not.na = is_complete(target_vocabulary_ids),
    source_vocabulary_id.and.target_vocabulary_ids.is.uniques = is_unique(concept_id)
  ),
  "Rule to calculate the vocaburies coverage.",
  list(
    source_vocabulary_id = "Vocabulary id as it appears in the code_counts files.",
    target_vocabulary_ids = "Vocabulary id as it appears in the concept table.",
    mantained_by = "Maintainer of the vocabulary.",
    ignore = "If TRUE row is ignored in the processing."
  ),
  #
  "DatabasesCodeCounts",
  readr::cols(
    database_name = readr::col_character(),
    path_to_code_counts_file = readr::col_character(),
    ignore = readr::col_logical()
  ),
  validate::validator(
    database_name.is.not.na = is_complete(database_name),
    path_to_code_counts_file.is.not.na = is_complete(path_to_code_counts_file)
  ),
  "List of databases and links to their code_counts files.",
  list(
    database_name = "Name of the database.",
    path_to_code_counts_file = "Relative path to the code_counts file for the database.",
    ignore = "If TRUE row is ignored in the processing."
  ),
  #
  "CodeCounts",
  readr::cols(
    source_vocabulary_id = readr::col_character(),
    source_code = readr::col_character(),
    n_events = readr::col_integer()
  ),
  validate::validator(
    source_vocabulary_id.is.not.na = is_complete(source_vocabulary_id),
    source_vocabulary_id.and.source_code.are.unique = is_unique(source_vocabulary_id, source_code),
    n_events.is.more.than.5.or.minus.one = n_events>5|n_events==-1
  ),
  "Code counts from a database.",
  list(
    source_vocabulary_id= "Vocabulary_id the code belongs to.",
    source_code = "Code value.",
    n_events = "Number of time the code appears in the database (for privacy reasons n_events must be less than 5)."
  )
)

usethis::use_data(tables_valid_format, overwrite = TRUE)
