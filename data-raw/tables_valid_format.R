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
  "",
  list(
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
  "Usagi file with few added columms neede to build the OMOP vocabulary tables",
  list(
    sourceCode = "Usagui column: code in the local vocabulary",
    sourceName = "Usagui column: code's name in English as in the local vocabulary or traslated from",
    sourceFrequency = "Usagui column: number of times the code appears in the reference database",
    sourceAutoAssignedConceptIds = "Usagui column:",
    `ADD_INFO:sourceConceptId` = "Usagui-extended column: over 2 billion unique concept_id",
    `ADD_INFO:sourceName_fi` = "Usagui-extended column:  code's name in Finnish",
    `ADD_INFO:sourceConceptClass` = "Usagui-extended column: text id of hierarchy with in the local vocabulary. This must be as it appears in the vocabulary_info file",
    `ADD_INFO:sourceDomain` = "Usagui-extended column: default domain for the code. If the code has a mapping the domain is infered from the standard code, if not, this value is used.",
    `ADD_INFO:sourceValidStartDate` = "Usagui-extended column: code's start valididty date in dd-mm-yyyy format. If not available set to ",
    `ADD_INFO:sourceValidEndDate` = "Usagui-extended column: code's end valididty date in dd-mm-yyyy format. If not available set to",
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
  "",
  list(),
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
  "",
  list(),
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
  "",
  list(),
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
  "",
  list()
)

usethis::use_data(tables_valid_format, overwrite = TRUE)
