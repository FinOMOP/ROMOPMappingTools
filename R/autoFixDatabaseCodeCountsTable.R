#' autoFixDatabaseCodeCountTable
#'
#' Automatically fixes issues in the database code count table.
#'
#' This function performs the following fixes on the provided database code count table:
#'
#' 1. Removes unused source vocabularies based on the specified `keep_only_source_vocabulary_ids`.
#' 2. Sums up repeated values for the same source vocabulary and source code.
#' 3. Removes events with a count less than 5.
#'
#' @param pathToCodeCountsFile Path to the CSV file containing the database code counts.
#' @param pathToCodeCountsFileFixed Path where the fixed CSV file should be saved. Defaults to overwriting the input file.
#' @param keepOnlySourceVocabularyIds Vector of source vocabulary IDs to keep. Other source vocabularies will be removed. If NULL, keeps all vocabularies.
#'
#' @importFrom dplyr distinct filter pull count group_by summarise
#' @importFrom stringr str_c
#'
#' @return Tibble containing the fixed database code count table.
#'
#' @export
autoFixDatabaseCodeCountsFile <- function(
    pathToCodeCountsFile,
    pathToCodeCountsFileFixed = pathToCodeCountsFile,
    keepOnlySourceVocabularyIds = NULL) {
  
  #
  # validate input
  #
  pathToCodeCountsFile |> checkmate::assertFileExists()
  databaseCodeCountsTibble <- readr::read_csv(pathToCodeCountsFile, show_col_types = FALSE)

  existingVocabularies <- databaseCodeCountsTibble |>
    dplyr::distinct(source_vocabulary_id) |>
    dplyr::pull(source_vocabulary_id)
    
  if (is.null(keepOnlySourceVocabularyIds)) {
    keepOnlySourceVocabularyIds  <- existingVocabularies
  }

    keepOnlySourceVocabularyIds |> checkmate::assertCharacter(null.ok = TRUE)
    keepOnlySourceVocabularyIds |> checkmate::assertSubset(existingVocabularies)

  #
  # remove not used vocabs
  #
  fixesLogR6  <- LogTibble$new()

  databaseCodeCountsTibble <- databaseCodeCountsTibble |>
      dplyr::filter(source_vocabulary_id %in% keepOnlySourceVocabularyIds)

  nVocabulariesRemoved <- nrow(databaseCodeCountsTibble |> dplyr::filter(!source_vocabulary_id %in% keepOnlySourceVocabularyIds))
  if (nVocabulariesRemoved > 0) {
    fixesLogR6$WARNING(
      step = "remove not used vocabs",
      message = paste0("Removed ", nVocabulariesRemoved, " source_vocabulary_id not in keepOnlySourceVocabularyIds")
    )
  }

  #
  # sum repeated values
  #
  repeatedCodes <- databaseCodeCountsTibble |>
    dplyr::count(source_vocabulary_id, source_code, sort = T) |>
    dplyr::filter(n > 1)

  if (nrow(repeatedCodes) > 0) {
    databaseCodeCountsTibble <- databaseCodeCountsTibble |>
      dplyr::group_by(source_vocabulary_id, source_code) |>
      dplyr::summarise(n_events = sum(n_events, na.rm = TRUE), .groups = "drop")

    fixesLogR6$WARNING(
      step = "sum repeated codes",
      message = paste0("File has ", nrow(repeatedCodes), " repeated codes. These were join and n_events added together")
    )
  }

  #
  # events with n_events less than 5 set to -1
  #
  n_more_that_5 <- nrow(databaseCodeCountsTibble |> dplyr::filter(n_events > 0 & n_events < 5))
  if (n_more_that_5 > 0) {
    databaseCodeCountsTibble <- databaseCodeCountsTibble |>
      dplyr::mutate(n_events = ifelse(n_events < 5, -1, n_events))

    fixesLogR6$WARNING(
      step = "set n_events less than 5 to -1",
      message = paste0("File has ", n_more_that_5, " n_events with less than 5 !!!! set to -1")
    )
  }

  databaseCodeCountsTibble |>
    readr::write_csv(pathToCodeCountsFileFixed)

  return(fixesLogR6$logTibble)
}
