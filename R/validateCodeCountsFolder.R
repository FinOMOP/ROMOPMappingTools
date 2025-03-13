#' Validate Code Counts Folder
#'
#' Validates all code count files in a code counts folder against a set of rules.
#' The folder must contain a vocabularies_coverage.csv file that describes the vocabularies
#' and their coverage, and a databases_coverage.csv file that describes the databases
#' and their corresponding code count files.
#'
#' @param pathToCodeCountsFolder Path to folder containing code count files
#'
#' @return A tibble containing validation results for all files
#'
#' @importFrom checkmate assertDirectory assertFileExists
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows mutate select everything
#'
#' @export
validateCodeCountsFolder <- function(
    pathToCodeCountsFolder) {
    #
    # Parameter validation
    #
    pathToCodeCountsFolder |> checkmate::assertDirectoryExists()


    pathToCodeCountsFolder |>
        file.path("vocabularies_coverage.csv") |>
        checkmate::assertFileExists()
    pathToCodeCountsFolder |>
        file.path("databases_coverage.csv") |>
        checkmate::assertFileExists()

    #
    # Checks
    #
    validationLogR6 <- LogTibble$new()

    if (!file.exists(pathToCodeCountsFolder |> file.path("vocabularies_coverage.csv"))) {
        validationLogR6$ERROR("Missing vocabularies_coverage.csv file")
        return(validationLogR6$logTibble)
    }
    if (!file.exists(pathToCodeCountsFolder |> file.path("databases_coverage.csv"))) {
        validationLogR6$ERROR("Missing databases_coverage.csv file")
        return(validationLogR6$logTibble)
    }

    # check the vocabularies_coverage.csv file has the correct columns
    vocabulariesCoverageTibble <- readr::read_csv(pathToCodeCountsFolder |> file.path("vocabularies_coverage.csv"), show_col_types = FALSE)
    missingColumns <- c("source_vocabulary_id", "target_vocabulary_ids", "mantained_by", "ignore") |>
        setdiff(names(vocabulariesCoverageTibble))
    if (length(missingColumns) > 0) {
        validationLogR6$ERROR("Missing columns", paste0("Missing columns: ", paste(missingColumns, collapse = ", ")))
        return(validationLogR6$logTibble)
    }

    # check the vocabularies_coverage.csv file has correct values
    validationRules <- validate::validator(
        source_vocabulary_id.is.not.empty = !is.na(source_vocabulary_id) & source_vocabulary_id != "",
        target_vocabulary_ids.is.not.empty = !is.na(target_vocabulary_ids) & target_vocabulary_ids != "",
        mantained_by.is.not.empty = !is.na(mantained_by) & mantained_by != ""
    )
    validations <- validate::confront(vocabulariesCoverageTibble, validationRules)
    result <- .applyValidationRules(fileTibble = NULL, validations, validationLogR6)
    validationLogR6 <- result$validationLogR6

    # check the databases_coverage.csv file has the correct columns
    databasesCoverageTibble <- readr::read_csv(pathToCodeCountsFolder |> file.path("databases_coverage.csv"), show_col_types = FALSE)
    missingColumns <- c("database_name", "path_to_code_counts_file", "ignore") |>
        setdiff(names(databasesCoverageTibble))
    if (length(missingColumns) > 0) {
        validationLogR6$ERROR("Missing columns", paste0("Missing columns: ", paste(missingColumns, collapse = ", ")))
        return(validationLogR6$logTibble)
    }

    # check the databases_coverage.csv file has correct values
    validationRules <- validate::validator(
        database_name.is.not.empty = !is.na(database_name) & database_name != "",
        path_to_code_counts_file.is.not.empty = !is.na(path_to_code_counts_file) & path_to_code_counts_file != ""
    )
    validations <- validate::confront(databasesCoverageTibble, validationRules)
    result <- .applyValidationRules(fileTibble = NULL, validations, validationLogR6)
    validationLogR6 <- result$validationLogR6

    validationLogTibble <- validationLogR6$logTibble |>
        dplyr::mutate(
            context = "code_counts_folder"
        )

    # check the databases_coverage.csv file has correct values
    for (i in 1:nrow(databasesCoverageTibble)) {
        pathToCodeCountsFile <- file.path(pathToCodeCountsFolder, databasesCoverageTibble$path_to_code_counts_file[i])

        validationLogTibble_codeCountsFile <- validateCodeCountsFile(pathToCodeCountsFile)
        validationLogTibble <- validationLogTibble |>
            dplyr::bind_rows(validationLogTibble_codeCountsFile |>
                dplyr::mutate(
                    context = databasesCoverageTibble$database_name[i]
                ))
    }
    return(validationLogTibble)
}


#' Validate Code Counts Folder
#'
#' Validates all code count files in a code counts folder against a set of rules.
#' The folder must contain a vocabularies_coverage.csv file that describes which vocabularies
#' are covered and a databases_coverage.csv file that describes which databases are covered.
#'
#' @param pathToCodeCountsFile Path to the CSV file containing the database code counts.
#'
#' @return A tibble containing validation results for all files
#'
#' @importFrom checkmate assertDirectory assertFileExists
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows mutate select everything
#'
#' @export
validateCodeCountsFile <- function(
    pathToCodeCountsFile) {
    #
    # Parameter validation
    #
    pathToCodeCountsFile |> checkmate::assertFileExists()

    codeCountsTibble <- readr::read_csv(pathToCodeCountsFile, show_col_types = FALSE)

    #
    # Checks
    #
    validationLogR6 <- LogTibble$new()

    missingColumns <- c("source_vocabulary_id", "source_code", "n_events") |>
        setdiff(names(codeCountsTibble))

    if (length(missingColumns) > 0) {
        validationLogR6$ERROR("Missing columns", paste0("Missing columns: ", paste(missingColumns, collapse = ", ")))
        return(validationLogR6$logTibble)
    }

    validationRules <- validate::validator(
        source_vocabulary_id.is.not.na = is_complete(source_vocabulary_id),
        source_vocabulary_id.and.source_code.are.unique = is_unique(source_vocabulary_id, source_code),
        n_events.is.not.na = is_complete(n_events),
        n_events.is.more.than.5.or.minus.one = n_events > 5 | n_events == -1
    )
    validations <- validate::confront(codeCountsTibble, validationRules)
    result <- .applyValidationRules(fileTibble = NULL, validations, validationLogR6)
    validationLogR6 <- result$validationLogR6

    return(validationLogR6$logTibble)
}
