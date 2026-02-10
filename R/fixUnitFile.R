#' Read Fix Unit File
#'
#' Reads a fix unit file (TSV format) with appropriate column type specifications.
#'
#' Required columns:
#' - TEST_NAME_ABBREVIATION (character): Test name abbreviation
#' - source_unit_clean (character): Source unit to fix (can be empty)
#' - source_unit_clean_fix (character): Fixed source unit
#'
#' @param pathToFixUnitFile Path to the fix unit file to read
#'
#' @return A tibble containing the fix unit data with appropriate column types
#'
#' @importFrom readr read_tsv cols col_character
#' @importFrom checkmate assertFileExists
#' @importFrom dplyr mutate
#'
#' @export
readFixUnitFile <- function(pathToFixUnitFile) {
    checkmate::assertFileExists(pathToFixUnitFile)

    cols <- readr::cols(
        TEST_NAME_ABBREVIATION = readr::col_character(),
        source_unit_clean = readr::col_character(),
        source_unit_clean_fix = readr::col_character(),
        .default = readr::col_character()
    )

    fixUnitTibble <- readr::read_tsv(pathToFixUnitFile, col_types = cols, na = c(""))
    if(!('validation_messages' %in% names(fixUnitTibble))) {
        fixUnitTibble <- fixUnitTibble |>
            dplyr::mutate(validation_messages = "")
    }
    return(fixUnitTibble)
}

#' Validate Fix Unit Tibble
#'
#' Validates a fix unit tibble against a set of rules:
#' - TEST_NAME_ABBREVIATION is not empty
#' - TEST_NAME_ABBREVIATION and source_unit_clean combination is unique
#' - TEST_NAME_ABBREVIATION and source_unit_clean_fix pair must exist in validNameUnitsTibble
#'
#' @param fixUnitTibble A tibble containing fix unit data
#' @param validNameUnitsTibble A tibble with columns test_name and measurement_unit containing valid combinations
#'
#' @return A list containing:
#'   - fixUnitTibble: The validated tibble with validation messages
#'   - validationLogR6: A LogTibble object containing validation results
#'
#' @importFrom checkmate assertTibble
#' @importFrom dplyr mutate slice bind_rows select
#' @importFrom validate validator confront
#'
#' @export
validateFixUnitTibble <- function(fixUnitTibble, validNameUnitsTibble) {
    checkmate::assertTibble(fixUnitTibble)
    checkmate::assertTibble(validNameUnitsTibble)
    
    # Validate that validNameUnitsTibble has required columns
    requiredValidColumns <- c("test_name", "measurement_unit")
    missingValidColumns <- requiredValidColumns |>
        setdiff(names(validNameUnitsTibble))
    
    if (length(missingValidColumns) > 0) {
        stop(paste0("validNameUnitsTibble missing required columns: ", paste(missingValidColumns, collapse = ", ")))
    }

    validationLogR6 <- LogTibble$new()

    fixUnitTibble <- fixUnitTibble |>
        dplyr::mutate(tmpvalidationMessages = "")

    # Check required columns exist
    requiredColumns <- c(
        "TEST_NAME_ABBREVIATION",
        "source_unit_clean",
        "source_unit_clean_fix"
    )

    missingColumns <- requiredColumns |>
        setdiff(names(fixUnitTibble))

    if (length(missingColumns) > 0) {
        validationLogR6$ERROR(
            "Missing required columns",
            paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
        )
        return(list(fixUnitTibble = fixUnitTibble, validationLogR6 = validationLogR6))
    } else {
        validationLogR6$SUCCESS("Missing required columns", "")
    }

    # Create a lookup for valid combinations
    validCombinations <- paste(validNameUnitsTibble$test_name, validNameUnitsTibble$measurement_unit, sep = "___")
    
    # Validate rules
    validationRules <- validate::validator(
        TEST_NAME_ABBREVIATION.source_unit_clean.is.unique = is_unique(TEST_NAME_ABBREVIATION, source_unit_clean),
        TEST_NAME_ABBREVIATION.is.empty = is_complete(TEST_NAME_ABBREVIATION),
        TEST_NAME_ABBREVIATION.source_unit_clean_fix.pair.valid = paste(TEST_NAME_ABBREVIATION, source_unit_clean_fix, sep = "___") %in% validCombinations
    )

    validations <- validate::confront(fixUnitTibble, validationRules, ref = list(validCombinations = validCombinations))
    result <- .applyValidationRules(fixUnitTibble, validations, validationLogR6) 
    fixUnitTibble <- result$fileTibble
    validationLogR6 <- result$validationLogR6

   
    fixUnitTibble <- fixUnitTibble |>
        dplyr::mutate(validation_messages = tmpvalidationMessages) |> 
        dplyr::select(-tmpvalidationMessages)

    return(list(fixUnitTibble = fixUnitTibble, validationLogR6 = validationLogR6))
}
