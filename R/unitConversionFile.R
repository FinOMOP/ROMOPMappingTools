#' Read Conversion File
#'
#' Reads a unit conversion file (TSV format) with appropriate column type specifications.
#'
#' Required columns:
#' - omop_quantity (character): OMOP quantity concept name
#' - source_unit_valid (character): Source unit to convert from
#' - to_source_unit_valid (character): Target unit to convert to
#' - conversion (character): Conversion formula or factor
#' - only_to_omop_concepts (integer): Optional concept ID restriction
#'
#' @param pathToConversionFile Path to the conversion file to read
#'
#' @return A tibble containing the conversion data with appropriate column types
#'
#' @importFrom readr read_tsv cols col_character col_integer
#' @importFrom checkmate assertFileExists
#'
#' @export
readConversionFile <- function(pathToConversionFile) {
    checkmate::assertFileExists(pathToConversionFile)

    cols <- readr::cols(
        omop_quantity = readr::col_character(),
        source_unit_valid = readr::col_character(),
        to_source_unit_valid = readr::col_character(),
        conversion = readr::col_character(),
        only_to_omop_concepts = readr::col_integer(),
        .default = readr::col_character()
    )

    readr::read_tsv(pathToConversionFile, col_types = cols, na = c(""))
}

#' Validate Conversion Tibble
#'
#' Validates a conversion tibble against a set of rules:
#' - omop_quantity column is not empty and is string
#' - source_unit_valid and to_source_unit_valid are not empty, are strings, and all units exist in validUnitsList
#' - conversion column is not empty and is string
#' - only_to_omop_concepts can be empty and is integer
#'
#' @param conversionTibble A tibble containing conversion data
#' @param validUnitsList A character vector of valid unit codes
#'
#' @return A tibble containing validation results with columns: type, step, message
#'
#' @importFrom checkmate assertTibble assertCharacter
#' @importFrom dplyr filter mutate pull
#' @importFrom validate validator confront
#'
#' @export
validateConversionTibble <- function(conversionTibble, validUnitsList) {
    checkmate::assertTibble(conversionTibble)
    checkmate::assertCharacter(validUnitsList, any.missing = FALSE)

    validationLogR6 <- LogTibble$new()

    # Check required columns exist
    requiredColumns <- c(
        "omop_quantity",
        "source_unit_valid",
        "to_source_unit_valid",
        "conversion",
        "only_to_omop_concepts"
    )

    missingColumns <- requiredColumns |>
        setdiff(names(conversionTibble))

    if (length(missingColumns) > 0) {
        validationLogR6$ERROR(
            "Missing required columns",
            paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
        )
        return(validationLogR6$logTibble)
    } else {
        validationLogR6$SUCCESS("Missing required columns", "")
    }

    # Validate omop_quantity: not empty and is string
    validationRules <- validate::validator(
        omop_quantity.is.empty = is_complete(omop_quantity),
        omop_quantity.is.not.string = is.character(omop_quantity)
    )
    validations <- validate::confront(conversionTibble, validationRules)
    result <- .applyValidationRules(fileTibble = NULL, validations, validationLogR6)
    validationLogR6 <- result$validationLogR6

    # Validate source_unit_valid: not empty, is string, and exists in validUnitsList
    validationRules <- validate::validator(
        source_unit_valid.is.empty = is_complete(source_unit_valid),
        source_unit_valid.is.not.string = is.character(source_unit_valid),
        source_unit_valid.not.in.validUnitsList = source_unit_valid %in% validUnitsList
    )
    validations <- validate::confront(
        conversionTibble,
        validationRules,
        ref = list(validUnitsList = validUnitsList)
    )
    result <- .applyValidationRules(fileTibble = NULL, validations, validationLogR6)
    validationLogR6 <- result$validationLogR6

    # Validate to_source_unit_valid: not empty, is string, and exists in validUnitsList
    validationRules <- validate::validator(
        to_source_unit_valid.is.empty = is_complete(to_source_unit_valid),
        to_source_unit_valid.is.not.string = is.character(to_source_unit_valid),
        to_source_unit_valid.not.in.validUnitsList = to_source_unit_valid %in% validUnitsList
    )
    validations <- validate::confront(
        conversionTibble,
        validationRules,
        ref = list(validUnitsList = validUnitsList)
    )
    result <- .applyValidationRules(fileTibble = NULL, validations, validationLogR6)
    validationLogR6 <- result$validationLogR6

    # Validate conversion: not empty and is string
    validationRules <- validate::validator(
        conversion.is.empty = is_complete(conversion),
        conversion.is.not.string = is.character(conversion)
    )
    validations <- validate::confront(conversionTibble, validationRules)
    result <- .applyValidationRules(fileTibble = NULL, validations, validationLogR6)
    validationLogR6 <- result$validationLogR6

    # Validate only_to_omop_concepts: can be empty and is integer
    # Check if column exists and has correct type
    # Since readr::col_integer() reads as integer, we just need to verify it's integer type
    if ("only_to_omop_concepts" %in% names(conversionTibble)) {
        # Check if all non-NA values are integers or can be converted to integers
        nonNaValues <- conversionTibble |>
            dplyr::filter(!is.na(only_to_omop_concepts)) |>
            dplyr::pull(only_to_omop_concepts)

        if (length(nonNaValues) > 0) {
            # Check if it's integer type or numeric that can be converted to integer
            if (!is.integer(nonNaValues) && !(is.numeric(nonNaValues) && all(nonNaValues == as.integer(nonNaValues), na.rm = TRUE))) {
                validationLogR6$ERROR(
                    "only_to_omop_concepts.is.not.integer",
                    "only_to_omop_concepts must be integer type"
                )
            } else {
                validationLogR6$SUCCESS("only_to_omop_concepts.is.not.integer", "")
            }
        } else {
            validationLogR6$SUCCESS("only_to_omop_concepts.is.not.integer", "")
        }
    }

    return(validationLogR6$logTibble)
}
