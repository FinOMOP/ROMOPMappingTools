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
#' @param pathToUnitConversionFile Path to the conversion file to read
#'
#' @return A tibble containing the conversion data with appropriate column types
#'
#' @importFrom readr read_tsv cols col_character col_integer
#' @importFrom checkmate assertFileExists
#'
#' @export
readUnitConversionFile <- function(pathToUnitConversionFile) {
    checkmate::assertFileExists(pathToUnitConversionFile)

    cols <- readr::cols(
        omop_quantity = readr::col_character(),
        source_unit_valid = readr::col_character(),
        to_source_unit_valid = readr::col_character(),
        conversion = readr::col_character(),
        only_to_omop_concepts = readr::col_integer(),
        .default = readr::col_character()
    )

    unitConversionTibble <- readr::read_tsv(pathToUnitConversionFile, col_types = cols, na = c(""))
    if(!('validation_messages' %in% names(unitConversionTibble))) {
        unitConversionTibble <- unitConversionTibble |>
            dplyr::mutate(validation_messages = "")
    }
    return(unitConversionTibble)
}

#' Validate Unit Conversion Tibble
#'
#' Validates a conversion tibble against a set of rules:
#' - omop_quantity column is not empty and is string
#' - source_unit_valid and to_source_unit_valid are not empty, are strings, and all units exist in validUnitsList
#' - conversion column is not empty and is string
#' - only_to_omop_concepts can be empty and is integer
#'
#' @param unitConversionTibble A tibble containing conversion data
#' @param validUnitsList A character vector of valid unit codes
#' @param validQuantitiesList A character vector of valid quantity codes
#'
#' @return A tibble containing validation results with columns: type, step, message
#'
#' @importFrom checkmate assertTibble assertCharacter
#' @importFrom dplyr filter mutate pull
#' @importFrom validate validator confront
#'
#' @export
validateUnitConversionTibble <- function(unitConversionTibble, validUnitsList, validQuantitiesList) {
    checkmate::assertTibble(unitConversionTibble)
    checkmate::assertCharacter(validUnitsList, any.missing = FALSE)
    checkmate::assertCharacter(validQuantitiesList, any.missing = FALSE)

    validationLogR6 <- LogTibble$new()

    unitConversionTibble <- unitConversionTibble |>
        dplyr::mutate(tmpvalidationMessages = "")

    # Check required columns exist
    requiredColumns <- c(
        "omop_quantity",
        "source_unit_valid",
        "to_source_unit_valid",
        "conversion",
        "only_to_omop_concepts"
    )

    missingColumns <- requiredColumns |>
        setdiff(names(unitConversionTibble))

    if (length(missingColumns) > 0) {
        validationLogR6$ERROR(
            "Missing required columns",
            paste0("Missing columns: ", paste(missingColumns, collapse = ", "))
        )
        return(list(unitConversionTibble = unitConversionTibble, validationLogR6 = validationLogR6))
    } else {
        validationLogR6$SUCCESS("Missing required columns", "")
    }

    # remove first row and af it after the validation
    firstRow <- unitConversionTibble |> dplyr::slice(1)
    unitConversionTibble <- unitConversionTibble |>
        dplyr::slice(-1)

    # Validate omop_quantity: not empty and is string
    validationRules <- validate::validator(
        omop_quantity.source_unit_valid.to_source_unit_valid.is.unique = is_unique(omop_quantity, source_unit_valid, to_source_unit_valid, only_to_omop_concepts),
        omop_quantity.is.empty = is_complete(omop_quantity),
        omop_quantity.not.in.validQuantitiesList = omop_quantity %in% validQuantitiesList,
        #source_unit_valid.is.empty = is_complete(source_unit_valid),
        source_unit_valid.not.in.validUnitsList = (source_unit_valid %in% validUnitsList)|(is.na(source_unit_valid)),
        #to_source_unit_valid.is.empty = is_complete(to_source_unit_valid),
        to_source_unit_valid.not.in.validUnitsList = (to_source_unit_valid %in% validUnitsList)|(is.na(to_source_unit_valid)),
        conversion.is.not.valid =
            # Acceptable: floats like "1", "10.2", or formulas like "10.93*X-23.50"
            grepl(
                "^[+-]?(\\d*\\.?\\d+|\\d+)([eE][+-]?\\d+)?$|^([+-]?\\d*\\.?\\d+[eE]?[+-]?\\d*)?\\*X([+-]\\d*\\.?\\d+)?$",
                conversion
            ) 
    )

    validations <- validate::confront(unitConversionTibble, validationRules, ref = list(validQuantitiesList = validQuantitiesList, validUnitsList = validUnitsList))
    result <- .applyValidationRules(unitConversionTibble, validations, validationLogR6) 
    unitConversionTibble <- result$fileTibble
    validationLogR6 <- result$validationLogR6

    # add first row back
    unitConversionTibble <- firstRow |>
        dplyr::bind_rows(unitConversionTibble)
   
    unitConversionTibble <- unitConversionTibble |>
        dplyr::mutate(validation_messages = tmpvalidationMessages) |> 
        dplyr::select(-tmpvalidationMessages)

    return(list(unitConversionTibble = unitConversionTibble, validationLogR6 = validationLogR6))
}
