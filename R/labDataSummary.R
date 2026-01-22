#' Read code counts lab folder
#'
#' Reads the three TSV files from a lab code counts folder and returns them as tibbles
#' with the correct column types.
#'
#' @param pathToCodeCountsLabFolder Path to folder containing lab code counts files
#'
#' @return A list containing three tibbles: summaryTest, summaryValues, and summaryOutcomes
#'
#' @importFrom checkmate assert_directory assert_file_exists
#' @importFrom readr read_tsv cols
#' @importFrom dplyr group_by summarise left_join mutate if_else distinct pull select filter ungroup bind_rows arrange case_when
#' @importFrom tibble tibble
#' @importFrom tidyr crossing
#' @importFrom purrr map2
.readCodeCountsLabFolder <- function(pathToCodeCountsLabFolder) {
    pathToCodeCountsLabFolder |> checkmate::assert_directory()
    pathToCodeCountsLabFolder |>
        file.path("summaryTest.tsv") |>
        checkmate::assert_file_exists()
    pathToCodeCountsLabFolder |>
        file.path("summaryValuesSource.tsv") |>
        checkmate::assert_file_exists()
    pathToCodeCountsLabFolder |>
        file.path("summaryValues.tsv") |>
        checkmate::assert_file_exists()
    pathToCodeCountsLabFolder |>
        file.path("summaryOutcomes.tsv") |>
        checkmate::assert_file_exists()

    # Read summaryTest.tsv
    summaryTest <- readr::read_tsv(
        file = pathToCodeCountsLabFolder |> file.path("summaryTest.tsv"),
        col_types = readr::cols(
            OMOP_CONCEPT_ID = "i",
            TEST_NAME = "c",
            MEASUREMENT_UNIT = "c",
            MEASUREMENT_UNIT_HARMONIZED = "c",
            omopQuantity = "c",
            CONVERSION_FACTOR = "c",
            n_records = "i",
            n_subjects = "i"
        ),
        show_col_types = FALSE,
        na = ""
    )

    # Read summaryValuesSource.tsv
    summaryValuesSource <- readr::read_tsv(
        file = pathToCodeCountsLabFolder |> file.path("summaryValuesSource.tsv"),
        col_types = readr::cols(
            OMOP_CONCEPT_ID = "i",
            TEST_NAME = "c",
            MEASUREMENT_UNIT = "c",
            value_source = "c",
            n_subjects = "i",
            n_records = "i"
        ),
        show_col_types = FALSE,
        na = ""
    )

    # Read summaryValues.tsv
    summaryValues <- readr::read_tsv(
        file = pathToCodeCountsLabFolder |> file.path("summaryValues.tsv"),
        col_types = readr::cols(
            OMOP_CONCEPT_ID = "i",
            TEST_NAME = "c",
            MEASUREMENT_UNIT = "c",
            n_subjects = "i",
            n_records = "i",
            decile = "d",
            decile_MEASUREMENT_VALUE = "d",
            decile_MEASUREMENT_VALUE_HARMONIZED = "d"
        ),
        show_col_types = FALSE,
        na = ""
    )

    # Read summaryOutcomes.tsv
    summaryOutcomes <- readr::read_tsv(
        file = pathToCodeCountsLabFolder |> file.path("summaryOutcomes.tsv"),
        col_types = readr::cols(
            OMOP_CONCEPT_ID = "i",
            TEST_NAME = "c",
            MEASUREMENT_UNIT = "c",
            TEST_OUTCOME = "c",
            n_TEST_OUTCOME = "i",
            n_subjects = "i"
        ),
        show_col_types = FALSE,
        na = ""
    )

    #
    # summaryValuesSource
    # - calculate distribution of values
    totalRecords <- summaryTest |>
        dplyr::distinct(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT, n_records)
    totalValues <- summaryValues |>
        dplyr::group_by(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT) |>
        dplyr::summarise(n_values = sum(n_records), .groups = "drop")
    missingValues <- totalRecords |>
        dplyr::left_join(totalValues, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT")) |>
        dplyr::mutate(
            n_records = dplyr::if_else(is.na(n_records), 0L, n_records),
            n_values = dplyr::if_else(is.na(n_values), 0L, n_values),
            n_missing = n_records - n_values
        ) |>
        dplyr::select(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT, n_records = n_missing)


    summaryValuesSourceToJoin <- dplyr::bind_rows(
        summaryValuesSource |>
            dplyr::select(-n_subjects),
        missingValues |>
            dplyr::mutate(value_source = "Missing")
    )

    summaryValuesSourceToJoin <- summaryValuesSourceToJoin |>
        dplyr::distinct(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT) |>
        tidyr::crossing(value_source = summaryValuesSourceToJoin |> dplyr::pull(value_source) |> unique()) |>
        dplyr::left_join(summaryValuesSourceToJoin, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT", "value_source")) |>
        dplyr::mutate(n_records = dplyr::if_else(is.na(n_records), 0L, n_records)) |>
        dplyr::rename(value = value_source, n = n_records) |>
        tidyr::nest(distribution_values = c(value, n)) |> 
        dplyr::left_join(totalValues, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT")) |> 
        dplyr::mutate(n_values = dplyr::if_else(is.na(n_values), 0L, n_values))


    # summaryValues
    # - calculate distribution of deciles
    summaryValuesA  <- summaryValues |>
        dplyr::filter(!is.na(decile_MEASUREMENT_VALUE)) |>
        dplyr::group_by(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT) |>
        dplyr::summarise(
            decile_MEASUREMENT_VALUE = list(tibble::tibble(
                decile = decile,
                value = decile_MEASUREMENT_VALUE
            )),
            .groups = "drop"
        )
    summaryValuesB  <- summaryValues |>
        dplyr::filter(!is.na(decile_MEASUREMENT_VALUE_HARMONIZED)) |>
        dplyr::group_by(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT) |>
        dplyr::summarise(
            decile_MEASUREMENT_VALUE_HARMONIZED = list(tibble::tibble(
                decile = decile,
                value = decile_MEASUREMENT_VALUE_HARMONIZED
            )),
            .groups = "drop"
        )
    summaryValuesToJoin <- dplyr::left_join(summaryValuesA, summaryValuesB, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT"))


    #
    # summaryOutcomes
    # - calculate distribution of outcomes
    totalRecords <- summaryTest |>
        dplyr::distinct(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT, n_records)
    totalOutcomes <- summaryOutcomes |>
        dplyr::group_by(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT) |>
        dplyr::summarise(n_outcomes = sum(n_TEST_OUTCOME), .groups = "drop")
    missingOutcomes <- totalRecords |>
        dplyr::left_join(totalOutcomes, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT")) |>
        dplyr::mutate(
            n_records = dplyr::if_else(is.na(n_records), 0L, n_records),
            n_outcomes = dplyr::if_else(is.na(n_outcomes), 0L, n_outcomes),
            n_missing = n_records - n_outcomes
        ) |>
        dplyr::select(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT, n_TEST_OUTCOME = n_missing)

    summaryOutcomesToJoin <- dplyr::bind_rows(
        summaryOutcomes |>
            dplyr::select(-n_subjects),
        missingOutcomes |>
            dplyr::mutate(TEST_OUTCOME = "NA")
    )
    summaryOutcomesToJoin <- summaryOutcomesToJoin |>
        dplyr::distinct(OMOP_CONCEPT_ID, TEST_NAME, MEASUREMENT_UNIT) |>
        tidyr::crossing(TEST_OUTCOME = summaryOutcomesToJoin |> dplyr::pull(TEST_OUTCOME) |> unique()) |>
        dplyr::left_join(summaryOutcomesToJoin, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT", "TEST_OUTCOME")) |>
        dplyr::mutate(n_TEST_OUTCOME = dplyr::if_else(is.na(n_TEST_OUTCOME), 0L, n_TEST_OUTCOME)) |>
        dplyr::rename(value = TEST_OUTCOME, n = n_TEST_OUTCOME) |>
        tidyr::nest(distribution_outcomes = c(value, n))


    summary <- summaryTest |>
        dplyr::left_join(summaryValuesSourceToJoin, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT")) |>
        dplyr::left_join(summaryValuesToJoin, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT")) |>
        dplyr::left_join(summaryOutcomesToJoin, by = c("OMOP_CONCEPT_ID", "TEST_NAME", "MEASUREMENT_UNIT"))

    return(summary)
}
