
library(tibble)
library(tidyr)

test_that("test validateFixUnitTibble returns errors with an invalid fix_unit_based_in_abbreviation.tsv file", {
  pathToFixUnitFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/fix_unit_based_in_abbreviation_with_errors.tsv", package = "ROMOPMappingTools")
  fixUnitTibble <- readFixUnitFile(pathToFixUnitFile)

  # Create validNameUnitsTibble with test_name and measurement_unit columns
  validNameUnitsTibble <- tibble::tribble(
    ~test_name, ~measurement_unit,
    "p-tt-inr", "inr",
    "u-ph", "ph",
    "s-ph", "ph",
    "fs-ph", "ph",
    "p-tt-inr", "g/24h",
    "u-ph", "mmol/24h",
    "s-ph", "nmol/24h"
  )

  result <- validateFixUnitTibble(fixUnitTibble, validNameUnitsTibble)

  fixUnitTibble <- result$fixUnitTibble

  # Check that we have the expected number of rows
  expect_equal(nrow(fixUnitTibble), 7)

  # Row 1: "p-tt-inr", "", "inr" - has duplicate TEST_NAME_ABBREVIATION and source_unit_clean combination
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[1], "TEST_NAME_ABBREVIATION source_unit_clean is unique"))

  # Row 2: "", "form", "inr" - empty TEST_NAME_ABBREVIATION
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[2], "TEST_NAME_ABBREVIATION is empty"))

  # Row 3: "p-tt-inr", "", "invalid" - invalid TEST_NAME_ABBREVIATION and source_unit_clean_fix pair
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[3], "TEST_NAME_ABBREVIATION source_unit_clean_fix pair valid"))

  # Row 4: "u-ph", "form", "ph" - should have no errors (valid)
  expect_equal(fixUnitTibble$validation_messages[4], "")

  # Row 5: "u-ph", "", "ph" - should have no errors (valid, different source_unit_clean)
  expect_equal(fixUnitTibble$validation_messages[5], "")

  # Row 6: "s-ph", "", "invalid_unit" - invalid TEST_NAME_ABBREVIATION and source_unit_clean_fix pair
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[6], "TEST_NAME_ABBREVIATION source_unit_clean_fix pair valid"))

  # Row 7: "fs-ph", "", "ph" - should have no errors (valid)
  expect_equal(fixUnitTibble$validation_messages[7], "")

  # Check that validation log has errors
  result$validationLogR6$logTibble |>
    dplyr::filter(type == "ERROR") |>
    nrow() |>
    expect_gt(0)
})


test_that("test validateFixUnitTibble returns no errors with a valid fix_unit_based_in_abbreviation.tsv file", {
  pathToFixUnitFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/fix_unit_based_in_abbreviation.tsv", package = "ROMOPMappingTools")
  fixUnitTibble <- readFixUnitFile(pathToFixUnitFile)
  pathToValidUnitsFile <- system.file("testdata/VOCABULARIES/UNITfi/UNITfi.usagi.csv", package = "ROMOPMappingTools")
  
  # Create validNameUnitsTibble from the actual combinations in the fixUnitTibble
  # This is more realistic than creating a cross product of all possible combinations
  validNameUnitsTibble <- fixUnitTibble |>
    dplyr::filter(!is.na(TEST_NAME_ABBREVIATION), !is.na(source_unit_clean_fix)) |>
    dplyr::select(test_name = TEST_NAME_ABBREVIATION, measurement_unit = source_unit_clean_fix) |>
    dplyr::distinct()

  result <- validateFixUnitTibble(fixUnitTibble, validNameUnitsTibble)

  result$validationLogR6$logTibble |>
    dplyr::filter(type == "ERROR") |>
    nrow() |>
    expect_equal(0)

})
