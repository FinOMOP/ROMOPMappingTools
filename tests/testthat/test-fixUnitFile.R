test_that("test validateFixUnitTibble returns errors with an invalid fix_unit_based_in_abbreviation.tsv file", {
  pathToFixUnitFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/fix_unit_based_in_abbreviation_with_errors.tsv", package = "ROMOPMappingTools")
  fixUnitTibble <- readFixUnitFile(pathToFixUnitFile)

  validUnitsList <- c("inr", "ph", "g/24h", "mmol/24h", "nmol/24h", "ratio", "%", "umol/l", "ml/min/173m2", "mosm/kgh2o", "kg/l", "miu/l", "mg/l", "pmol/l", "mmol/l", "ug/l", "e12/l", "estimate", "mosm/l")

  result <- validateFixUnitTibble(fixUnitTibble, validUnitsList)

  fixUnitTibble <- result$fixUnitTibble

  # Check that we have the expected number of rows (first row removed and added back, so same count)
  expect_equal(nrow(fixUnitTibble), 9)

  # Row 1 (first row/header): should have no errors
  expect_equal(fixUnitTibble$validation_messages[1], "")

  # Row 2: "p-tt-inr", "", "inr" - should have no errors (valid)
  expect_equal(fixUnitTibble$validation_messages[2], "")

  # Row 3: "", "form", "inr" - empty TEST_NAME_ABBREVIATION
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[3], "TEST_NAME_ABBREVIATION is empty"))

  # Row 4: "p-tt-inr", "", "invalid" - invalid source_unit_clean_fix
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[4], "source_unit_clean_fix not in validUnitsList"))

  # Row 5: "p-tt-inr", "", "inr" - duplicate TEST_NAME_ABBREVIATION and source_unit_clean combination
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[5], "TEST_NAME_ABBREVIATION source_unit_clean is unique"))

  # Row 6: "u-ph", "form", "ph" - should have no errors (valid)
  expect_equal(fixUnitTibble$validation_messages[6], "")

  # Row 7: "u-ph", "", "ph" - should have no errors (valid, different source_unit_clean)
  expect_equal(fixUnitTibble$validation_messages[7], "")

  # Row 8: "s-ph", "", "invalid_unit" - invalid source_unit_clean_fix
  expect_true(stringr::str_detect(fixUnitTibble$validation_messages[8], "source_unit_clean_fix not in validUnitsList"))

  # Row 9: "fs-ph", "", "ph" - should have no errors (valid)
  expect_equal(fixUnitTibble$validation_messages[9], "")

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
  validUnitsList <- readUsagiFile(pathToValidUnitsFile) |>
    dplyr::filter(`ADD_INFO:UniqueForLab` == TRUE) |>
    dplyr::pull(sourceCode) |>
    unique() |>
    na.omit()

  result <- validateFixUnitTibble(fixUnitTibble, validUnitsList)

  result$validationLogR6$logTibble |>
    dplyr::filter(type == "ERROR") |>
    nrow() |>
    expect_equal(0)

  result$fixUnitTibble |>
    dplyr::filter(validation_messages != "") |> View()
})
