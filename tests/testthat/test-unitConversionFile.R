test_that("test validateConversionTibble returns no errors with a valid quantity_source_unit_conversion.tsv file", {


  pathToUnitConversionFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/quantity_source_unit_conversion.tsv", package = "ROMOPMappingTools")
  unitConversionTibble <- readUnitConversionFile(pathToUnitConversionFile)
  pathToValidatedUnitConversionFile <- tempfile(fileext = ".tsv")

  validUnitsList <- c(unitConversionTibble$source_unit_valid, unitConversionTibble$to_source_unit_valid) |> unique() |> na.omit()
  validQuantitiesList <- unitConversionTibble$omop_quantity |> unique() |> na.omit()

  result <- validateUnitConversionTibble(unitConversionTibble, validUnitsList, validQuantitiesList)

  result$validationLogR6$logTibble |> dplyr::filter(type == "ERROR") |> nrow() |> expect_equal(0)

})

test_that("test validateConversionTibble returns errors with a invalid quantity_source_unit_conversion.tsv file", {

  pathToUnitConversionFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/quantity_source_unit_conversion with_errors.tsv", package = "ROMOPMappingTools")
  unitConversionTibble <- readUnitConversionFile(pathToUnitConversionFile)
  pathToValidatedUnitConversionFile <- tempfile(fileext = ".tsv")

  validUnitsList <- c("au/ml", "g", "mg")
  validQuantitiesList <- c("Arbitrary Concentration", "Mass", "Mass Concentration")

  result <- validateUnitConversionTibble(unitConversionTibble, validUnitsList, validQuantitiesList)

  unitConversionTibble <- result$unitConversionTibble

  # Check that we have 10 rows (first row removed and added back, so same count)
  expect_equal(nrow(unitConversionTibble), 10)

  # Row 1 (first row): "-", "NA", "NA", "1" - should have no errors
  expect_equal(unitConversionTibble$tmpvalidationMessages[1], "")

  # Row 2: "Arbitrary Concentration", "g", "au/ml", "1" - should have no errors
  expect_equal(unitConversionTibble$tmpvalidationMessages[2], "")

  # Row 3: "invalid", "au/ml", "au/ml", "1" - invalid omop_quantity
  expect_true(stringr::str_detect(unitConversionTibble$tmpvalidationMessages[3], "ERROR: omop_quantity not in validQuantitiesList"))

  # Row 4: "Arbitrary Concentration", "invalid", "au/ml", "1" - invalid source_unit_valid
  expect_true(stringr::str_detect(unitConversionTibble$tmpvalidationMessages[4], "ERROR: source_unit_valid not in validUnitsList"))

  # Row 5: "Arbitrary Concentration", "au/ml", "invalid", "1" - invalid to_source_unit_valid
  expect_true(stringr::str_detect(unitConversionTibble$tmpvalidationMessages[5], "ERROR: to_source_unit_valid not in validUnitsList"))

  # Row 6: "Arbitrary Concentration", "au/ml", "NA", "1" - empty to_source_unit_valid
  expect_true(stringr::str_detect(unitConversionTibble$tmpvalidationMessages[6], "ERROR: to_source_unit_valid is empty"))

  # Row 7: "Arbitrary Concentration", "au/ml", "NA", "1" - empty to_source_unit_valid
  expect_true(stringr::str_detect(unitConversionTibble$tmpvalidationMessages[7], "ERROR: to_source_unit_valid is empty"))

  # Row 8: "Arbitrary Concentration", "g", "au/ml", "NA" - invalid conversion (empty)
  expect_true(stringr::str_detect(unitConversionTibble$tmpvalidationMessages[8], "ERROR: conversion is not valid"))

  # Row 9: "Arbitrary Concentration", "g", "au/ml", "10.93*X-23.50" - should have no errors (valid formula)
  expect_equal(unitConversionTibble$tmpvalidationMessages[9], "")

  # Row 10: "Arbitrary Concentration", "g", "au/ml", "invalid" - invalid conversion
  expect_true(unitConversionTibble$tmpvalidationMessages[10] == "")

  # Check that validation log has errors
  result$validationLogR6$logTibble |> dplyr::filter(type == "ERROR") |> nrow() |> expect_gt(0)

})
