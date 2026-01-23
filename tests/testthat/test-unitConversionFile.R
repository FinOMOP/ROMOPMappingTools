# test_that("test validateConversionTibble returns no errors with a valid quantity_source_unit_conversion.tsv file", {
#   pathToUnitConversionFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/quantity_source_unit_conversion.tsv", package = "ROMOPMappingTools")
#   unitConversionTibble <- readUnitConversionFile(pathToUnitConversionFile)

#   pathToValidUnitsFile <- system.file("testdata/VOCABULARIES/UNITfi/UNITfi.usagi.csv", package = "ROMOPMappingTools")
#   validUnitsList <- readUsagiFile(pathToValidUnitsFile) |>
#     dplyr::filter(`ADD_INFO:UniqueForLab` == TRUE) |>
#     dplyr::pull(sourceCode) |>
#     unique() |>
#     na.omit()

#   validQuantitiesList <- unitConversionTibble$omop_quantity |>
#     unique() |>
#     na.omit()

#   result <- validateUnitConversionTibble(unitConversionTibble, validUnitsList, validQuantitiesList)

#   result$validationLogR6$logTibble |>
#     dplyr::filter(type == "ERROR") |>
#     nrow() |>
#     expect_equal(0)
    
#   result$unitConversionTibble |>
#     dplyr::filter(validation_messages != "") |>
#     nrow() |>
#     expect_equal(0)
# })

test_that("test validateConversionTibble returns errors with a invalid quantity_source_unit_conversion.tsv file", {
  pathToUnitConversionFile <- system.file("testdata/VOCABULARIES/LABfi_ALL/quantity_source_unit_conversion with_errors.tsv", package = "ROMOPMappingTools")
  unitConversionTibble <- readUnitConversionFile(pathToUnitConversionFile)
  pathToValidatedUnitConversionFile <- tempfile(fileext = ".tsv")

  validUnitsList <- c("au/ml", "g", "mg", "kg", "ng")
  validQuantitiesList <- c("Arbitrary Concentration", "Mass", "Mass Concentration")

  result <- validateUnitConversionTibble(unitConversionTibble, validUnitsList, validQuantitiesList)

  unitConversionTibble <- result$unitConversionTibble

  # Check that we have 10 rows (first row removed and added back, so same count)
  expect_equal(nrow(unitConversionTibble), 9)

  # Row 1 (first row): "-", "NA", "NA", "1" - should have no errors
  expect_equal(unitConversionTibble$validation_messages[1], "")

  # Row 2: "Arbitrary Concentration", "g", "au/ml", "1" - should have no errors
  expect_equal(unitConversionTibble$validation_messages[2], "")

  # Row 3: "invalid", "au/ml", "au/ml", "1" - invalid omop_quantity
  expect_true(stringr::str_detect(unitConversionTibble$validation_messages[3], "ERROR: omop_quantity not in validQuantitiesList"))

  # Row 4: "Arbitrary Concentration", "invalid", "au/ml", "1" - invalid source_unit_valid
  expect_true(stringr::str_detect(unitConversionTibble$validation_messages[4], "ERROR: source_unit_valid not in validUnitsList"))

  # Row 5: "Arbitrary Concentration", "au/ml", "invalid", "1" - invalid to_source_unit_valid
  expect_true(stringr::str_detect(unitConversionTibble$validation_messages[5], "ERROR: to_source_unit_valid not in validUnitsList"))

  # Row 6: "Arbitrary Concentration", "au/ml", "NA", "1" - empty to_source_unit_valid
  expect_true(unitConversionTibble$validation_messages[6] == "")

  # Row 8: "Arbitrary Concentration", "g", "au/ml", "NA" - invalid conversion (empty)
  expect_true(stringr::str_detect(unitConversionTibble$validation_messages[7], "ERROR: conversion is not valid"))

  # Row 8: "Arbitrary Concentration", "kg", "au/ml", "10.93*X-23.50" - should have no errors (valid formula)
  expect_equal(unitConversionTibble$validation_messages[8], "")

  # Row 9: "Arbitrary Concentration", "ng", "au/ml", "invalid" - invalid conversion
  expect_true(stringr::str_detect(unitConversionTibble$validation_messages[9], "ERROR: conversion is not valid"))

  # Check that validation log has errors
  result$validationLogR6$logTibble |>
    dplyr::filter(type == "ERROR") |>
    nrow() |>
    expect_gt(0)
})
