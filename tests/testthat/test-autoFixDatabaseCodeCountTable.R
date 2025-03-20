
test_that("validateCodeCountsFile errors with the wrong file", {
    pathToCodeCountsFile <- system.file("testdata/CODE_COUNTS/databases/codeCountsWithErrors.csv", package = "ROMOPMappingTools")

    pathToCodeCountsFileFixed <- tempfile(fileext = ".csv")

    fixesLogTibble <- autoFixDatabaseCodeCountsFile(
        pathToCodeCountsFile,
        pathToCodeCountsFileFixed
    )

    fixesLogTibble |> 
    dplyr::filter(step == "sum repeated codes") |> nrow() |> expect_equal(1)

    fixesLogTibble |> 
    dplyr::filter(step == "set n_events less than 5 to -1") |> nrow() |> expect_equal(1)

    validationLogTibble <- validateCodeCountsFile(pathToCodeCountsFileFixed)
    validationLogTibble |> 
    dplyr::filter(type != "SUCCESS") |> nrow() |> expect_equal(0)
})
