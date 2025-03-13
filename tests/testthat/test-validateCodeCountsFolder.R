test_that("validateCodeCountsFolder returns all SUCCESS for valid code counts folder", {
    pathToCodeCountsFolder <- system.file("testdata/CODE_COUNTS", package = "ROMOPMappingTools")
    
    # Run validation
    validationsLogTibble <- validateCodeCountsFolder(
        pathToCodeCountsFolder
    )

    validationsLogTibble |> dplyr::filter(type != "SUCCESS") |> nrow() |> expect_equal(0)
})


test_that("validateCodeCountsFile errors with the wrong file", {
    pathToCodeCountsFile <- system.file("testdata/CODE_COUNTS/databases/codeCountsWithErrors.csv", package = "ROMOPMappingTools")

    validationsLogTibble <- validateCodeCountsFile(pathToCodeCountsFile)
    validationsLogTibble |> 
    dplyr::filter(step == "source_vocabulary_id and source_code are unique") |> nrow() |> expect_equal(1)

    validationsLogTibble |> 
    dplyr::filter(step == "n_events is more than 5 or minus one") |> nrow() |> expect_equal(1)

})


