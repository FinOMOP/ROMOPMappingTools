#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
usethis::use_tibble()

## Add one line by package you want to add as dependency
usethis::use_package( "dplyr" )
usethis::use_package( "tidyr" )
usethis::use_package( "tibble" )
usethis::use_package( "stringr")
usethis::use_package( "tibble")
usethis::use_package( "purrr")
usethis::use_package( "readr")
usethis::use_package( "lubridate")
usethis::use_package( "ggplot2")
usethis::use_package("reactable")
usethis::use_package("reactablefmtr")
usethis::use_package("validate")



## DATA
usethis::use_data_raw("tables_valid_format")


## FUNCTIONS
usethis::use_r("readTable")
usethis::use_r("validateTable")
usethis::use_r("validateTables")



usethis::use_r("importMappingTables")
usethis::use_r("convertMappingsTablesToOMOPtables")


usethis::use_r("mergeOMOPtables")

usethis::use_r("importOMOPtables")

usethis::use_r("importDatabasesCodeCountsTables")


usethis::use_r("calculateMappingStatus")

# VISUAL
usethis::use_r("plotTableMappingStatus")
usethis::use_r("plotTableVocabularyStatus")

usethis::use_r("buildStatusDashboard")





usethis::use_r("readMappingVocabulariesInfoFile")
usethis::use_test("readMappingVocabulariesInfoFile")

usethis::use_r("readFilesFromMappingVocabulariesInfoTibble")



usethis::use_r("importDatabasesCodeCounts")

usethis::use_r("calculateMappingStatus")




usethis::use_r("readUsagiCCRFile")

usethis::use_r("readVocabularyInfoFile")

usethis::use_r("convertVocabularyMappingsToOMOPtables")




usethis::use_r("readVocabulariesInfo")
usethis::use_r("readVocabularyMappings")

usethis::use_r("validateOMOPtables")

usethis::use_r("readOMOPtables")
usethis::use_r("mergeOMOPtables")

usethis::use_r("mapOMOPtoCodesFrequencies")

### data visualization
usethis::use_r("tableMappingStatus")
usethis::use_r("tableVocabularyStatus")
usethis::use_r("buildMappingStatusDashboard")


## TEST
usethis::use_test("validateUsagiFile")

usethis::use_test("readVocabulariesInfo")
usethis::use_test("readVocabularyMappings")
usethis::use_test("convertVocabularyMappingsToOMOPtables")

usethis::use_test("readOMOPtables")
#usethis::use_r("mergeOMOPtables")

usethis::use_test("importCodesFrequencies")# to do
#usethis::use_r("mapOMOPtoCodesFrequencies")



## VIGNETTES
usethis::use_vignette("run_demo")
