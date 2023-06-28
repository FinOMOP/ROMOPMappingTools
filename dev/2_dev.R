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

usethis::use_package("flexdashboard")

usethis::use_package('DataQualityDashboard')
usethis::use_package('DatabaseConnector')
usethis::use_package('SqlRender')
usethis::use_package('duckdb')
usethis::use_package('rmarkdown')
usethis::use_package('shiny')

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

usethis::use_r("autoFixDatabaseCodeCountTable")


usethis::use_r("calculateMappingStatus")


usethis::use_r("createTemporalDatabaseWithOMOPtable")
usethis::use_r("validateOMOPtablesWithDQD")

# VISUAL
usethis::use_r("plotTableMappingStatus")
usethis::use_r("plotTableVocabularyStatus")

usethis::use_r("buildStatusDashboard")






## VIGNETTES
usethis::use_vignette("run_demo")
usethis::use_vignette("files_description")
