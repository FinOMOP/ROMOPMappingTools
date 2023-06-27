######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()

# # pre build vignette localy
# options(rmarkdown.html_vignette.check_title = FALSE)
# knitr::knit("vignettes/run_on_demo_repo.Rmd.orig", output = "vignettes/run_on_demo_repo.Rmd")
# devtools::build_rmd("vignettes/run_on_demo_repo.Rmd")
# browseURL("vignettes/run_on_demo_repo.html")
#
#
# # increase version
pkgdown::build_site()



#rhub::check_for_cran()
usethis::use_version()

gert::git_push()
