
# ROMOPMappingTools

<!-- badges: start -->
[![R-CMD-check](https://github.com/FinOMOP/ROMOPMappingTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinOMOP/ROMOPMappingTools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

![](https://github.com/FinOMOP/ROMOPMappingTools/assets/54809193/6c195491-eceb-447c-86c9-86d66b8ee63d)

ROMOPMappingTools provides functions to validate, update, and summarize multiple Usagi mapping files, as well as to transform them into the OMOP vocabulary tables. 

Functions can be use at three different levels: 

1. Working with individual mapping files
   - Validate Usagi mapping files: conceptIds exist in the OMOP vocabulary, domain combinations are correct, etc.
   - Update Usagi mapping : Automatically fixes mapping in that become obsolete due to new OMOP vocabulary releases.

2. Working with multiple mapping files
   - Validate, and update multiple mapping files at once.
   - Transform and append a group of Usagi mapping files to OMOP vocabulary tables: Concept, concept_relationship, and concept_ancestor tables.
   - Run DQD on the new OMOP vocabulary tables.
   - Build a status dashboard to show the progress of multiple mapping files.
  
3. Working with in multiple mapping files in a github repository
   - Create github action workflows to automatically validate Usagi files in each pull request.
   - Create github action workflows to automatically update the progress dashboard when a pull request is merged.

[See vignettes for details](https://finomop.github.io/ROMOPMappingTools/). 
[See FimOMOP repository for an example of how to use the package in a github repository](https://github.com/FinOMOP/FinOMOP_mappings).
