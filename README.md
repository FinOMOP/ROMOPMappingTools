
# ROMOPMappingTools

<!-- badges: start -->
<!-- badges: end -->

![](https://github.com/FinOMOP/ROMOPMappingTools/assets/54809193/6c195491-eceb-447c-86c9-86d66b8ee63d)

ROMOPMappingTools provides the functions needed to maintain a repository with mappings of local medical vocabularies to OMOP standard vocabularies. 

It allows to : 

- Transform multipe mapping files in usagi-extended format to the OMOP vocabulary tables
- Append created OMOP vocabulary tables to OMOP vocabulary tables downloaded from Athena
- Run QC on the input an output files
- Given the code's frequency from one or more local databases calculates how well a OMOP vocabulary covers the medical events
- Creates a interactive dashboard to show the above resuls

This package contains only the functions to process the input files. 
How the files and repository are organised is independent of this package. 

As an example we show an demo version of the repo and folder structure we use for mataining the mapping files in FinOMOP:
[FinOMOP_OMOP_vocabulary_test](https://github.com/FinOMOP/FinOMOP_OMOP_vocabulary_test). Check also the documentation part: [FinOMOP_OMOP_vocabulary_test/DOCUMENTATION](https://github.com/FinOMOP/FinOMOP_OMOP_vocabulary_test/blob/development/DOCUMENTATION/README.md)



See vignettes for help. 


