# Running the complete mapping process

``` r
library(ROMOPMappingTools)
```

## Introduction

The `runAll` function provides a streamlined way to execute the complete
mapping process: 1. Validates all Usagi files in a vocabulary folder 2.
Creates and populates the SOURCE_TO_CONCEPT_MAP_EXTENDED table 3.
Creates CDM vocabulary tables from the mappings 4. Validates the
resulting CDM tables using DataQualityDashboard
