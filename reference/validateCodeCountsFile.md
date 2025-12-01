# Validate Code Counts Folder

Validates all code count files in a code counts folder against a set of
rules. The folder must contain a vocabularies_coverage.csv file that
describes which vocabularies are covered and a databases_coverage.csv
file that describes which databases are covered.

## Usage

``` r
validateCodeCountsFile(pathToCodeCountsFile)
```

## Arguments

- pathToCodeCountsFile:

  Path to the CSV file containing the database code counts.

## Value

A tibble containing validation results for all files
