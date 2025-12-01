# Validate Code Counts Folder

Validates all code count files in a code counts folder against a set of
rules. The folder must contain a vocabularies_coverage.csv file that
describes the vocabularies and their coverage, and a
databases_coverage.csv file that describes the databases and their
corresponding code count files.

## Usage

``` r
validateCodeCountsFolder(pathToCodeCountsFolder)
```

## Arguments

- pathToCodeCountsFolder:

  Path to folder containing code count files

## Value

A tibble containing validation results for all files
