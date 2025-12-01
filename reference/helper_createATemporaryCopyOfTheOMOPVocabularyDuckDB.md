# Create a Temporary Copy of the OMOP Vocabulary DuckDB

This function creates a temporary copy of the OMOP vocabulary DuckDB
file from the package's test data.

## Usage

``` r
helper_createATemporaryCopyOfTheOMOPVocabularyDuckDB(
  pathToFullOMOPVocabularyCSVsFolder = NULL
)
```

## Arguments

- pathToFullOMOPVocabularyCSVsFolder:

  A character string containing the path to the full OMOP vocabulary
  CSVs folder.

## Value

A character string containing the path to the temporary copy of the
DuckDB file
