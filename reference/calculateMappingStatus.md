# calculateMappingStatus

Calculates the mapping status for code counts in different databases and
vocabularies.

## Usage

``` r
calculateMappingStatus(
  pathToCodeCountsFolder,
  connectionDetails,
  vocabularyDatabaseSchema,
  includeCountForAllDatabases = TRUE,
  skipValidation = TRUE
)
```

## Arguments

- pathToCodeCountsFolder:

  The path to the vocabularies coverage file.

- connectionDetails:

  The connection details for the OMOP tables.

- vocabularyDatabaseSchema:

  The database schema for the OMOP vocabulary.

- includeCountForAllDatabases:

  Logical value indicating whether to include code counts for all
  databases (default is TRUE).

- skipValidation:

  Logical value indicating whether to skip validation of the input
  parameters (default is TRUE).

## Value

A list containing two tables: `concepts_to_match` and
`code_counts_matched`, representing the mapping status for code counts.

## Details

This function calculates the mapping status for code counts in different
databases and vocabularies. The calculation involves the following
steps:

1.  Validating the input parameters to ensure they meet the required
    format.

2.  Reading and validating the provided vocabularies coverage file.

3.  Creating a table called `concepts_to_match` that collects concepts
    in OMOP with mapping relationships and synonyms.

4.  Creating a table called `all_code_counts` that contains all code
    counts in the vocabularies coverage, optionally ignoring databases
    with failed rules.

5.  Creating a new database called "all_databases" with normalized event
    counts across all databases.

6.  Calculating the percentage of codes per database and vocabulary.

7.  Calculating the mapping status for code counts based on whether they
    have mappings and codes.

8.  Returning the mapping status information in the form of two tables:
    `concepts_to_match` and `code_counts_matched`.
