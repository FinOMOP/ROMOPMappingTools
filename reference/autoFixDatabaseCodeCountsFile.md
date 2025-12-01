# autoFixDatabaseCodeCountTable

Automatically fixes issues in the database code count table.

## Usage

``` r
autoFixDatabaseCodeCountsFile(
  pathToCodeCountsFile,
  pathToCodeCountsFileFixed = pathToCodeCountsFile,
  keepOnlySourceVocabularyIds = NULL
)
```

## Arguments

- pathToCodeCountsFile:

  Path to the CSV file containing the database code counts.

- pathToCodeCountsFileFixed:

  Path where the fixed CSV file should be saved. Defaults to overwriting
  the input file.

- keepOnlySourceVocabularyIds:

  Vector of source vocabulary IDs to keep. Other source vocabularies
  will be removed. If NULL, keeps all vocabularies.

## Value

Tibble containing the fixed database code count table.

## Details

This function performs the following fixes on the provided database code
count table:

1.  Removes unused source vocabularies based on the specified
    `keep_only_source_vocabulary_ids`.

2.  Sums up repeated values for the same source vocabulary and source
    code.

3.  Removes events with a count less than 5.
