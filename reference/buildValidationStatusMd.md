# Build Validation Status Markdown

Creates a markdown file containing the validation status results. The
markdown file includes a table showing validation results with:

- Context of the validation

- Type of message (SUCCESS, WARNING, ERROR)

- Validation step

- Detailed message

## Usage

``` r
buildValidationStatusMd(
  validationLogTibble,
  pathToValidationStatusMdFile = file.path(tempdir(),
    "VOCABULARIES_VALIDATION_STATUS.md")
)
```

## Arguments

- validationLogTibble:

  A tibble containing validation results with columns: context, type,
  step, and message

- pathToValidationStatusMdFile:

  Path where the markdown file will be saved (default:
  tempdir()/VOCABULARIES_VALIDATION_STATUS.md)

## Value

Path to the generated markdown file
