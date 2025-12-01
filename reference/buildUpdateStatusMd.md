# Build Update Status Markdown

Creates a markdown file containing the update status results. The
markdown file includes a table showing update results with:

- Context of the update

- Type of message (SUCCESS, WARNING, ERROR)

- Update step

- Detailed message

## Usage

``` r
buildUpdateStatusMd(
  updateLogTibble,
  pathToUpdateStatusMdFile = file.path(tempdir(),
    "VOCABULARIES_LAST_AUTOMATIC_UPDATE_STATUS.md")
)
```

## Arguments

- updateLogTibble:

  A tibble containing update results with columns: context, type, step,
  and message

- pathToUpdateStatusMdFile:

  Path where the markdown file will be saved (default:
  tempdir()/VOCABULARIES_LAST_AUTOMATIC_UPDATE_STATUS.md)

## Value

Path to the generated markdown file
