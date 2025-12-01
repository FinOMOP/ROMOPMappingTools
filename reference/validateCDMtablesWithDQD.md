# Validate CDM Tables Using Data Quality Dashboard

Validates the CDM vocabulary tables using the OHDSI Data Quality
Dashboard. This performs standard CDM vocabulary table checks to ensure
data quality and consistency.

## Usage

``` r
validateCDMtablesWithDQD(
  connectionDetails,
  vocabularyDatabaseSchema,
  validationResultsFolder
)
```

## Arguments

- connectionDetails:

  DatabaseConnector connection details object

- vocabularyDatabaseSchema:

  Schema name where the vocabulary tables are stored

- validationResultsFolder:

  Folder where validation results will be saved

## Value

A tibble containing validation results
