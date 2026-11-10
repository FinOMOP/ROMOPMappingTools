# Finnish names from Usagi files to `CONCEPT_SYNONYM`

The optional Usagi column `ADD_INFO:sourceName_fi` is carried through the STCM table into the CDM `CONCEPT_SYNONYM` table as a Finnish synonym of the source concept.

## Decisions
- **Finnish only.** `language_concept_id` is hardcoded to 4181730. There is no parameter yet.
- **New STCM column.** `source_code_description_fi` carries the name, as for the other extended columns.
- **Empty name is a warning.** The column is optional, so a missing name never blocks a mapping.
- **Same name as `sourceName` is skipped silently.** It is neither warned about nor added as a synonym. 11 ICD10fi Latin terms are affected.
- **No blank-name rule.** readr already turns whitespace-only names into NA, so they count as empty.
- **Post-load check against the STCM table.** The STCM table is already filtered by mappingStatus, so it is a better reference than the Usagi files.
- **Minimal fixture change.** Only concept 4181730 was added to the test vocabulary. The full vocabulary was not regenerated.

## Data flow

```
Usagi CSV                 STCM (extended)                CDM
ADD_INFO:sourceName_fi -> source_code_description_fi -> CONCEPT_SYNONYM
                                                         concept_id           = source_concept_id
                                                         concept_synonym_name = source_code_description_fi
                                                         language_concept_id  = 4181730 (Finnish)
```

- The language is hardcoded to Finnish (`4181730`).
- A synonym is added only when the name is not empty and differs from `sourceName`. Names equal to `sourceName` are skipped silently, with no warning.
- Only rows that reach the STCM table become synonyms: mappingStatus APPROVED, UNCHECKED or INVALID_TARGET (`includeConcepts`).

## Changes by step

### 1. Reading and writing Usagi files (`R/usagiFile.R`)
- `readUsagiFile()` reads `ADD_INFO:sourceName_fi` as character. readr trims whitespace and reads `""` as NA, so blank names become NA.
- `writeUsagiFile()` places the column after `ADD_INFO:sourceParentVocabulary` and writes empty strings as NA.

### 2. Validation (`R/validateUsagiFile.R`)
- If `ADD_INFO:sourceName_fi` is present, the C&CR columns (`sourceConceptId`, `sourceConceptClass`, `sourceDomain`) are required, as for the other extended columns.
- New checks:

| Step in the log | Level | Rule |
|---|---|---|
| `SourceNameFi is more than 1000 characters` | ERROR | Fits `concept_synonym_name varchar(1000)` |
| `SourceNameFi is not the same for all rows of the sourceCode` | ERROR | Multi-mapped codes must repeat the same Finnish name |
| `SourceNameFi is empty` | WARNING | Missing name for an APPROVED or UNCHECKED row |

- ERRORs set the row to FLAGGED, like the other checks. The WARNING only adds a message to `ADD_INFO:validationMessages`.

### 3. STCM table
- `inst/ddl/5.4/sql_server/createSourceToConceptMapExtended.sql`: new column `source_code_description_fi varchar(1000) NULL`.
- `R/appendUsagiFileToSTCMtable.R`:
  - The column is part of the extended STCM columns.
  - It is filled from `ADD_INFO:sourceName_fi`, or with NA when the Usagi file doesn't have that column.
- An extended STCM table created before this change lacks the column and must be recreated with `createSourceToConceptMapExtended()`.

### 4. STCM to CDM (`inst/sql/sql_server/STCMExtendedToCDM.sql`, section 5)
- DELETE removes Finnish synonyms (`language_concept_id = 4181730`) for the `source_concept_id`s in the STCM table. Synonyms in other languages are not touched.
- INSERT adds one row per distinct `(source_concept_id, source_code_description_fi)`, skipping rows where:
  - `source_concept_id = 0`
  - the name is NULL or blank
  - the name equals `source_code_description`
- Re-running it gives the same result.

### 5. Post-load check (`R/buildVocabuliesAll.R`)
- After `STCMToCDMTables()`, a new log step, `Finnish names in CONCEPT_SYNONYM`, compares two counts:
  - the expected names in the STCM table, using the same filters as the SQL;
  - the Finnish rows in `CONCEPT_SYNONYM`.
- It logs SUCCESS if the counts match and ERROR otherwise.

### 6. Other
- `R/calculateMappingStatus.R`: changed `language_concept_id == 4181730` to standard SQL `=`. This query fills `concept_name_fi` in the status reports, so it now also picks up synonyms for the custom vocabularies.
- Docs: roxygen comments, Rd files (edited by hand because roxygen2 isn't installed locally; run `devtools::document()` to confirm) and NEWS.md.

## Tests
- `test-validateUsagiFile.R`:
  - The too-long, differing-within-sourceCode and empty checks each fire.
  - The C&CR columns are required when `sourceName_fi` is present.
- `test-appendUsagiFileToSTCMTable.R`:
  - The new column appears in the extended STCM table and gets its value.
  - It is NA when the Usagi file has no Finnish column.
- `test-STCMToCDMTable.R`:
  - Synonyms are created with language 4181730, including for multi-mapped and unmapped codes.
  - Rows with an empty name, a blank name, a name equal to the concept name, or `source_concept_id = 0` are skipped.
  - Running it twice gives the same result.
  - An existing English synonym is preserved.
- `test-validateVocabularyFolder.R`: the valid test folder gives one expected WARNING. UNITfi has the `sourceName_fi` column with all values empty.
- `test-buildVocabulariesAll.R`: the step `Finnish names in CONCEPT_SYNONYM` is SUCCESS.
- Test vocabulary: added concept 4181730 (Finnish language) to `OMOPVocabulary.duckdb`, `CONCEPT.csv` (gitignored) and `createTestData.R`. Otherwise DQD fails the `CONCEPT_SYNONYM.language_concept_id` foreign-key check.

Full suite: 27 tests, 0 failures, 0 errors. Run with `Rscript --no-init-file`, because the renv library isn't installed for R 4.6.

## Files changed
- `R/`:
  - `usagiFile.R`, `validateUsagiFile.R`, `appendUsagiFileToSTCMtable.R`
  - `createSourceToConceptMapExtended.R`, `buildVocabuliesAll.R`, `calculateMappingStatus.R`
- `inst/`:
  - `ddl/5.4/sql_server/createSourceToConceptMapExtended.sql`
  - `sql/sql_server/STCMExtendedToCDM.sql`
  - `testdata/OMOPVocabulary/OMOPVocabulary.duckdb`
  - `testdata/createTestData.R`
- `man/`:
  - `readUsagiFile.Rd`, `writeUsagiFile.Rd`, `validateUsagiFile.Rd`
  - `appendUsagiFileToSTCMtable.Rd`, `createSourceToConceptMapExtended.Rd`
- `tests/testthat/`:
  - `test-validateUsagiFile.R`, `test-appendUsagiFileToSTCMTable.R`, `test-STCMToCDMTable.R`
  - `test-validateVocabularyFolder.R`, `test-buildVocabulariesAll.R`
- `NEWS.md`
