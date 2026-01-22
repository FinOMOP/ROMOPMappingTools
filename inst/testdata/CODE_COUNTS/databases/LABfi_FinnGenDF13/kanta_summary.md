# kanta_summary.duckdb

**Table: summaryTest**

Summary for the kanta `TEST_NAME` + `MEASUREMENT_UNIT` pairs

* `OMOP_CONCEPT_ID`: Integer, OMOP concept ID mapped to the `TEST_NAME` + `MEASUREMENT_UNIT` pair
* `TEST_NAME`: Character, lab test name
* `MEASUREMENT_UNIT`: Character, lab test unit
* `MEASUREMENT_UNIT_HARMONIZED`: Character, lab test unit harmonised unit
* `omopQuantity`: Character, quantity related to OMOP measurements
* `CONVERSION_FACTOR`: Character, conversion factors for values to harmonized value
* `n_records`: Integer, number of records for each `TEST_NAME` + `MEASUREMENT_UNIT` pair
* `n_subjects`: Integer, number of patients for each `TEST_NAME` + `MEASUREMENT_UNIT` pair (removed if <=5)


**Table: summaryValuesSource**

Value source distributions for the `TEST_NAME` + `MEASUREMENT_UNIT` pairs

* `OMOP_CONCEPT_ID`: Integer, OMOP concept ID mapped to the `TEST_NAME` + `MEASUREMENT_UNIT` pair
* `TEST_NAME`: Character, lab test name
* `MEASUREMENT_UNIT`: Character, lab test unit
* `value_source`: Character, values coming from source or extracted
* `n_subjects`: Integer, number of subjects (removed if <=5)
* `n_records`: Integer, number of records


**Table: summaryValues**

Value distributions for the `TEST_NAME` + `MEASUREMENT_UNIT` pairs

* `OMOP_CONCEPT_ID`: Integer, OMOP concept ID mapped to the `TEST_NAME` + `MEASUREMENT_UNIT` pair
* `TEST_NAME`: Character, lab test name
* `MEASUREMENT_UNIT`: Character, lab test unit
* `n_subjects`: Integer, number of subjects (removed if <=5)
* `n_records`: Integer, number of records
* `decile`: Numeric, decile value
* `decile_MEASUREMENT_VALUE`: Numeric, decile of measurement value
* `decile_MEASUREMENT_VALUE_HARMONIZED`: Numeric, decile of harmonized measurement value


**Table: summaryOutcomes**

Outcome distributions for the `TEST_NAME` + `MEASUREMENT_UNIT` pairs

* `OMOP_CONCEPT_ID`: Integer, OMOP concept ID mapped to the `TEST_NAME` + `MEASUREMENT_UNIT` pair
* `TEST_NAME`: Character, lab test name
* `MEASUREMENT_UNIT`: Character, lab test unit
* `TEST_OUTCOME`: Character, test outcome
* `n_TEST_OUTCOME`: Integer, number of test outcomes
* `n_subjects`: Integer, number of subjects (removed if <=5)
* `n_records`: Integer, number of records
