library(dplyr)
library(ellmer)

Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = '/Users/javier/keys/atlas-development-270609-410deaacc58b.json')

chat  <- ellmer::chat_google_vertex(
  location = "europe-north1",
  project_id = "atlas-development-270609",
  system_prompt = NULL,
  model = "gemini-2.5-pro"
)


summary  <- readr::read_tsv("tests/manualtest/lab_data_summary.csv", na = "")
summary |> glimpse()


summary  <- readr::read_tsv("tests/manualtest/lab_data_summary.csv", na = "")
finnish_code <- summary |> dplyr::pull(testId) |> head(1)

prompt <- interpolate_file(
    path = "tests/manualtest/prompt.md",
    finnish_code = finnish_code
)

output_structure <- type_object(
  .description = "Extract list of ingredients ",
  components = type_string(description = "The NPU components as: System - Component; kind-of-property."),
  loinc = type_string(description = "The LOINC name if found"),
  no_loinc = type_string(description = "Explanation why no LOINC equivalent was found"),
  summary = type_string(description = "Summary of the search process, in few lines")
)

structured_recipe <- chat$chat_structured(
  prompt,
  type = output_structure
)


print(structured_recipe)
chat$get_cost()



# Parallel 
set.seed(13)

summary_n   <-  summary |>
select(status, OMOP_CONCEPT_ID, concept_name, omopQuantity, testId) |>
dplyr::filter(status == "APPROVED") |>
dplyr::sample_n(10) 

prompts <- interpolate_file(
    path = "tests/manualtest/prompt.md",
    finnish_code = summary_n$testId
)

response  <- parallel_chat_structured(
    chat = chat,
    prompts = prompts,
    type = output_structure
)



bind_cols(summary_n, response |> as_tibble()) |>
transmute(testId, concept_name, loinc, yes = (concept_name==loinc), components, no_loinc, summary)  |> 
View()



