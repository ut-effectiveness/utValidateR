## code to prepare `checklist` dataset goes here

# Rule specification

library(tibble)
library(dplyr)
library(rlang)
library(tidyr)
library(purrr)
library(stringr)

# Currently a partial rule set for demonstration purposes.
rule_spec <- tribble(
  ~rule,  ~checker,
  "S00a", expr(!is_duplicated(student_id)),
  "S00b", expr(!is_duplicated(ssn)),
  "S03a", expr(!is.na(student_id) & !is.na(ssn)),
  "S03b", expr(!is.na(ssn)),
  "S03c", expr((us_citizenship_code != 1) | !is.na(ssn)),
  "S04b", expr(matches_regex(ssn, ssn_regex)),
  "S06a", expr(!is.na(last_name)),
  "S06b", expr(!has_nonalpha(last_name)),
  "S06c", expr(!is.na(first_name)),
  "S06d", expr(!has_nonalpha(first_name)),
  "S06e", expr(is.na(middle_name) | !has_nonalpha(middle_name)),
  "S07a", expr(is.na(previous_last_name) | !has_nonalpha(previous_last_name)),
  "S07b", expr(is.na(previous_first_name) | !has_nonalpha(previous_first_name)),
  "S08a", expr(!is.na(local_address_zip_code)),
  "S09a", expr(!is.na(us_citizenship_code)),
  "S10a", expr(!is.na(first_admit_country_code))
)

# dataframe with rule info from Data Inventory
all_rules <- read.csv("sandbox/full-rules-rename.csv") %>%
  mutate(ushe_rule = map(ushe_rule, ~unlist(str_split(., pattern = ", ")))) %>%
  unnest(cols = ushe_rule) %>%
  mutate(activity_date = ifelse(activity_date == "n/a", NA_character_, activity_date)) %>%
  select(rule = ushe_rule, description, status, activity_date) %>%
  glimpse()

# Rule info joined to anonymous-function tibble
checklist <- all_rules %>%
  inner_join(rule_spec, by = "rule") %>%
  glimpse()


usethis::use_data(checklist, overwrite = TRUE)
