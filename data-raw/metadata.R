# metadata.R
# This script generates the rule-metadata.csv file, which is ingested into the checklist in checklist.R
# Based on work in my 20220901 notebook.

library(readxl)

read_data_inventory <- function(xlsx_file, xlsx_sheet = "validation rules") {
  # Data Inventory excel file
  invntry <- read_excel(xlsx_file, sheet = xlsx_sheet) %>% # Replaces "" cells with NA!
    select(
      ushe_rule = `USHE Validation Rule(s)`,
      description = `Validation Rule`,
      type = `Validation Type`,
      status = `Validation Status`,
      steward = `Data Steward`,
      # ushe_element = `USHE Element(s)`,
      # ushe_file = `USHE File(s)`,
      # ushe_field = `USHE Field`,
      ushe_notes = `USHE Notes`,
      # ushe_sql = `USHE Sample SQL`,
      activity_date = `Activity Date`,
      banner = Banner #,
      # edify = Edify
    )

  # One of those \r\n Windows issues (does excel always do that?)
  out <- invntry %>%
    mutate(across(where(is.character), ~ gsub("\r\n", "\n", .)))

  out
}

# Produce a `ushe_rule` list-column enumerating all rules defined in each row
unnest_metadata <- function(df) {
  df %>%
    mutate(ushe_rule = map(ushe_rule, ~unlist(str_split(., pattern = ", "))),
           ref_rule = map_chr(ushe_rule, ~`[`(., 1))) %>%  # for left_joining to rule_spec
    unnest(cols = ushe_rule) %>% # Produces one row per element of ushe_rule list column
    mutate(activity_date = ifelse(activity_date == "n/a", NA_character_, activity_date))
}


# Note that I've made the following changes manually in the xlsx (in addition to others):
# - S21b and related rules should be USHE
# - C14c should be USHE
# - Replacing all occurrences of space_utlz.building_detail. bldg_activity_date" with "building_activity_date"
# - Replacing all occurrences of "space_utlz.room_activity_date" with "room_activity_date"

data_inventory_xlsx <- "sandbox/State Report Data Inventory annotated(AutoRecovered).xlsx"
metadf_i <- read_data_inventory(data_inventory_xlsx) %>%
  `[`(-61, ) %>% # removed redundant S18b rule (keeping undergrad, ditching grad)
  unnest_metadata() %>%
  filter(!(ushe_rule == "SC11a" & ref_rule == "S20a")) %>% # Remove redundant SC11a rule
  mutate(ushe_rule = ifelse(ushe_rule == "24a", "S24a", ushe_rule), # fix apparent typo omitting "S"
         ushe_rule = ifelse(ushe_rule == "23d", "S23d", ushe_rule)) %>%  # fix apparent typo omitting "S"
  glimpse()


write.csv(metadf_i, "sandbox/rule-metadata.csv", row.names = FALSE)
