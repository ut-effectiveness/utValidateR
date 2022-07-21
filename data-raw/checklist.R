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
  # "S02a", expr(!is_duplicated(ushe_academic_year) &
  #                !is_duplicated(version_id) &
  #                !is_duplicated(season)), # TODO: check once we have dummy data
  "S03a", expr(!is.na(student_id) & !is.na(ssn)),
  "S03b", expr(!is.na(ssn)),
  "S03c", expr((us_citizenship_code != 1) | !is.na(ssn)),
  # "S04a", TODO: what is the "id flag" that every student should have?
  "S04b", expr(is_valid_ssn(ssn)),
  # "S04c", TODO: sort out id flag for rule logic
  # "S04d", TODO: sort out id flag for rule logic
  "S05a", expr(is_valid_previous_id(previous_student_id)),
  "S06a", expr(!is.na(last_name)),
  "S06b", expr(is_alpha_chr(last_name)),
  "S06c", expr(!is.na(first_name)),
  "S06d", expr(is_alpha_chr(first_name)),
  "S06e", expr(is_alpha_chr(middle_name)),
  # "S06f", expr(is_alpha_chr(name_suffix)), # Not currently in dummy data
  "S07a", expr(is_alpha_chr(previous_last_name)),
  "S07b", expr(is_alpha_chr(previous_first_name)),
  # "S07c", expr(is_alpha_chr(previous_middle_name)), # Not currently in dummy data
  # "S07d", expr(is_alpha_chr(previous_name_suffix)), # Not currently in dummy data
  "S08a", expr(!is.na(local_address_zip_code)),
  "S08b", expr(is_valid_zip_code(local_address_zip_code) &
                 is_valid_zip_code(mailing_address_zip_code)),
  "S09a", expr(!is.na(us_citizenship_code)),
  "S09b", expr(equivalent(is_international, us_citizenship_code == 2)),
  "S10a", expr(!is.na(first_admit_county_code)), # TODO verify I don't need to check code validity
  "S11a", expr(is_utah_county(first_admit_county_code) | # TODO: verify assumptions of county encoding
                 !(first_admit_state_code == "UT")),
  "S12", expr(!is.na(birth_date)),
  "S12a", expr(date_before_present_year(birth_date)),
  "S12b", expr(is.na(birth_date) | is.Date(birth_date)), # TODO: check since rule references date string
  "S12c", expr(age_in_range(birth_date, 0, 100)),
  "S13", expr(is_valid_gender(gender_code)),
  # "S13a", TODO: how to track change over time? Do we compare different dataframes?
  # "S13b", TODO: how to track change over time? Do we compare different dataframes?
  "S14a", expr(is_asian %in% c(TRUE, FALSE)),
  "S14b", expr(is_black %in% c(TRUE, FALSE)),
  "S14h", expr(is_hispanic_latino_ethnicity %in% c(TRUE, FALSE)),
  "S14i", expr(is_american_indian_alaskan %in% c(TRUE, FALSE)),
  # "S14m", TODO: is the check on multiple ethnicities relevant here? Edify column says "n/a"
  "S14n", expr(is_international %in% c(TRUE, FALSE)),
  "S14p", expr(is_hawaiian_pacific_islander %in% c(TRUE, FALSE)),
  "S14u", expr(is_other_race %in% c(TRUE, FALSE)), # TODO: resolve multiple "S14u" descriptions
  "S14w", expr(is_white %in% c(TRUE, FALSE)),
  # "S15a", expr(!is.na(residency_code)), # Not currently in dummy data
  "S16a", expr(!is.na(primary_major_cip_code)), # TODO: sufficient to verify non-missing? (sql checks values)
  "S17a", expr(is_valid_student_type(student_type_code)),
  "S17b", expr(!(is_grad_level(primary_level_class_id) &
                   is_undergrad_type(student_type_code))),
  # "S17c", TODO: How to compare to previous student data? Multiple dataframes? (similar to S13a)
  # "S17d", TODOO: same question as S17c
  "S17e", expr(!is_hs_type(student_type_code) | age_in_range(birth_date, 10, 20)),
  "S17f", expr(!is_freshmen_type(student_type_code) | age_in_range(birth_date, 18, Inf)),
  "S17g", expr(!is_freshmen_type(student_type_code) | age_in_range(birth_date, 0, 21)),
  # "S17h", TODO: How to determine "invalid student type enrolled in concurrent classes"?
  # "S17i", TODO: Same rule as S17h?
  # "S17j", TODO: Tracking changes--same question as S13a
  "S17k", expr(!is_freshmen_type(student_type_code) | age_in_range(birth_date, 16, Inf)),
  "S18a", expr(is_valid_class_level(primary_level_class_id)),
  "S18b", expr(!(is_grad_level(primary_level_class_id) & is_undergrad_type(student_type_code)) &
                 !(is_undergrad_level(primary_level_class_id) & is_grad_type(student_type_code))),
  "S19a", expr(is.na(primary_degree_id)),
  "S20a", expr(is_valid_credits(institutional_cumulative_credits_earned)),
  "S24a", expr(is_valid_credits(transfer_cumulative_credits_earned)), # TODO: sort out excel sheet ambiguity
  "S21",  expr(!is.na(institutional_cumulative_gpa)), # TODO: do I need to do more than check missing? (sql does...)
  "S21a", expr(is_valid_gpa(institutional_cumulative_gpa)),
  # "S21b", TODO: ambiguous specification. Need to parse through sql
  # "S22b", TODO: duplicate of S21b? Same line in excel document
  # "S23a", TODO: how to check whether undergrad students have grad hours or grad gpa?
  # "S23d", TODO: duplicate of S21b? Same line in excel document
  "S25a", expr(toupper(full_time_part_time_code) %in% c("P", "F")),
  "S26a", expr(age_in_range(birth_date, 0, 125)), # See notes on rule relevance in excel sheet
  # "S26b", Not relevant to compare age to birthdate since we only have birthdate
  "S27a", expr(first_admit_country_code %in% "US" | !is_us_state(first_admit_state_code)),
  "S27b", expr(!(first_admit_country_code %in% "US" &
                   (!is_us_county(first_admit_county_code) | !is_us_state(first_admit_state_code)))),
  "S27c", expr(is_valid_country_code(first_admit_country_code)),
  # "S28a", TODO: No high school code present
  # "S29a", TODO: No waiver code present
  # "S29b", TODO: No waiver code present
  "S30a", expr(!is.na(secondary_major_cip_code)), # TODO: verify this only applies to secondary major
  # "S31a", membership hours--noted as n/a in excel sheet
  # "S32a", expr(is_valid_clep(transfer_cumulative_clep_earned)), # TODO: missing this info. I'll need to understand what makes clep valid
  # "S33a", expr(is_valid_clep(transfer_cumulative_ap_earned)), # TODO: missing this info. From sql, looks like same logic as clep
  # "S34a", expr(is_valid_ssid(ssid)), # TODO: missing this info. I'll need to understand what makes ssid valid
  # "S34b", expr(is_valid_ssid(ssid) | !(is_hs_type(student_type_code) & first_admit_state_code == "UT")), # TODO: No ssid present
  # "S34c", expr(!(is.na(ssid) & first_admit_state_code == "UT" & is_hs_type(student_type_code))), # TODO: no ssid present
  # "S34d", expr(!is.na(ssid) | !is_concurrent(budget_code)) # TODO: no ssid, no budget code present
  # "S34e", expr(!is.na(ssid) | (!is_hs_type(student_type_code) & !is_freshmen_type(student_type_code))), # TODO: no ssid present
  "S35a", expr(is_valid_student_id(student_id)),
  "S35b", expr(is_valid_student_id(student_id)), # TODO: redundant with S35a? Seems to be relevant for banner IDs only
  "S36a", expr(is_valid_act_score(act_composite_score)), # TODO: verify I'm correctly matching rule to act score category (composite, english, etc.)
  "S38a", expr(is_valid_act_score(act_english_score)),
  "S39a", expr(is_valid_act_score(act_math_score)),
  "S40a", expr(is_valid_act_score(act_reading_score)),
  "S41a", expr(is_valid_act_score(act_science_score)),
  "S42a", expr(!is.na(high_school_graduation_date) | !is_freshmen_type(student_type_code)),
  # "S43c", expr((institutional_gpa == institutional_cumulative_gpa) |  # TODO: no institutional_gpa present, just cumulative gpa
  #              !(student_type_code %in% matching_gpa_student_types)), # TODO: verify which student types must have matching gpa
  "S44c", expr(!(is_pell_awarded %in% TRUE &
                   (!(is_pell_eligible %in% TRUE) | is_hs_type(student_type_code)))), # pell_eligible might already account for student type
  "S44d", expr((is_pell_eligible %in% c(TRUE, FALSE)) & (is_pell_awarded %in% c(TRUE, FALSE))), # Somewhat different from rule description but pell variables are now logical, not character
  "S45c", expr(is_bia %in% c(TRUE, FALSE)),
  "S46a", expr(!is.na(primary_major_college_id) & primary_major_college_id != ""),
  "S46b", expr(is_alpha_chr(primary_major_college_id)), # Same checks as name validity
  "S47a", expr(!is.na(primary_major_cip_code) & primary_major_cip_code != ""),
  "S47b", expr(primary_major_cip_code != secondary_major_cip_code),
  # "S47c", expr(is_alpha_chr(primary_major_desc)), # TODO: No primary_major_desc in data
  "S48a", expr(is_alpha_chr(secondary_major_college_id)), # TODO: verify my separation of excel line into multiple rules (same line as S46b)
  # "S49a", expr(!is_missing_chr(secondary_major_cip_code) & !is_missing_chr(secondary_major_desc)), # TODO: no secondary_major_desc in data
  # "S49b", expr(is_alpha_chr(secondary_major_desc)), # TODO: No secondary_major_desc in data
  "C00",  expr(!is_duplicated(cbind(subject_code, course_number, section_number))),
  "C04a", expr(nchar(course_number) == 4),
  "C04c", expr(!stringr::str_detect(course_number, "^[89]")),
  "C04d", expr(!stringr::str_detect(substring(course_number, 1, 4), "[a-zA-Z]")),
  "C06a", expr(is_valid_credits_chr(course_min_credits)),
  "C07a", expr(is_valid_credits_chr(course_max_credits)),
  "C07b", expr(max_credits >= min_credits),
  "C09", expr(tolower(c_line_item) %in% c("a","b","c","d","e","f","g","h","i","p","q","r","s","t","x")),
  "C10", expr(!is_missing_chr(campus_id)),
  "C11", expr(!is_missing_chr(budget_code)),
  # "C11b", expr(!(budget_code %in% c("BC", "SF")) | !(in_concurrent_master_list(course, subject))), # TODO: concurrent enrollment list
  "C12", expr(is_valid_instruction_method(instruction_method_code)), # TODO!
  "C13", expr(is_valid_program_type(program_type)), # TODO!
  # "C13a", USHE check on perkins program types
  # "C13c", USHE check on perkins budget codes
  "C14a", expr(c_credit_ind %in% c("C", "N")), # USHE check
  "C14b", expr(!(course_level_id == "N" & section_format_type_code != "LAB")),
  # "C14c", TODO: complicated logic, waiting for dummy data
  "C15a", expr(!is_missing_chr(meet_start_time)),
  "C16a", expr(!is_missing_chr(meet_end_time)),
  # "C17a", USHE rule for missing course meeting days
  "C18", expr(is.na(meeting_building_id) | !equivalent(meeting_building_id, building_number)),
  "C18a", expr(!is_missing_chr(meeting_building_id)),
  "C19a", expr(!is_missing_chr(building_number)),
  "C19c", expr(building_number %in% building_inventory), # TODO: how to get building inventory?
  "C19d", expr(building_number %in% rooms_inventory), # TODO: how to get rooms inventory?
  "C20a", expr(!is_missing_chr(meet_room_number)), # TODO: conditionality required?
  "C21a", expr(is_valid_occupancy(room_max_occupancy)),
  "C22a", expr(room_use_code %in% room_use_codes), # TODO: need valid room use codes
  "C22b", expr(!is_missing_chr(room_use_code)),
  "C39a", expr(is.Date(meet_start_date) & !is.na(meet_start_date)), # Summer TODO: how to distinguish?
  "C39b", expr(is.Date(meet_start_date) & !is.na(meet_start_date)), # Fall
  "C39c", expr(is.Date(meet_start_date) & !is.na(meet_start_date)), # Spring
  "C40a", expr(is.Date(meet_end_date) & !is.na(meet_end_date)), # Summer
  "C40b", expr(is.Date(meet_end_date) & !is.na(meet_end_date)), # Fall
  "C40c", expr(is.Date(meet_end_date) & !is.na(meet_end_date)), # Spring
  "C41a", expr(!is_missing_chr(title)),
  # "C41b", USHE rule for course title validity
  # "C41d", USHE rule for course title validity
  "C42a", expr(!is_missing_chr(istructor_employee_id)),
  "C42b", expr(is_missing_chr(instructor_employee_id) |
                 (nchar(instructor_employee_id) == 9L &
                    grepl("^[[a-zA-Z]]", instructor_employee_id))),
  # "C42c" USHE rule for instructor ID
  "C43a", expr(!is.missing_chr(first_name)),
  # "C43c", expr(is_alpha_chr(c_instruct_name)), USHE rule
  "C44", expr(!is_missing_chr(section_format_type_code)),
  "C44a", expr(is_valid_section_format(section_format_type_code, valid_sections)),
  "C46", expr(!is_missing_chr(academic_department_id)),
  "C46a", expr(is_alpha_chr(academic_department_id, missing_ok = TRUE)),
  # "C47b", expr(c_gen_ed %in% valid_gened_codes), # USHE rule
  # "C48a", expr(is_missing_chr(c_dest_site) | c_dest_site %in% reference_highschools), #USHE rule
  "C49a", expr(!is.na(class_size) & class_size != 0),
  "C49b", expr(is.na(class_size) | class_size >= 0 & class_size <= 9999),
  # "C49c", USHE rule comparing enrolled students to class size (involving COUNT)
  "C51a", expr(c_level %in% c("R", "U", "G")), # USHE check
  # "C51b" USHE rule for invalid remedial level (somewhat complex)
  "C52a", expr(!is_missing_chr(course_reference_number)),
  "C52b", expr(is_valid_course_reference_number(course_reference_number)),
  "C52c", expr(!is_duplicated(course_reference_number)),
)


# Helper to get the ushe file type from the ushe element
get_ushe_file <- function(ushe_element) {
  out <- case_when(
    grepl("^SC", ushe_element) ~ "Student Course",
    grepl("^S[0-9]", ushe_element) ~ "Student",
    grepl("^C", ushe_element) ~ "Course",
    grepl("^G", ushe_element) ~ "Graduation",
    grepl("^B", ushe_element) ~ "Buildings",
    grepl("^R", ushe_element) ~ "Rooms",
    TRUE ~ NA_character_
    )
  out
}

# dataframe with rule info from Data Inventory
all_rules <- read.csv("sandbox/full-rules-rename.csv") %>%
  mutate(ushe_rule = map(ushe_rule, ~unlist(str_split(., pattern = ", "))),
         ref_rule = map_chr(ushe_rule, ~`[`(., 1))) %>%
  unnest(cols = ushe_rule) %>%
  mutate(activity_date = ifelse(activity_date == "n/a", NA_character_, activity_date)) %>%
  select(rule = ushe_rule, ref_rule, description, status,
         type, activity_date) %>%
  glimpse()

# Rule info joined to anonymous-function tibble
checklist <- all_rules %>%
  inner_join(rule_spec, by = c(ref_rule = "rule")) %>%
  mutate(file = get_ushe_file(rule)) %>%
  glimpse()


usethis::use_data(checklist, overwrite = TRUE)
