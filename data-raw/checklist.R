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
  "C07b", expr(course_max_credits >= course_min_credits),
  "C08a", expr(is_valid_credit_format(contact_hours)),
  "C09", expr(tolower(c_line_item) %in% c("a","b","c","d","e","f","g","h","i","p","q","r","s","t","x")),
  "C10", expr(!is_missing_chr(campus_id)),
  "C11", expr(!is_missing_chr(budget_code)),
  # "C11b", expr(!(budget_code %in% c("BC", "SF")) | !(in_concurrent_master_list(course, subject))), # TODO: concurrent enrollment list
  "C12", expr(is_valid_values(instruction_method_code,
                              valid_instruction_method_codes,
                              missing_ok = TRUE)), # TODO: is missing OK?
  "C13", expr(is_valid_values(program_type, valid_program_types, missing_ok = TRUE)),
  # "C13a", USHE check on perkins program types
  # "C13c", USHE check on perkins budget codes
  "C14a", expr(c_credit_ind %in% c("C", "N")), # USHE check
  "C14b", expr(!(course_level_id == "N" & section_format_type_code != "LAB")),
  # "C14c", TODO: complicated logic, waiting for dummy data
  "C15a", expr(!is_missing_chr(meet_start_time_1)),
  "C23a", expr(!is_missing_chr(meet_start_time_2)),
  "C31a", expr(!is_missing_chr(meet_start_time_3)),
  "C16a", expr(!is_missing_chr(meet_end_time_1)),
  "C24a", expr(!is_missing_chr(meet_end_time_2)),
  "C32a", expr(!is_missing_chr(meet_end_time_3)),
  # "C17a", USHE rule for missing course meeting days
  "C18", expr(is.na(meet_building_id_1) | !equivalent(meet_building_id_1, building_number_1)),
  "C26", expr(is.na(meet_building_id_2) | !equivalent(meet_building_id_2, building_number_2)),
  "C34", expr(is.na(meet_building_id_3) | !equivalent(meet_building_id_3, building_number_3)),
  "C18a", expr(!is_missing_chr(meet_building_id_1)),
  "C26a", expr(!is_missing_chr(meet_building_id_2)),
  "C34a", expr(!is_missing_chr(meet_building_id_3)),
  "C19a", expr(!is_missing_chr(building_number_1)),
  "C27a", expr(!is_missing_chr(building_number_2)),
  "C35a", expr(!is_missing_chr(building_number_3)),
  "C19c", expr(building_number_1 %in% building_inventory),
  "C27c", expr(building_number_2 %in% building_inventory),
  "C35c", expr(building_number_3 %in% building_inventory),
  "C19d", expr(building_number_1 %in% rooms_inventory),
  "C27d", expr(building_number_2 %in% rooms_inventory),
  "C35d", expr(building_number_3 %in% rooms_inventory),
  "C20a", expr(!is_missing_chr(meet_room_number_1)), # TODO: conditionality required?
  "C28a", expr(!is_missing_chr(meet_room_number_2)), # TODO: conditionality required?
  "C36a", expr(!is_missing_chr(meet_room_number_3)), # TODO: conditionality required?
  "C21a", expr(is_valid_occupancy(room_max_occupancy_1)),
  "C29a", expr(is_valid_occupancy(room_max_occupancy_2)),
  "C37a", expr(is_valid_occupancy(room_max_occupancy_3)),
  "C22a", expr(room_use_code_1 %in% valid_room_use_codes), # TODO: need valid room use codes
  "C30a", expr(room_use_code_2 %in% valid_room_use_codes), # TODO: need valid room use codes
  "C38a", expr(room_use_code_3 %in% valid_room_use_codes), # TODO: need valid room use codes
  "C22b", expr(!is_missing_chr(room_use_code_1)),
  "C30b", expr(!is_missing_chr(room_use_code_2)),
  "C38b", expr(!is_missing_chr(room_use_code_3)),
  "C39a", expr(is.Date(meet_start_date) & !is.na(meet_start_date)), # Summer TODO: how to distinguish?
  "C39b", expr(is.Date(meet_start_date) & !is.na(meet_start_date)), # Fall
  "C39c", expr(is.Date(meet_start_date) & !is.na(meet_start_date)), # Spring
  "C40a", expr(is.Date(meet_end_date) & !is.na(meet_end_date)), # Summer
  "C40b", expr(is.Date(meet_end_date) & !is.na(meet_end_date)), # Fall
  "C40c", expr(is.Date(meet_end_date) & !is.na(meet_end_date)), # Spring
  "C41a", expr(!is_missing_chr(course_title)),
  # "C41b", USHE rule for course title validity
  # "C41d", USHE rule for course title validity
  "C42a", expr(!is_missing_chr(instructor_employee_id)),
  "C42b", expr(is_missing_chr(instructor_employee_id) |
                 (nchar(instructor_employee_id) == 9L &
                    grepl("^[[a-zA-Z]]", instructor_employee_id))),
  # "C42c" USHE rule for instructor ID
  "C43a", expr(!is.missing_chr(first_name)),
  # "C43c", expr(is_alpha_chr(c_instruct_name)), USHE rule
  "C44", expr(!is_missing_chr(section_format_type_code)),
  "C44a", expr(is_valid_values(section_format_type_code,
                               valid_section_format_type_codes,
                               missing_ok = TRUE)), # Is missing OK here? Other conditionality?
  "C45", expr(!is_missing_chr(college_id)),
  "C45a", expr(is_alpha_chr(college_id)),
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
  "G02b", expr(student_id %in% student_file$student_id), # TODO: how to get student file?
  "G21e", expr(ssn %in% student_file$ssn), # TODO: how to get student file?
  # "G03e", expr(nchar(first_name) <= 15), # USHE rule, might need to rename
  # "G03g", expr(nchar(middle_name) <= 15), # USHE rule
  # "G03i", expr(nchar(name_suffix) <= 4), # USHE rule
  "G08b", expr(is_valid_graduation_date(gradutation_date)),
  "G09a", expr(is_valid_values(cip_code, valid_cip_codes, missing_ok = TRUE)),
  "G10a", expr(degree_status_code != "AW" | !is_missing_chr(degree_type)), # TODO: check assumed names and values
  # "G12b", USHE rule for Transfer hours over 300 credits
  # "G13b", USHE rule for Graduation hours 1.5 times required hours
  # "G14b", USHE rule for Other hours 1.5 times required hours
  # "G15b", USHE rule for Remedial hours over 60
  "G18a", expr(is.numeric(required_credits) &
                 !is.na(required_credits) &
                 required_credits >= 0 ),
  "G19a", expr(!is_utah_county(county_code) | !is_missing_chr(high_school_code)),
  "G21d", expr(!is_duplicated(cbind(sis_student_id, # TODO: check "sis_student_id" vs "student_id"
                                    graduation_date, primary_major_cip_code, degree_id,
                                    ipeds_award_level_code, primary_major_id))),
  "G24a", expr(graduated_academic_year_code %in% reference_year), # TODO: how to get reference year?
  "G25a", expr(is_valid_values(season, c("1", "2", "3"))), # TODO: check values
  "G28a", expr(!is_missing_chr(degree_desc)),
  "SC04a", expr(!is_missing_chr(subject_code)),
  "SC05a", expr(!is_missing_chr(course_number)),
  "SC06a", expr(!is_missing_chr(section_number)),
  # "SC08b", TODO: "invalid earned credit hours based on grade" but I see no grade type
  # "SC08c", TODO: "earned credit hours is missing, but have a grade" but no grade present
  # "SC08d", TODO: complex logic for "At end of term, earned credit hours should match attempted credit hours (when a passing grade)"
  "SC10a", expr(is_valid_values(final_grade, valid_grades, missing_ok = TRUE) |
                  is.na(attempted_credits) | attempted_credits == 0),
  # "SC10b", USHE rule "missing concurrent enrollment grades"
  # "SC10c", USHE rule "invalid concurrent enrollment grade"
  # "SC12a", USHE rule "Invaild student type codes (concurrent enrollment)"
  # "SC12b", USHE rule "Student type and HS code alignment (concurrent students only in Utah High schools)"
  # "SC12c", USHE rule "Budget code and student type alignment (concurrent classes has concurrent students)"
  # "SC12d", USHE rule "Budget code, student type and entry action alignment"
  # "SC12e", USHE rule "Budget code and student type alignment (concurrent students in concurrent classes)"
  # "SC12f", USHE rule "Budget code and student type alignement (concurrent students in concurrent classes in out-of state high schools)"
  "SC14a", expr(is_valid_course_reference_number(course_reference_number)),
  "SC14b", expr(!is_missing_chr(course_reference_number)),
  # "SC15b", TODO: sort out what needs to be done with edify data
  # "SC15c", TODO: sort out what needs to be done with edify data
  "B02a", expr(!is_missing_chr(building_location_code) & !is_missing_chr(building_location_desc)),
  "B02b", expr(is_valid_values(building_location_code, valid_building_location_codes)),
  "B03a", expr(!is_missing_chr(building_ownership_code)),
  "B03b", expr(is_valid_values(building_ownership_code, valid_ownership_codes)),
  "B04a", expr(!is_missing_chr(building_construction_year)), # TODO: Should this be a different year (b_year in ushe)?
  "B05a", expr(!is_missing_chr(building_name)),
  "B06a", expr(!is_missing_chr(building_number)),
  "B06b", expr(!is_duplicated(building_number)),
  "B07b", expr(!is_missing_chr(building_abbrv)),
  "B08a", expr(is.na(building_cost_replacement) |
                 building_cost_replacement <= 3.5e6 |
                 !is_missing_chr(building_construction_year)),
  "B08b", expr((!is.na(building_cost_replacement) & building_cost_replacement > 3.5e6) |
                 !is_missing_chr(building_construction_year)),
  "B10a", expr(!is_missing_chr(building_cost_replacement)),
  "B11a", expr(is.na(building_cost_replacement) |
                 building_cost_replacement <= 3.5e6 |
                 !is_missing_chr(building_condition_code)),
  "B11b", expr(is_valid_values(building_condition_code, valid_building_condition_codes)),
  "B11c", expr((!is.na(building_cost_replacement) & building_cost_replacement > 3.5e6) |
                 !is_missing_chr(building_condition_code)),
  "B12a", expr(!is_missing_chr(building_area_gross)),
  "B12b", expr(!is.na(as.numeric(building_area_gross)) &
                 as.numeric(building_area_gross) > 0), # TODO: condition on ownership and aux?
  # "B12c", TODO: this one needs a summary of rooms data - "Gross area less than sum of rooms in building"
  "B14a", expr(is.na(building_cost_replacement) |
                 building_cost_replacement <= 3.5e6 | !
                 is_missing_chr(building_risk_number)), # Risk number not currently in buildings table
  "B14b", expr((!is.na(building_cost_replacement) & building_cost_replacement > 3.5e6) |
                 !is_missing_chr(building_risk_number)),
  "B15a", expr(!is_missing_chr(building_auxiliary)), # Do I need to condition on ownership?
  "B15a", expr(is_valid_values(building_auxiliary, c("A", "N"), missing_ok = TRUE)),
  "B99a", expr(!is_duplicated(cbind(b_inst,b_year,b_number))), # USHE rule
  # "B99b", TODO: USHE rule for "buildings must have rooms", requires join to rooms data
  # "R03b", TODO USHE rule for "Room building not in building file", requires join to buildings data
  "R04a", expr(!is_missing_chr(room_number)),
  "R06a", expr(!is_missing_chr(room_group1_code)),
  "R06b", expr(is_valid_values(room_group1_code, valid_room_group1_codes, missing_ok = TRUE)),
  "R07a", expr((room_group1_code %in% "Z") | !is_missing_chr(room_use_code_group)),
  "R07b", expr(is_valid_values(room_use_code_group, valid_room_use_code_groups, missing_ok = TRUE)),
  "R07c", expr(!(room_group1_code %in% "Z") | !is_missing_chr(room_use_code_group)),
  "R08a", expr(!is_missing_chr(room_use_code)),
  "R08b", expr(is_valid_values(room_use_code, valid_room_use_codes, missing_ok = TRUE)),
  "R08d", expr(!(room_use_code %in% c("250", "255"))),
  "R09a", expr(!is_missing_chr(room_name)),
  "R10a", expr(!(room_use_code %in% c("110", "210", "230")) | !is_missing_chr(room_stations)),
  "R10b", expr(!(room_use_code %in% c("110", "210", "230") &
                   room_stations == "0" &
                   room_group1_code != "Z" &
                   room_use_code_group != "000")),
  "R10c", expr(!(room_use_code %in% "110") |
                 room_group1_code %in% "Z" |
                 room_use_code_group %in% "000" |
                 in_range(room_area / room_stations, 7, 16)),
  "R10d", expr(!(room_use_code %in% "210") |
                 room_group1_code %in% "Z" |
                 room_use_code_group %in% "000" |
                 in_range(room_area / room_stations, 8, 19)),
  "R11a", expr(!is_missing_chr(room_area)),
  "R11b", expr(!(room_area %in% "0")),
  "R13a", expr(room_prorated %in% c("Y", "N")),
  "R13b", expr(!is_missing_chr(room_prorated)),
  "R13c", expr(!(room_prorated %in% "Y" &
                   room_prorated_area %in% "0" &
                   room_area %in% "0")),
  "R13d", expr(!(room_area %in% "0") | room_prorated_area %in% "0"),
  "R13e", expr(!(room_prorated_area %in% "0") | room_area %in% "0"),
  "R13f", expr(room_prorated_area %in% "0" | !(room_prorated %in% "N")),
  "R14a", expr(!is_missing_chr(room_prorated_area)),
  # "R14b" TODO: Needs a join of proration info to room info. How to get sum of prorated area?
  "R15a", expr(is.Date(room_activity_date) & !is.na(room_activity_date)),
  "R15b", expr(!is.na(room_activity_date)),
  "R15b", expr(age_in_range(room_activity_date, 0, Inf)),
  "R99a", expr(!is_duplicated(cbind(r_inst, r_year, r_build_number, r_number,
                                    r_suffix, r_group1, r_use_code)))
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
