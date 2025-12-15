## code to prepare `checklist` dataset goes here

# Rule specification

library(tibble)
library(dplyr)
library(rlang)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)

# This is where each rule is implemented as an R expression
rule_spec <- tribble(
  ~rule,  ~checker,
  "S00a", expr(!is_duplicated(cbind(student_id, term_id))),
  "S00b", expr(!is_duplicated(cbind(ssn, term_id))),
  "S02a", expr(!is.na(s_year) & !is.na(s_term) & !is.na(s_extract)), # USHE check
  "SC02a", expr(!is.na(sc_year) & !is.na(sc_term) & !is.na(sc_extract)), # USHE check
  "C02", expr(!is.na(c_year) & !is.na(c_term) & !is.na(c_extract)), # USHE check
  "S03a", expr(!is.na(s_id) & !is_missing_chr(s_id)), #USHE check
  "S03b", expr(!(s_id_flag %in% "S") | !is.na(s_ssn)), # USHE check
  "S03c", expr(!((s_citz_code %in% "1") & s_ssn(ssn))), #USHE check
  "S04a", expr(s_id_flag %in% c("S", "I")), # USHE check
  "S04b", expr(is_valid_ssn(ssn, missing_ok = TRUE)),
  "S04c", expr(!(s_id_flag %in% "S") | (s_id == s_banner_id)), # USHE check
  "S04d", expr(!(s_id_flag %in% "I") | (s_id != s_banner_id)), # USHE check
  "S05a", expr(is_valid_previous_id(previous_student_id)),
  "S06a", expr(!is_missing_chr(last_name)),
  "S06b", expr(is_alpha_chr(last_name)),
  "S06c", expr(!is_missing_chr(first_name)),
  "S06d", expr(is_alpha_chr(first_name)),
  "S06e", expr(is_alpha_chr(middle_name)),
  "S06f", expr(is_alpha_chr(name_suffix)),
  "S07a", expr(is_alpha_chr(previous_last_name)),
  "S07b", expr(is_alpha_chr(previous_first_name)),
  "S07c", expr(is_alpha_chr(previous_middle_name)),
  "S07d", expr(is_alpha_chr(previous_name_suffix)),
  "S08a", expr(!is_missing_chr(local_address_zip_code) &
                 !is_missing_chr(mailing_address_zip_code)),
  "S08b", expr(is_valid_zip_code(s_curr_zip, missing_ok = FALSE)), # USHE Check
  "S09a", expr(!is_missing_chr(us_citizenship_code)),
  "S09b", expr(!((us_citizenship_code == 2 & is.na(ipeds_race_ethnicity == "Non-Resident Alien")) |
                 (is.na(us_citizenship_code) & ipeds_race_ethnicity == "Non-Resident Alien") |
                 ((us_citizenship_code != 2) & ipeds_race_ethnicity == "Non-Resident Alien") |
                 (us_citizenship_code == 2 & (ipeds_race_ethnicity != "Non-Resident Alien")))),
  "S10a", expr(!is_missing_chr(first_admit_county_code)), # TODO verify I don't need to check code validity
  "S11a", expr(is_utah_county(first_admit_county_code) |
                 !(first_admit_state_code %in% "UT")),
  "S12", expr(!is.na(birth_date)),
  "S12a", expr(date_before_present_year(birth_date)),
  "S12b", expr(is.na(s_birth_dt) | is.Date(s_birth_dt)), # USHE rule
  "S12c", expr(age_in_range(birth_date, 0, 100)),
  "S13", expr(toupper(gender_code) %in% c("M", "F")),
  "S13a", expr(TODO('USHE rule for "gender change after census"')),
  "S13b", expr(TODO('USHE rule for "gender change after previous term"')),
  "S14a", expr(s_ethnic_a %in% c("A", NA)),
  "S14b", expr(s_ethnic_b %in% c("B", NA)),
  "S14h", expr(s_ethnic_h %in% c("H", NA)),
  "S14i", expr(s_ethnic_i %in% c("I", NA)),
  "S14m", expr(!(s_ethnic_ipeds %in% c("H","A","B","I","P","W","N","U"))), # TODO: verify my interpretation
  "S14n", expr(s_ethnic_n %in% c("N", NA)),
  "S14p", expr(s_ethnic_p %in% c("P", NA)),
  "S14u", expr(s_ethnic_u %in% c("U", NA)),
  "S14ua", expr(!(s_ethnic_u %in% "U") |
                  (is.na(s_ethnic_a) & is.na(s_ethnic_b) & is.na(s_ethnic_h) &
                   is.na(s_ethnic_i) & is.na(s_ethnic_p) & is.na(s_ethnic_w) &
                   is.na(s_ethnic_n))),
  "S14w", expr(s_ethnic_w %in% c("W", NA)),
  "G07a", expr(g_ethnic_a %in% c("A", NA)),
  "G07b", expr(g_ethnic_b %in% c("B", NA)),
  "G07c", expr(g_ethnic_h %in% c("H", NA)),
  "G07d", expr(g_ethnic_i %in% c("I", NA)),
  "G07g", expr(g_ethnic_n %in% c("N", NA)),
  "G07e", expr(g_ethnic_p %in% c("P", NA)),
  "G07h", expr(g_ethnic_u %in% c("U", NA)),
  "G07i", expr(!(g_ethnic_u %in% "U") |
                  (is.na(g_ethnic_a) & is.na(g_ethnic_b) & is.na(g_ethnic_h) &
                     is.na(g_ethnic_i) & is.na(g_ethnic_p) & is.na(g_ethnic_w) &
                     is.na(g_ethnic_n))),
  "G07f", expr(g_ethnic_w %in% c("W", NA)),

  "S15a", expr(is_valid_values(s_regent_res, c("R", "N", "A", "M", "G"))), #USHE check
  "UTS01", expr(is_valid_values(residency_code, c("R", "N", "A", "M", "G", "C", "H"))),
  "S16a", expr(is_valid_values(primary_major_cip_code, valid_cip_codes)),
  "S17a", expr(is_valid_values(s_reg_status, valid_s_reg_statuses, missing_ok = FALSE)), # USHE check
  "S17b", expr(!((s_reg_status %in% c("CS","HS","FF","FH","TU")) &
                   (s_level %in% c("GN","GG")))),
  "S17c", expr(TODO("How to compare to previous student data? Multiple dataframes? (similar to S13a)")),
  "S17d", expr(TODO("same question as S17c. These are database checks)")),
  "S17e", expr(!(s_reg_status %in% "HS") | (s_age >= 10 & s_age <= 20)),
  "S17f", expr(!(s_reg_status %in% "FF") | s_age >= 18),
  "S17g", expr(!(s_reg_status %in% "FH") | s_age <= 21), #ignoring conditioning on highschool hs_alternative
  "S17h", expr(s_reg_status %in% "HS" | !(c_budget_code %in% c("BC", "SF"))), #USHE rule
  "S17j", expr(TODO('USHE rule for "REGISTRATION STATUS CHANGED BASED ON 3RD WEEK DATA".
                    How to compare to previous data?')),
  "S17k", expr(!(s_reg_status %in% "FH") | s_age >= 16),
  "S18a", expr(is_valid_values(primary_level_class_id, valid_level_class_ids,
                               missing_ok = FALSE)),
  "S18b", expr(!(s_reg_status %in% c("HS", "FH", "FF", "TU", "CS", "RS") & s_level %in% c("GN","GG")) |
                 (s_reg_status %in% c("NG","TG","CG","RG") & s_level %in% c("FR", "SO", "JR", "SR", "UG"))),
  "S19a", expr(is_valid_values(ipeds_award_level_code, valid_ipeds_degree_award_levels, missing_ok = TRUE)),
  "S20a", expr(is_valid_credits(institutional_cumulative_credits_earned, missing_ok = TRUE)),
  "S24a", expr(is_valid_credits(transfer_cumulative_credits_earned, missing_ok = TRUE)),
  "S21",  expr(!is.na(institutional_cumulative_gpa)), # USHE rule
  "S21a", expr(is_valid_gpa(institutional_cumulative_gpa, missing_ok = TRUE)),
  "S21b", expr(s_level %in% c("GN", "GG") |
                 !(s_cum_gpa_ugrad %in% c(0, "", NA)) |
                 sc_grade %in% c("CR", "NG", "P", "SP") |
                 sc_earned_cr <= 0 |
                 s_cum_hrs_ugrad <= 0 |
                 sc_att_cr <= 0 |
                 !(s_extract %in% "E")), # USHE rule
  "S22b", expr(!(s_level %in% c("GN", "GG")) |
                 !(s_cum_hrs_grad %in% c(0, "", NA)) |
                 sc_grade %in% c("CR", "NG", "P", "SP") |
                 sc_earned_cr <= 0 |
                 s_cum_hrs_grad <= 0 |
                 sc_att_cr <= 0 |
                 !(s_extract %in% "E")), # USHE rule--unsure of logic since this is for grad and sql was for ugrad
  "S23d", expr(!(s_level %in% c("GN", "GG")) |
                 !(s_cum_gpa_grad %in% c(0, "", NA)) |
                 sc_grade %in% c("CR", "NG", "P", "SP") |
                 sc_earned_cr <= 0 |
                 s_cum_hrs_grad <= 0 |
                 sc_att_cr <= 0 |
                 !(s_extract %in% "E")), # USHE rule--unsure of logic since this is for grad and sql was for ugrad

  "S23a", expr(s_level %in% c("GN", "GG") | as.numeric(s_cum_gpa_grad) %in% c(0, NA)),
  "S25a", expr(toupper(full_time_part_time_code) %in% c("P", "F")),
  "S26a", expr(!is.na(s_age) & s_age > 0 & s_age <= 125),
  "S26b", expr(TODO("Not relevant to compare age to birthdate since we only have birthdate?")),
  "S27a", expr((first_admit_country_code %in% "US") | !is_us_state(first_admit_state_code)),
  "S27b", expr(!(first_admit_country_code %in% "US" &
                   (first_admit_county_code %in% "97" |
                      is_nonus_state(first_admit_state_code)))),
  "S27c", expr(is_valid_values(first_admit_country_code, valid_country_codes, missing_ok = FALSE)),
  "S28a", expr(!(s_state_origin %in% "UT") |
                 !is_undergrad_type(s_reg_status) |
                 is_valid_values(s_high_school, valid_highschools, missing_ok = FALSE)),
  "S29a", expr(s_hb75_waiver <= 100),
  "S29b", expr(!is.na(s_hb75_waiver) & s_hb75_waiver <= 100 & s_hb75_waiver >= 0),
  "S30a", expr(is_valid_values(secondary_major_cip_code, valid_cip_codes)),
  "S31a", expr(s_inst %in% c("5220","5221","3679","3676","63") | s_cum_membership %in% 0),
  "S32a", expr(is_valid_credits(total_cumulative_clep_credits_earned, missing_ok = TRUE)),
  "S33a", expr(is_valid_credits(total_cumulative_ap_credits_earned, missing_ok = TRUE)),
  "S34a", expr(is_valid_student_id(student_id)),
  "S34b", expr(is_valid_student_id(student_id) |
                 !(is_hs_type(student_type_code) & first_admit_state_code == "UT")),
  "S34c", expr(!(is.na(student_id) &
                   first_admit_state_code == "UT" &
                   is_hs_type(student_type_code))),
  "S34d", expr(!is.na(student_id) | !(budget_code %in% c("BC", "SF"))),
  "S34e", expr(!is.na(student_id) |
                 (!is_hs_type(student_type_code) &
                    !is_freshmen_type(student_type_code))),
  "S35a", expr(is_valid_student_id(student_id)),
  "S35b", expr(is_valid_student_id(student_id)), # TODO: redundant with S35a? Seems to be relevant for banner IDs only
  "S35c", expr(is_alpha_chr(substring(s_banner_id, 1, 1))),
  "G21c", expr(is_alpha_chr(substring(g_banner_id, 1, 1))),
  "SC13c", expr(is_alpha_chr(substring(sc_banner_id, 1, 1))),
  "S36a", expr(is_valid_act_score(act_composite_score)),
  "S37a", expr(is_valid_values(s_curr_cip, valid_cip_codes)), #USHE Rule
  "S38a", expr(is_valid_act_score(act_english_score)),
  "S39a", expr(is_valid_act_score(act_math_score)),
  "S40a", expr(is_valid_act_score(act_reading_score)),
  "S41a", expr(is_valid_act_score(act_science_score)),
  "S42a", expr(!is.na(high_school_graduation_date)),
  "S43c", expr((s_term_gpa == s_cum_gpa_ugrad) |
               (s_reg_status %in% c("FF", "FH", "TU", "TG")) |
               (s_level == "FR")), # USHE rule
  "S44c", expr(!is_hs_type(student_type_code) |
                 (!(is_pell_eligible %in% TRUE) & !(is_pell_awarded %in% TRUE))), #USHE check
  "UTS02", expr(!is_hs_type(student_type_code) | !(is_pell_awarded %in% TRUE)),
  "S44d", expr(s_pell %in% c("E", "R") | !(s_extract %in% "e")),
  "S45c", expr(s_bia %in% "B" | !(s_extract %in% "e")),
  "S46a", expr(!is_missing_chr(primary_major_college_id)),
  "S46b", expr(is_alpha_chr(primary_major_college_id)),
  "S47a", expr(!is_missing_chr(primary_major_cip_code)),
  "S47b", expr(is_missing_chr(primary_major_desc) |
                 is_missing_chr(secondary_major_desc) |
                 (primary_major_desc != secondary_major_desc)),
  "S47c", expr(matches_regex(primary_major_desc, "^[a-zA-Z' \\- & /]*$", #alpha plus space, apostrophe, hyphen, ampersand, forward slash
                             missing_ok = TRUE)),
  "S48a", expr(is_alpha_chr(secondary_major_college_id)),
  "S49a", expr(is_missing_chr(secondary_major_cip_code) | !is_missing_chr(secondary_major_desc)),
  "S49b", expr(matches_regex(secondary_major_desc, "^[a-zA-Z' \\- & /]*$", #alpha plus space, apostrophe, hyphen, ampersand, forward slash
                             missing_ok = TRUE)),
  "C00",  expr(!is_duplicated(cbind(term_id, subject_code, course_number, section_number))),
  "C04a", expr(nchar(course_number) != 3 | nchar(course_number) != 2 | nchar(course_number) != 1),
  "C04c", expr(!stringr::str_detect(course_number, "^[89]")),
  "C04d", expr(!stringr::str_detect(substring(course_number, 1, 4), "[a-zA-Z]")),
  "C06a", expr(is_valid_credits_chr(course_min_credits)),
  "C07a", expr(is_valid_credits_chr(course_max_credits)),
  "C07b", expr(as.numeric(course_max_credits) >= as.numeric(course_min_credits)),
  "C08a", expr(is_valid_credits(contact_hours, missing_ok = TRUE)),
  "C09", expr(is_valid_values(tolower(c_line_item), valid_c_line_items, missing_ok = TRUE)), # USHE rule
  "C10", expr(!is_missing_chr(campus_id)),
  "C11", expr((campus_id %in% 'XXX') | !is_missing_chr(budget_code)),
  "C11b", expr((paste0(subject_code, "-", course_number) %in% concurrent_course_ids) | !(budget_code %in% c('BC', 'SF'))),
  "C12", expr(is_valid_values(c_delivery_method,
                              valid_ushe_instruction_method_codes,
                              missing_ok = TRUE)), # USHE Check
  "UTC01", expr(is_valid_values(instruction_method_code,
                                valid_instruction_method_codes,
                                missing_ok = TRUE)),
  "UTC02", expr(!(section_status == "C" & class_size > 0)),
  "UTC03", expr(!(subject_code != "CED" & !is.na(enrollment) & enrollment != 0 &
        schedule_code %in% c("LEC", "LEX") &
        ((!is.na(lab_hours)   & lab_hours > 0) |
            (!is.na(other_hours) & other_hours > 0)))),
  "UTC04", expr(!(!is.na(subject_code)   & subject_code != "CED" &
        !is.na(enrollment)     & enrollment != 0 &
        !is.na(schedule_code)  & schedule_code %in% c("LAB", "LBC", "ACT") &
        ((!is.na(other_hours)   & other_hours > 0) | (!is.na(lecture_hours) & lecture_hours > 0)))),
  "UTC05", expr(!(subject_code != "CED" &
        !is.na(enrollment) & enrollment != 0 & schedule_code == "LAB" &
          !is.na(credit_hours) & credit_hours != 0)),
  "UTC06", expr(!(subject_code != "CED" & !is.na(enrollment) & enrollment != 0 &
        schedule_code == "LBC" & is.na(credit_hours))),
  "UTC07", expr(!(subject_code != "CED" & !is.na(enrollment) & enrollment != 0 & schedule_code == "LEL" &
        (is.na(credit_hours) | is.na(lab_hours)) & !is.na(other_hours))),
  "UTC08", expr(!(subject_code != "CED" & !is.na(enrollment) & enrollment != 0 &
        !(schedule_code %in% c("LEC", "LEX", "LEL")) & !is.na(lecture_hours) & lecture_hours > 0)),
  "UTC09", expr(!(subject_code != "CED" & !is.na(enrollment) & enrollment != 0 &
        !(schedule_code %in% c("LAB", "LBC", "LEL", "ACT")) & !is.na(lab_hours) & lab_hours > 0)),
  "UTC10", expr(!(is.na(program_type) & subject_code != "CED")),
  "UTC12", expr(!(subject_code != "CED" & ((active_ind == "A" & is.na(budget_code)) |
            (!(budget_code %in% valid_budget_codes))))),
  "UTC13", expr(!(active_ind == "A" & subject_code != "CED" & ((budget_code %in% c("BC", "SF")) !=
            stringr::str_detect(section_number, "V|S\\^|S|X|J")))),
  "UTC14", expr(!(!is.na(budget_code) & stringr::str_detect(budget_code, "^B") &
           !is.na(campus_code) & !is.na(instruction_method) &
        ((campus_code != "O01" & instruction_method == "I") | (campus_code %in% c("O01", "UOS") & instruction_method != "I")))),
  "UTC16", expr(!(!(instruction_method %in% c("I", "E")) & !(building_code_1 %in% c("VIRT", "ONLINE")) &
        !is.na(building_code_1) & is.na(room_code_1))),
  "C13", expr(is_valid_values(program_type, valid_program_types, missing_ok = TRUE)),
  "C13a", expr(TODO("USHE check on perkins program types. Requires a query?")),
  "C13c", expr(TODO("USHE check on perkins budget codes. Need query for perkins codes?")),
  "C14a", expr(c_credit_ind %in% c("C", "N")), # USHE check
  "C14b", expr(!(c_credit_ind %in% "N" &
                   c_extract %in% "3" &
                   !(c_instruct_type %in% "LAB"))), #USHE check now
  "C14c", expr(c_instruct_type %in% "LAB" |
                 c_program_type %in% c("P", "V") |
                 c_budget_code %in% c("BV", "SQ") |
                 paste(c_crs_sbj, c_crs_num) %in% TODO("Need Reference.dbo.ETPL for code lookup") |
                 !(c_extract %in% "E")),
  "C15a", expr(!is_missing_chr(meet_start_time_1) | is.na(meet_days_1)),
  "C23a", expr(!is_missing_chr(meet_start_time_2) | is.na(meet_days_2)),
  "C31a", expr(!is_missing_chr(meet_start_time_3) | is.na(meet_days_3)),
  "C16a", expr(!is_missing_chr(meet_end_time_1) | is.na(meet_days_1)),
  "C24a", expr(!is_missing_chr(meet_end_time_2) | is.na(meet_days_2)),
  "C32a", expr(!is_missing_chr(meet_end_time_3) | is.na(meet_days_3)),
  "C17a", expr(!is_missing_chr(c_days) |
                 c_delivery_method %in% c("C", "I", "V", "Y") |
                 c_budget_code %in% "SF" |
                 !(c_extract %in% "3")) , # USHE check, TODO: add site-type (query) condition?
  "C25a", expr(!is_missing_chr(c_days2) |
                 c_delivery_method %in% c("C", "I", "V", "Y") |
                 c_budget_code %in% "SF" |
                 !(c_extract %in% "3")) , # USHE check, TODO: add site-type (query) condition?
  "C33a", expr(!is_missing_chr(c_days3) |
                 c_delivery_method %in% c("C", "I", "V", "Y") |
                 c_budget_code %in% "SF" |
                 !(c_extract %in% "3")) , # USHE check, TODO: add site-type (query) condition?
  "C18", expr(is.na(meet_building_id_1) | !equivalent(meet_building_id_1, building_number_1)),
  "C26", expr(is.na(meet_building_id_2) | !equivalent(meet_building_id_2, building_number_2)),
  "C34", expr(is.na(meet_building_id_3) | !equivalent(meet_building_id_3, building_number_3)),
  "C18a", expr(course_conditional_check(meet_building_id_1, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_1)),
  "C26a", expr(course_conditional_check(meet_building_id_2, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_2)),
  "C34a", expr(course_conditional_check(meet_building_id_3, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_3)),
  "C19a", expr(course_conditional_check(building_number_1, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_1)),
  "C27a", expr(course_conditional_check(building_number_2, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_2)),
  "C35a", expr(course_conditional_check(building_number_3, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_3)),
  "C19c", expr(building_number_1 %in% building_inventory),
  "C27c", expr(building_number_2 %in% building_inventory),
  "C35c", expr(building_number_3 %in% building_inventory),
  "C19d", expr(building_number_1 %in% rooms_inventory),
  "C27d", expr(building_number_2 %in% rooms_inventory),
  "C35d", expr(building_number_3 %in% rooms_inventory),
  "C20a", expr(course_conditional_check(meet_room_number_1, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_1)),
  "C28a", expr(course_conditional_check(meet_room_number_2, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_2)),
  "C36a", expr(course_conditional_check(meet_room_number_3, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_3)),
  "C21a", expr(is_valid_occupancy(room_max_occupancy_1) | is.na(meet_room_number_1)),
  "C29a", expr(is_valid_occupancy(room_max_occupancy_2) | is.na(meet_room_number_2)),
  "C37a", expr(is_valid_occupancy(room_max_occupancy_3) | is.na(meet_room_number_3)),
  "C22a", expr(is_valid_values(room_use_code_1, valid_room_use_codes, missing_ok = TRUE)),
  "C30a", expr(is_valid_values(room_use_code_2, valid_room_use_codes, missing_ok = TRUE)),
  "C38a", expr(is_valid_values(room_use_code_3, valid_room_use_codes, missing_ok = TRUE)),
  "C22b", expr(course_conditional_check(room_use_code_1, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_1)),
  "C30b", expr(course_conditional_check(room_use_code_2, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_2)),
  "C38b", expr(course_conditional_check(room_use_code_3, instruction_method_code, section_format_type_code, budget_code, campus_id) | is.na(meet_days_3)),
  "C39a", expr(is_valid_dates_for_term(meet_start_date, term_id, "30", campus_id)) , # Summer
  "C39b", expr(is_valid_dates_for_term(meet_start_date, term_id, "40", campus_id)) , # Fall
  "C39c", expr(is_valid_dates_for_term(meet_start_date, term_id, "20", campus_id)) , # Spring
  "C40a", expr(is_valid_dates_for_term(meet_end_date, term_id, "30", campus_id)), # Summer
  "C40b", expr(is_valid_dates_for_term(meet_end_date, term_id, "40", campus_id)), # Fall
  "C40c", expr(is_valid_dates_for_term(meet_end_date, term_id, "20", campus_id)), # Spring
  "C41a", expr(!is_missing_chr(course_title)),
  "C41b", expr(is_missing_chr(c_title) |
                 grepl("[a-zA-Z]{2}", c_title) |
                 !(c_extract %in% "3")),
  "C41d", expr(is_missing_chr(c_title) |
                 !grepl("[^a-zA-Z0-9 /&()+:.-\\']", c_title) |
                 !(c_extract %in% "3")),
  "C42a", expr(!is_missing_chr(instructor_employee_id) | (class_size == 0)),
  "C42b", expr(!is_missing_chr(instructor_employee_id) |
                 (class_size == 0) |
                 (nchar(instructor_employee_id) == 8 &
                    grepl("^[0-9]", instructor_employee_id))),
  "C42c", expr(is_missing_chr(c_instruct_id) |
                 !grepl("^[a-zA-Z\\']", c_instruct_id) |
                 is_valid_values(substring(c_instruct_id, 1, 1), valid_i_banner)), # TODO: valid_i_banner needs a query
  "C43a", expr(!is_missing_chr(instructor_name) | (class_size == 0)),
  "C43c", expr(is_alpha_chr(c_instruct_name) | !(c_extract %in% "3")),
  "C44", expr(!is_missing_chr(section_format_type_code)),
  "C44a", expr(is_valid_values(c_instruct_type, valid_instruct_types, missing_ok = TRUE)),
  "C45", expr(!is_missing_chr(college_id)),
  "C45a", expr(is_alpha_chr(college_id)),
  "C46", expr(!is_missing_chr(academic_department_id)),
  "C46a", expr(is_alpha_chr(academic_department_id, missing_ok = TRUE)),
  "C47b", expr(is_valid_values(c_gen_ed, valid_gened_codes, missing_ok = FALSE)), # USHE rule TODO: needs gened codes (query)
  "C48a", expr(is_valid_values(c_dest_site, valid_highschools)), #USHE rule
  "C49a", expr(!is.na(class_size) & class_size != 0),
  "C49b", expr(is.na(class_size) | class_size >= 0 & class_size <= 9999),
  "C49c", expr(TODO("USHE rule comparing enrolled students to class size (involving group by/COUNT)")),
  "C51a", expr(c_level %in% c("R", "U", "G")), # USHE check
  "C51b", expr(c_crs %in% c("MATH", "MAT", "ENGL", "RDG", "WRTG", "ESL") |
                !(c_level %in% "R")), # Ignoring complex edge-case logic
  "C52a", expr(!is_missing_chr(course_reference_number)),
  "C52b", expr(is_valid_course_reference_number(course_reference_number)),
  "C52c", expr(!is_duplicated(cbind(course_reference_number, term_id))),
  "G01b", expr(!is_missing_chr(g_inst)),
  "SC01a", expr(!is_missing_chr(sc_inst)),
  "R01a", expr(!is_missing_chr(r_inst)),
  "G02a", expr(!is_missing_chr(s_id) & !is_missing_chr(s_id)), # USHE Rule
  "G02b", expr(sis_student_id %in% TODO("Need a way to bring in students table for comparing")),
  "G12a", expr(is_valid_credits(overall_cumulative_credits_earned, missing_ok = TRUE)), # TODO: verify mapping of rules to fields
  "G13a", expr(is_valid_credits(required_credits)),
  "G14a", expr(is_valid_credits(total_cumulative_ap_credits_earned, missing_ok = TRUE)),
  "G15a", expr(is_valid_credits(total_cumulative_clep_credits_earned, missing_ok = TRUE)),
  "G22a", expr(is_valid_credits(total_cumulative_credits_attempted_other_sources, missing_ok = TRUE)),
  "G23a", expr(is_valid_credits(transfer_cumulative_credits_earned, missing_ok = TRUE)),
  "G17a", expr(is_valid_values(ipeds_award_level_code, valid_ipeds_degree_award_levels, missing_ok = TRUE)),
  "G21e", expr(ssn %in% TODO("Need a way to bring in students table for comparing (same as G02b)")),
  "G03e", expr(nchar(g_first) <= 15), # USHE rule
  "G03g", expr(nchar(g_middle) <= 15), # USHE rule
  "G03i", expr(nchar(g_suffix) <= 4), # USHE rule
  "G08b", expr(is_valid_graduation_date(graduation_date)),
  "G09a", expr(is_valid_values(primary_major_cip_code, valid_cip_codes, missing_ok = TRUE)),
  "G10a", expr(!is_missing_chr(degree_type)),
  "G10b", expr(is_valid_values(degree_id, valid_degree_ids, missing_ok = FALSE)),
  "G11a", expr(is_valid_gpa(cumulative_graduation_gpa, missing_ok = TRUE)),
  "G12b", expr(is.na(g_trans_total) | g_trans_total <= 300), #USHE rule
  "G13b", expr((g_req_hrs_deg * 1.5) >= g_grad_hrs), #USHE rule
  "G14b", expr((g_req_hrs_deg * 1.5) >= g_other_hrs), #USHE rule
  "G15b", expr(g_remedial_hrs <= 60), #USHE rule
  "G16a", expr(is_valid_values(previous_degree_type, valid_previous_degree_types)),
  "G18a", expr(is.na(primary_program_id) |
                 is.numeric(required_credits) &
                 !is.na(required_credits) &
                 required_credits >= 0  &
                 overall_cumulative_credits_earned  >= required_credits),
  "G19a", expr(!is_utah_county(first_admit_county_code) | !is_missing_chr(high_school_code)),
  "G21a", expr(is_valid_student_id(sis_student_id)),
  "G21b", expr(is_valid_student_id(sis_student_id)), # Redundant unless I can assume banner_id format
  "G21d", expr(!is_duplicated(cbind(sis_student_id,
                                    graduation_date, primary_major_cip_code, degree_id,
                                    ipeds_award_level_code, primary_major_id))),
  "G24a", expr(is_valid_year(graduated_academic_year_code, missing_ok = FALSE)), # TODO: should verify matching some reference year
  "G25a", expr(is_valid_values(season, valid_seasons)),
  "G28a", expr(!is_missing_chr(degree_desc)),
  "SC03", expr(!is.na(sc_id) & !is.na(sc_id)), # USHE Rule
  "SC04a", expr(!is_missing_chr(subject_code)),
  "SC05a", expr(!is_missing_chr(course_number)),
  "SC06a", expr(!is_missing_chr(section_number)),
  "SC07a", expr(is_valid_credits(attempted_credits)),
  "SC08a", expr(is_valid_credits(earned_credits)),
  "SC09a", expr(is_valid_credits(contact_hours, missing_ok = TRUE)),
  "SC11a", expr(is_valid_credits(sc_membership_hrs)),
  "SC08b", expr(is.na(earned_credits) | earned_credits == 0 |
                  !(final_grade %in% c('CW', 'L', 'NG', 'E', 'F', 'UW',
                                       'I', 'IP', 'NC', 'AU', 'W'))),
  "SC08c", expr(final_grade %in% c(NA, "IP") | !is.na(earned_credits)),
  "SC08d", expr(!(final_grade %in% passing_grades) | (earned_credits == attempted_credits)), # TODO: this only applies to end of term--how to impose this condition?
  "SC10a", expr(is_valid_values(final_grade, valid_final_grades, missing_ok = TRUE) |
                  is.na(attempted_credits) | attempted_credits == 0),
  "SC10b", expr(!(sc_student_type %in% c("CC", "DC") &
                  sc_grade %in% c(NA, "", "IP", "I") &
                  sc_extract %in% "E")), #USHE rule
  "SC10c", expr(!(sc_student_type %in% c("CC", "DC") &
                    sc_grade %in% c(NA, "", "IP", "I") &
                    sc_extract %in% "E")), #USHE rule
  "SC12a", expr(is_valid_values(sc_student_type, c("UC", "CC", "EC", "DC"),
                                missing_ok = TRUE)), #USHE rule
  "SC12b", expr(!(sc_student_type %in% "CC") |
                  !(s_high_school %in% non_concurrent_highschools)), #USHE rule
  "SC12c", expr(sc_student_type %in% "CC" |
                  !(c_budget_code %in% c("BC", "SF"))), # USHE rule
  "SC12d", expr(s_reg_status %in% "HS" |
                  !(sc_student_type %in% c('CC', 'DC'))), #USHE rule
  "SC12e", expr(c_budget_code %in% c("BC", "SF") |
                  !(sc_student_type %in% "CC")), #USHE rule
  "SC12f", expr(s_high_school %in% ut_highschools | !(sc_student_type %in% "CC")), #USHE rule
  "SC13a", expr(is_valid_student_id(student_id)),
  "SC13b", expr(is_valid_student_id(student_id)), # Redundant unless I can assume banner_id format
  "SC14a", expr(is_valid_course_reference_number(course_reference_number)),
  "SC14b", expr(!is_missing_chr(course_reference_number)),
  "SC15b", expr(TODO("Database rule--how to get cr_type equivalent, how do sql values translate?")),
  "SC15c", expr(TODO("Database rule--how to get cr_type equivalent, how do sql values translate?")),
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
  "B12c", expr(TODO('Needs a summary of rooms data "Gross area less than sum of rooms in building"')),
  "B14a", expr(is.na(building_cost_replacement) |
                 building_cost_replacement <= 3.5e6 | !
                 is_missing_chr(building_risk_number)), # Risk number not currently in buildings table
  "B14b", expr((!is.na(building_cost_replacement) & building_cost_replacement > 3.5e6) |
                 !is_missing_chr(building_risk_number)),
  "B15a", expr(!is_missing_chr(building_auxiliary)), # Do I need to condition on ownership?
  "B15b", expr(is_valid_values(building_auxiliary, c("A", "N"), missing_ok = TRUE)),
  "B99a", expr(!is_duplicated(cbind(b_inst,b_year,b_number))), # USHE rule
  "B99b", expr(TODO('USHE rule for "buildings must have rooms", requires join to rooms data')),
  "R03b", expr(TODO('USHE rule for "Room building not in building file", requires join to buildings data')),
  "R04a", expr(!is_missing_chr(room_number)),
  "R06a", expr(!is_missing_chr(room_group1_code)),
  "R06b", expr(is_valid_values(room_group1_code, valid_room_group1_codes, missing_ok = TRUE)),
  "R07a", expr((room_group1_code %in% "Z") | !is_missing_chr(room_use_code_group)),
  "R07b", expr(is_valid_values(room_use_code_group, valid_room_use_code_groups, missing_ok = TRUE)),
  "R07c", expr(!(room_group1_code %in% "Z") | !is_missing_chr(room_use_code_group)),
  "R08a", expr(!is_missing_chr(room_use_code) | str_detect(room_number, "ROOF") | str_detect(room_number, "FIELD")),
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
                 in_range(as.numeric(room_area) / as.numeric(room_stations), 7, 16)),
  "R10d", expr(!(room_use_code %in% "210") |
                 room_group1_code %in% "Z" |
                 room_use_code_group %in% "000" |
                 in_range(as.numeric(room_area) / as.numeric(room_stations), 8, 19)),
  "R11a", expr(!is_missing_chr(room_area)),
  "R11b", expr(!(room_area %in% "0")),
  "R13a", expr(room_prorated %in% c("Y", "N")),
  "R13b", expr(!is_missing_chr(room_prorated)),
  "R13c", expr(!(room_prorated %in% "Y" &
                   room_prorated_area %in% "0" &
                   room_area %in% "0")),
  "R13d", expr(!(room_area %in% "0") | room_prorated_area %in% "0"),
  "R13e", expr(!(room_prorated_area %in% "0") | room_area %in% "0"),
  "R13f", expr(room_prorated_area %in% c("0", "0.0") | !(room_prorated %in% "N")),
  "R14a", expr(!is_missing_chr(room_prorated_area)),
  "R14b", expr(TODO('Needs a join of proration info to room info. How to get sum of prorated area?')),
  "R15a", expr(is.Date(room_activity_date) & !is.na(room_activity_date)),
  "R15b", expr(!is.na(room_activity_date)),
  "R15c", expr(is.na(room_activity_date) | age_in_range(room_activity_date, 0, Inf)), # Not flagging NAs (15b does)
  "R99a", expr(!is_duplicated(cbind(r_inst, r_year, r_build_number, r_number,
                                    r_suffix, r_group1, r_use_code))),
  "UTS03", expr(!is.na(college_id)),
  "UTS04", expr(!is.na(department_id)),
  "UTS05", expr(!is_missing_chr(high_school_code)),
  "UTS06", expr(is_degree_intent_consistent_program(student_type_code, primary_program_code)),
  "UTS07", expr(is.na(ssid) | nchar(ssid) == 7 & stringr::str_detect(ssid, "^(1|2)")),
  "UTS08", expr(!(is.na(ssid) & is_hs_type(student_type_code))),
  "UTS09", expr(is.na(cur_prgm_2) |
      cur_prgm  %in% primary_valid_programs |
      cur_prgm_2 %in% second_valid_programs),
  "UTS10", expr(entry_action != "HS" | is.na(cur_prgm) | cur_prgm %in% c("ND-CONC", "ND-SA", "ND-CE", "ND-ACE")),
  "UTS11", expr(!(first_admit_country_code %in% "US") | !is_missing_chr(first_admit_county_code)),
  "UTS12", expr(!(first_admit_country_code %in% "US") | !is_missing_chr(first_admit_state_code)),
  "UTS13", expr(!(first_admit_state_code %in% "UT") | !is_missing_chr(first_admit_county_code)),
  "UTS14", expr(!(first_admit_state_code %in% "UT") | !is_missing_chr(first_admit_country_code)),
  "UTS15", expr(!(age < 20) | !is_missing_chr(high_school_code)),
  "UTS16", expr(!(birth_date >= high_school_grad_date)),
  "UTS17", expr(is_valid_ssn_legacy(ssn)),
  "UTS18", expr(!(is_missing_chr(high_school_grad_date) & !(student_type %in% c("P", "1", "2", "3", "5")))),
  "UTS19", expr(!((citz_code != "5" & admit_state == "AS") | (citz_code == "5" & admit_state != "AS"))),
  "UTS20", expr(!(citz_code == "4" & (admit_state != "UT" | !stringr::str_detect(high_school_code, "^45")))),
  "UTS21", expr(is.na(age) || (age > 10 & age < 100)),
  "UTS22", expr(!(((citz_code != "2" & !is.na(visa_type)) | (citz_code == "2" & is.na(visa_type)) |
          (!citz_code %in% c("2","3") & !is.na(visa_type)) | (citz_code == "2" & is.na(visa_type))) &
        (visa_expire_date > Sys.Date() | is.na(visa_expire_date)))),

  "UTG01", expr(as.numeric(substr(graduated_term_id, 1, 4)) == as.numeric(lubridate::year(graduation_date))),
  "UTG02", expr(as.numeric(graduated_academic_year_code) == as.numeric(graduation_academic_year_check)),
  "UTG03", expr(is_valid_graduation_date(graduation_date)),
  "UTG04", expr(as.numeric(graduation_term_year_check) == as.numeric(graduated_academic_year_code)),
  "UTSC01", expr(
    !(budget_code %in% c("BC", "SF") &
        !(startsWith(high_school_code, "45") | high_school_code == "484870")))
)


#' Helper to get the ushe file type from the ushe element
#'
#' @param rule rule name, e.g. "S00b"
get_ushe_file <- function(rule) {
  out <- case_when(
    grepl("^UTSC", rule) ~ "Student Course",
    grepl("^SC", rule) ~ "Student Course",
    grepl("^S[0-9]", rule) ~ "Student",
    grepl("^C", rule) ~ "Course",
    grepl("^G", rule) ~ "Graduation",
    grepl("^B", rule) ~ "Buildings",
    grepl("^R", rule) ~ "Rooms",
    grepl("^UTS", rule) ~ "Student",
    grepl("^UTC", rule) ~ "Course",
    grepl("^UTG", rule) ~ "Graduation",
    TRUE ~ NA_character_
    )
  out
}

# dataframe with rule info from Data Inventory
rule_metadata <- read.csv("sandbox/rule-metadata.csv") %>%
  select(rule = ushe_rule, ref_rule, description, status,
         type, activity_date, banner) %>%

  # Where a rule exists in rule_spec, force it to be its own ref_rule.
  # I've determined these need their own specification (can't use another rule's spec)
  mutate(ref_rule = ifelse(rule %in% rule_spec$rule, rule, ref_rule)) %>%
  glimpse()


# Rule info joined to anonymous-function tibble
checklist <- rule_metadata %>%
  inner_join(rule_spec, by = c(ref_rule = "rule")) %>%
  mutate(file = get_ushe_file(rule)) %>%
  glimpse()


usethis::use_data(checklist, overwrite = TRUE)
