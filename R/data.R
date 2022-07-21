#' Fake rooms validation file
#'
#' A dataset containing simulated data for the rooms validation process
#' with USHE. This dataset would be pulled from the data warehouse, and then fed into
#' the validation scripts in the utValidateR package.
#'
#' @format A data frame with 7,102 rows and 10 variables:
#' \describe{
#'    \item{room_acitivity_date}{}
#'    \item{room_area}{}
#'    \item{room_disabled_access}{}
#'    \item{room_name}{}
#'    \item{room_number}{}
#'    \item{room_prorated}{}
#'    \item{room_prorated_area}{}
#'    \item{room_stations}{}
#'    \item{room_use_code}{}
#'    \item{rooms_id}{}
#' }
#' @source The script for creating this data set is in the dev folder
'fake_rooms_validation'

#' Fake buildings validation file
#'
#' A dataset containing simulated data for the buildings validation process
#' with USHE. This dataset would be pulled from the data warehouse, and then fed into
#' the validation scripts in the utValidateR package.
#'
#' @format A data frame with 112 rows and 13 variables:
#' \describe{
#'    \item{building_activity_date}{}
#'    \item{building_area_gross}{}
#'    \item{building_auxiliary}{}
#'    \item{building_auxiliary_12}{}
#'    \item{building_condition_code}{}
#'    \item{building_condition_desc}{}
#'    \item{building_construction_year}{}
#'    \item{building_cost_myr}{}
#'    \item{building_cost_replacement}{}
#'    \item{building_location_code}{}
#'    \item{building_location_desc}{}
#'    \item{building_number}{}
#'    \item{building_remodel_year}{}
#' }
#' @source The script for creating this data set is in the dev folder
'fake_buildings_validation'

#' Fake graduation validation file
#'
#' A dataset containing simulated data for the graduation validation process
#' with USHE. This dataset would be pulled from the data warehouse, and then fed into
#' the validation scripts in the utValidateR package.
#'
#' @format A data frame with 13,866 rows and 44 variables:
#' \describe{
#'    \item{birth_date}{}
#'    \item{cumulative_graduation_gpa}{}
#'    \item{degree_desc}{}
#'    \item{degree_id}{}
#'    \item{ethnicity_code}{}
#'    \item{ethnicity_desc}{}
#'    \item{first_admit_county_code}{}
#'    \item{first_name}{}
#'    \item{gender_code}{}
#'    \item{gorsdav_activity_date}{}
#'    \item{graduated_academic_year_code}{}
#'    \item{graduated_term_id}{}
#'    \item{graduation_date}{}
#'    \item{high_school_code}{}
#'    \item{ipeds_award_level_code}{}
#'    \item{ipeds_race_ethnicity}{}
#'    \item{is_american_indian_alaskan}{}
#'    \item{is_asian}{}
#'    \item{is_black}{}
#'    \item{is_hawaiian_pacific_islander}{}
#'    \item{is_hispanic_latino_ethnicity}{}
#'    \item{is_international}{}
#'    \item{is_other_race}{}
#'    \item{is_white}{}
#'    \item{last_name}{}
#'    \item{middle_name}{}
#'    \item{name_suffix}{}
#'    \item{overall_cumulative_credits_earned}{}
#'    \item{previous_degree_type}{}
#'    \item{primary_major_cip_code}{}
#'    \item{primary_major_college_desc}{}
#'    \item{primary_major_desc}{}
#'    \item{required_credits}{}
#'    \item{season}{}
#'    \item{shrdgmr_activity_date}{}
#'    \item{shrtgpa_activity_date}{}
#'    \item{sis_student_id}{}
#'    \item{sis_system_id}{}
#'    \item{ssn}{}
#'    \item{total_cumulative_ap_credits_earned}{}
#'    \item{total_cumulative_clep_credits_earned}{}
#'    \item{total_cumulative_credits_attempted_other_sources}{}
#'    \item{total_remedial_hours}{}
#'    \item{transfer_cumulative_credits_earned}{}
#' }
#' @source The script for creating this data set is in the dev folder
'fake_graduation_validation'

#' Fake student-courses validation file
#'
#' A dataset containing simulated data for the student-courses validation process
#' with USHE. This dataset would be pulled from the data warehouse, and then fed into
#' the validation scripts in the utValidateR package.
#'
#' @format A data frame with 91,570 rows and 18 variables:
#' \describe{
#'    \item{attempted_credits}{}
#'    \item{attribute_code}{}
#'    \item{budget_code}{}
#'    \item{contact_hours}{}
#'    \item{course_level_id}{}
#'    \item{course_number}{}
#'    \item{course_reference_number}{}
#'    \item{earned_credits}{}
#'    \item{final_grade}{}
#'    \item{latest_high_school_code}{}
#'    \item{part_term_weeks}{}
#'    \item{section_number}{}
#'    \item{sis_student_id}{}
#'    \item{sis_system_id}{}
#'    \item{ssbsect_activity_date}{}
#'    \item{ssn}{}
#'    \item{subject_code}{}
#'    \item{term_id}{}
#' }
#' @source The script for creating this data set is in the dev folder
'fake_student_course_validation'

#' Fake student validation file
#'
#' A dataset containing simulated data for the student-courses validation process
#' with USHE. This dataset would be pulled from the data warehouse, and then fed into
#' the validation scripts in the utValidateR package.
#'
#' @format A data frame with 10,000 rows and 54 variables:
#' \describe{
#'    \item{act_composite_score}{}
#'    \item{act_english_score}{}
#'    \item{act_math_score}{}
#'    \item{act_reading_score}{}
#'    \item{act_science_score}{}
#'    \item{birth_date}{}
#'    \item{first_admit_country_code}{}
#'    \item{first_admit_county_code}{}
#'    \item{first_admit_state_code}{}
#'    \item{first_name}{}
#'    \item{full_time_part_time_code}{}
#'    \item{gender_code}{}
#'    \item{high_school_graduation_date}{}
#'    \item{institutional_cumulative_credits_earned}{}
#'    \item{institutional_cumulative_gpa}{}
#'    \item{is_american_indian_alaskan}{}
#'    \item{is_asian}{}
#'    \item{is_bia}{}
#'    \item{is_black}{}
#'    \item{is_hawaiian_pacific_islander}{}
#'    \item{is_hispanic_latino_ethnicity}{}
#'    \item{is_international}{}
#'    \item{is_other_race}{}
#'    \item{is_pell_awarded}{}
#'    \item{is_pell_eligible}{}
#'    \item{is_white}{}
#'    \item{last_name}{}
#'    \item{level_id}{}
#'    \item{local_address_zip_code}{}
#'    \item{mailing_address_zip_code}{}
#'    \item{middle_name}{}
#'    \item{preferred_first_name}{}
#'    \item{preferred_middle_name}{}
#'    \item{previous_first_name}{}
#'    \item{previous_last_name}{}
#'    \item{previous_student_id}{}
#'    \item{primary_degree_id}{}
#'    \item{primary_level_class_id}{}
#'    \item{primary_major_cip_code}{}
#'    \item{primary_major_college_desc}{}
#'    \item{primary_major_college_id}{}
#'    \item{residential_housing_code}{}
#'    \item{sabsupl_activity_date}{}
#'    \item{secondary_major_cip_code}{}
#'    \item{secondary_major_college_desc}{}
#'    \item{secondary_major_college_id}{}
#'    \item{sgbstdn_activity_date}{}
#'    \item{spriden_activity_date}{}
#'    \item{ssn}{}
#'    \item{student_id}{}
#'    \item{student_type_code}{}
#'    \item{term_id}{}
#'    \item{transfer_cumulative_credits_earned}{}
#'    \item{us_citizenship_code}{}
#'
#' }
#' @source The script for creating this data set is in the dev folder
"fake_student_df"
