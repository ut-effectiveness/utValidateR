#' Fake student file containing 10,000 rows
#'
#' A dataset containing a simulated file from the student file submission process
#' with USHE. This file would be pulled from the data warehouse, and then fed into
#' the validation and shaping scripts in the utValidateR package.
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
#'   ...
#' }
#' @source The script for creating this data set is in the dev folder
"fake_student_df"



#' Rule specification using R expressions
#'
#' This tibble is the container for all rules that can be applied to USHE or Database
#' validations. The `checker` column in particular contains the code for performing
#' each rule check, as an expression to be computed on the dataframe to check.
#'
#' @format a tibble with ?? columns and ?? variables:
#' \describe{
#'   \item{rule}{Rule name, e.g. S01a}
#'   \item{ref_rule}{For multiple rules with the same definition, which rule contains the definition}
#'   \item{description}{Short description of rule}
#'   \item{status}{Whether breaking the rule is a "Failure" or a "Warning"}
#'   \item{activity_date}{Name of the relevant activity-date variable}
#'   \item{checker}{Expression defining the rule}
#'   \item{file}{The USHE file to which the rule applies}
#' }
#' @source Defined in data-raw/checklist.R
"checklist"


#' Auxiliary information for performing rule checks
#'
#' While the `checklist` object contains the expressions to perform each rule check,
#' there may be additional information required beyond what is defined in the
#' `checker` expression and the dataframe being checked. The `aux_info` provides
#' this information as a list, primarily of vectors of valid values.
#'
#' @format a list with (at last count) 24 elements:
#' \describe{
#'   \item{valid_s_reg_statuses}{From the google doc shared by Justin}
#'   \item{valid_c_line_items}{From the google doc shared by Justin}
#'   \item{valid_instruction_method_codes}{From the google doc shared by Justin}
#'   \item{valid_program_types}{From the google doc shared by Justin}
#'   \item{valid_instruct_types}{From the google doc shared by Justin}
#'   \item{valid_cip_codes}{From the CIPCode2020.csv in sandbox/, from Justin}
#'   \item{valid_highschools}{From sandbox/highschools.txt (pipe-separated) - HS_ACT_CODE}
#'   \item{non_concurrent_highschools}{Pulled from sql code in data inventory for SC12b}
#'   \item{concurrent_course_ids}{from sandbox/analytics_quad_concurrent_courses.csv,
#'         but should change in future (TODO)}
#'   \item{ut_highschools}{From sandbox/highschools.txt}
#'   \item{valid_degree_ids}{From Reference.dbo.Degree_type.xlsx, now in "sandbox/degree-types.csv"}
#'   \item{valid_previous_degree_types}{From the google doc shared by Justin}
#'   \item{valid_level_class_ids}{From the google doc shared by Justin (s_levels)}
#'   \item{valid_seasons}{Pulled from sql in G25a row of data inventory xlsx}
#'   \item{valid_final_grades}{From the google doc shared by Justin}
#'   \item{passing_grades}{From the sql in SC08d in data inventory}
#'   \item{valid_building_location_codes}{From the google doc shared by Justin}
#'   \item{valid_ownership_codes}{From the google doc shared by Justin}
#'   \item{valid_building_condition_codes}{From the google doc shared by Justin}
#'   \item{valid_room_group1_codes}{From the google doc shared by Justin}
#'   \item{valid_room_use_code_groups}{From the google doc shared by Justin}
#'   \item{valid_room_use_codes}{From the google doc shared by Justin}
#'   \item{building_inventory}{From sandbox/analytics_quad_buildings.csv, but should change (TODO)}
#'   \item{rooms_inventory}{From sandbox/analytics_quad_buildings.csv, but should change (TODO)}
#'
#'
#' }
#' @source Defined in data-raw/aux_info.R
"aux_info"
