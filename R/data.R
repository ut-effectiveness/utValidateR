#' Fake student-courses validation file
#'
#' A dataset containing simulated data for the student-courses validation process
#' with USHE. This dataset would be pulled from the data warehouse, and then fed into
#' the validation scripts in the utValidateR package.
#'
#' @format A data frame with 9,1570 rows and 18 variables:
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
