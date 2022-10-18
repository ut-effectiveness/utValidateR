library(tidyverse)
library(here)
library(golem)
library(utValidateR)
library(AuditDataSteward)
library(utHelpR)
library(xlsx)

#Define variables
v_term = "202240"

#Pulling the data
student <- utHelpR::get_data_from_sql_file(file_name = 'student.sql',
                                           dsn = 'edify',
                                           context = 'sandbox'
) %>%
  filter(term_id == v_term)

student_courses <- utHelpR::get_data_from_sql_file(file_name = 'student_courses.sql',
                                                   dsn = 'edify',
                                                   context = 'sandbox'
) %>%
  filter(term_id == v_term)

courses <- utHelpR::get_data_from_sql_file(file_name = 'course_validation.sql',
                                           dsn = 'edify',
                                           context = 'sandbox'
) %>%
  filter(term_id == v_term)

buildings <- utHelpR::get_data_from_sql_file(file_name = 'buildings.sql',
                                             dsn = 'edify',
                                             context = 'sandbox'
)

rooms <- utHelpR::get_data_from_sql_file(file_name = 'rooms.sql',
                                         dsn = 'edify',
                                         context = 'sandbox'
)

graduation <- utHelpR::get_data_from_sql_file(file_name = 'graduation.sql',
                                              dsn = 'edify',
                                              context = 'sandbox')




#Use the utValidateR Package to perform audits
data("checklist")
data("aux_info")

#fk_student <- utValidateR::fake_student_df
#View(course_checks)

student_check_res <- do_checks(df_tocheck = student,
                               checklist = get_checklist("student", "database"),
                               aux_info = aux_info)

student_course_res <- do_checks(df_tocheck = student_courses,
                                checklist = get_checklist("student course", "database"),
                                aux_info = aux_info)

course_check_res <- do_checks(df_tocheck = courses,
                              checklist = get_checklist("course", "database"),
                              aux_info = aux_info)


AuditDataSteward::run_app(
  student_result = student_check_res
)






# course_results_pl <- course_check_res %>%
#   select(
#     everything()
#   ) %>%
#   pivot_longer(
#     cols = contains("status")
#   ) %>%
#   mutate(
#     rule = unlist(str_extract_all(name, "^[^_]+"))
#   ) %>%
#   filter(value == "Failure") %>%
#   left_join(course_checks, by = "rule") %>%
#   mutate(
#     checker = unlist(checker)
#   ) %>%
#   select(
#     rule,
#     description,
#     checker,
#     value,
#     course_reference_number,
#     course_number,
#     subject_code,
#     section_number,
#     course_title,
#     instructor_employee_id,
#     meet_building_id_1,
#     meet_building_id_2,
#     meet_building_id_3,
#     building_number_1,
#     building_number_2,
#     building_number_3,
#     budget_code,
#     class_size,
#     college_id,
#     contact_hours,
#     meet_days_1,
#     meet_days_2,
#     meet_days_3,
#     instruction_method_code,
#     academic_department_id,
#     meet_end_date,
#     instructor_name,
#     section_format_type_code,
#     course_max_credits,
#     course_min_credits,
#     program_type,
#     meet_room_number_1,
#     meet_room_number_2,
#     meet_room_number_3,
#     room_max_occupancy_1,
#     room_max_occupancy_2,
#     room_max_occupancy_3,
#     room_use_code_1,
#     room_use_code_2,
#     room_use_code_3,
#     campus_id,
#     meet_start_time_1,
#     meet_start_time_2,
#     meet_start_time_3
#   )
#
# glimpse(course_results_pl)
#
#
# View(course_results_pl)
#
#
# View(course_results)
# View(course_check_res)
#
# write_excel_csv(course_results_pl, here('sensitive', 'course_audits.csv'))

