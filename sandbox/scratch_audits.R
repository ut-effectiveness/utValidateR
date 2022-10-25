library(tidyverse)
library(here)
library(golem)
library(utValidateR)
library(AuditDataSteward)
library(utHelpR)
library(xlsx)

#This is a scratch audit report used for testing

#Define variables
v_term = "202240"

#Pulling the data
student <- utHelpR::get_data_from_sql_file(file_name = 'student.sql',
                                           dsn = 'edify',
                                           context = 'sandbox'
)

courses <- utHelpR::get_data_from_sql_file(file_name = 'course.sql',
                                           dsn = 'edify',
                                           context = 'sandbox'
)

student_courses <- utHelpR::get_data_from_sql_file(file_name = 'student_courses.sql',
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
                               aux_info = aux_info,
                               verbose = TRUE)

course_check_res <- do_checks(df_tocheck = courses,
                              checklist = get_checklist("course", "database"),
                              aux_info = aux_info,
                              verbose = TRUE)

student_course_res <- do_checks(df_tocheck = student_courses,
                                checklist = get_checklist("student course", "database"),
                                aux_info = aux_info)




AuditDataSteward::run_app(
  student_result = student_check_res,
  course_result = course_check_res
)

View(courses %>%
  select(meet_building_id_3,
         meet_days_3,
         meet_start_time_3))
