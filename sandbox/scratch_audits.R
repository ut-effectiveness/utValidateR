library(tidyverse)
library(here)
library(golem)
library(utValidateR)
library(AuditDataSteward)
library(utHelpR)
library(xlsx)

#This is a scratch audit report used for testing

#utHelpR::set_edify_password()

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
)



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

rooms_res <- do_checks(df_tocheck = rooms,
                       checklist = get_checklist("rooms", "database"),
                       aux_info = aux_info)

building_res <- do_checks(df_tocheck = buildings,
                       checklist = get_checklist("buildings", "database"),
                       aux_info = aux_info)




AuditDataSteward::run_app(
  student_result = student_check_res,
  course_result = course_check_res,
  student_course_result = student_course_res,
  room_result = rooms_res,
  building_result = building_res
)
