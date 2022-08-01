library(utHelpR)
library(utValidateR)
library(tidyverse)

data("checklist")
data("aux_info")

#Fake Data
f_blds_df <- (utValidateR::fake_buildings_validation)
f_rooms_df <- (utValidateR::fake_rooms_validation)
f_s_df <- (utValidateR::fake_student_df)
f_sc_df <- (utValidateR::fake_student_course_validation)
f_c_df <-(utValidateR::fake_course_validation)
f_gr_df <-(utValidateR::fake_graduation_validation)

#Validation Data
v_blds_df <- (utHelpR::get_data_from_sql_file("buildings_validation.sql", "edify", "sandbox"))
v_rooms_df <- (get_data_from_sql_file("rooms_validation.sql", "edify", "sandbox"))
v_s_df <- (get_data_from_sql_file("student_validation.sql", "edify", "sandbox"))
v_sc_df <- (get_data_from_sql_file("student_courses_validation.sql", "edify", "sandbox"))
v_c_df <- (get_data_from_sql_file("course_validation.sql", "edify", "sandbox"))
v_gr_df <- (get_data_from_sql_file("graduation_validation.sql", "edify", "sandbox"))

#USHE Data
#TODO Need to add USHE data sets

#Checks
#Define the check type
ushe_check_type <- "USHE"
db_check_type <- "Database"

#Define the type of check here:
check_type = db_check_type

db_blds_checks <- checklist %>%
  filter(file == "Buildings", type == check_type)
db_rooms_checks <- checklist %>%
  filter(file == "Rooms", type == check_type)
db_s_checks <- checklist %>%
  filter(file == "Student", type == check_type)
db_sc_checks <- checklist %>%
  filter(file == "Student Course", type == check_type)
db_c_checks <- checklist %>%
  filter(file == "Course", type == check_type)
db_gr_checks <- checklist %>%
  filter(file == "Graduation", type == check_type)

#Run checks by running the do_checks function.
  #Set check_df to the data set
  #Set rules_df to the rule set

check_df <- v_s_df
rules_df <- db_s_checks

check_res <- do_checks(check_df, rules_df, aux_info = aux_info)
View(db_s_checks)
View(check_res)


