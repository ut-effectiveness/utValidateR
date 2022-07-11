## code to prepare `fake_student_course_validation` dataset goes here

source(here::here('data-raw', 'fake_data_functions.R'))

library(utHelpR)

student_course_validation_pull <- get_data_from_sql_file(
  here::here('sandbox', 'fake_data', 'sql', 'student_course_validation.sql'),
  'edify',
  'none') %>%
  order_cols()

student_course_validation <- student_course_validation_pull %>%
  select(-sis_system_id, -ssn, -student_id) %>%
  mutate(ssn = ssn(nrow(student_course_validation_pull))[['ssn']]) %>%
  mutate(sis_system_id = fake_id(8, nrow(student_course_validation_pull), TRUE)) %>%
  mutate(sis_student_id = fake_id(8, nrow(student_course_validation_pull), TRUE)) %>%
  stir(final_grade) %>%
  stir(latest_high_school_code) %>%
  stir(ssbsect_activity_date) %>%
  stir(earned_credits) %>%
  stir(section_number) %>%
  order_cols()

#usethis::use_data(fake_student_course_validation, overwrite = TRUE)
