## code to prepare `fake_course_validation` dataset goes here

source(here::here('data-raw', 'fake_data_functions.R'))

library(utHelpR)

course_validation_pull <- get_data_from_sql_file(
  here::here('sandbox', 'fake_data', 'sql', 'course_validation.sql'),
  'edify',
  'none') %>%
  order_cols()

sample_size <- nrow(course_validation_pull)

fake_course_validation <- course_validation_pull %>%
  select(-instructor_employee_id, -instructor_name) %>%
  mutate(instructor_employee_id = fake_id(8, sample_size, TRUE)) %>%
  bind_cols(names(sample_size, 1.5*sample_size)[c('first_name', 'last_name')]) %>%
  unite(instructor_name, c('first_name', 'last_name'), sep = " ") %>%
  order_cols()

usethis::use_data(fake_course_validation, overwrite = TRUE)
