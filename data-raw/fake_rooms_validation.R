## code to prepare `fake_rooms_validation` dataset goes here

source(here::here('data-raw', 'fake_data_functions.R'))

library(utHelpR)

rooms_validation_pull <- get_data_from_sql_file(
  here::here('sandbox', 'fake_data', 'sql', 'rooms_validation.sql'),
  'edify',
  'none') %>%
  order_cols()

fake_rooms_validation <- rooms_validation_pull

usethis::use_data(fake_rooms_validation, overwrite = TRUE)
