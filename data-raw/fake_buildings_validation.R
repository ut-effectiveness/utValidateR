## code to prepare `fake_buildings_validation` dataset goes here

source(here::here('data-raw', 'fake_data_functions.R'))

library(utHelpR)

buildings_validation_pull <- get_data_from_sql_file(
  here::here('sandbox', 'fake_data', 'sql', 'buildings_validation.sql'),
  'edify',
  'none') %>%
  order_cols()

fake_buildings_validation <- buildings_validation_pull

usethis::use_data(fake_buildings_validation, overwrite = TRUE)
