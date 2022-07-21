## code to prepare `fake_graduation_validation` dataset goes here

source(here::here('data-raw', 'fake_data_functions.R'))

library(utHelpR)

graduation_validation_pull <- get_data_from_sql_file(
  here::here('sandbox', 'fake_data', 'sql', 'graduation_validation.sql'),
  'edify',
  'none') %>%
  order_cols()

sample_size <- nrow(graduation_validation_pull)

fake_graduation_validation <- graduation_validation_pull %>%
  select(-sis_system_id, -ssn, -student_id, -birth_date,
         -first_name, -last_name, -middle_name) %>%
  mutate(ssn = ssn(sample_size)[['ssn']]) %>%
  mutate(sis_system_id = fake_id(8, sample_size, TRUE)) %>%
  mutate(sis_student_id = fake_id(8, sample_size, TRUE)) %>%
  mutate(birth_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size)) %>%
  stir("cumulative_graduation_gpa") %>%
  stir("degree_desc") %>%
  stir("degree_id") %>%
  stir("ethnicity_code") %>%
  stir("ethnicity_desc") %>%
  stir("first_admit_county_code") %>%
  stir("gender_code") %>%
  stir("gorsdav_activity_date") %>%
  stir("graduation_date") %>%
  stir("high_school_code") %>%
  stir("ipeds_race_ethnicity") %>%
  stir("is_american_indian_alaskan") %>%
  stir("is_asian") %>%
  stir("is_black") %>%
  stir("is_hawaiian_pacific_islander") %>%
  stir("is_hispanic_latino_ethnicity") %>%
  stir("is_international") %>%
  stir("is_other_race") %>%
  stir("is_white") %>%
  stir("name_suffix") %>%
  stir("overall_cumulative_credits_earned") %>%
  stir("previous_degree_type") %>%
  stir("primary_major_cip_code") %>%
  stir("primary_major_college_desc") %>%
  stir("primary_major_desc") %>%
  stir("shrdgmr_activity_date") %>%
  stir("shrtgpa_activity_date") %>%
  stir("total_cumulative_ap_credits_earned") %>%
  stir("total_cumulative_clep_credits_earned") %>%
  stir("total_cumulative_credits_attempted_other_sources") %>%
  stir("total_remedial_hours") %>%
  stir("transfer_cumulative_credits_earned") %>%
  bind_cols(names(sample_size, 1.5*sample_size)[c('first_name', 'last_name', 'middle_name')]) %>%
  order_cols()

usethis::use_data(fake_graduation_validation, overwrite = TRUE)

