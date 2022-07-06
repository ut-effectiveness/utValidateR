## code to prepare `fake_student_df` dataset goes here

library(tidyverse)
library(charlatan)

## Code lists ####
sample_size <- 10000

first_admit_county_code_list <- c(039,011,033,053,0009,043,029,005,013,030,007,
                                  00097,031,045,057,001,009,027,055,047,041,023,003,037,035,049,015,017,019,
                                  021,051,025, rep(NA, 10)) %>%
  as.character()

first_admit_state_code_list <- c('KS','NV','OH','NY','BC','WV','AR','CT','YT',
                                 'LA','WY','MT','MI','DE','AA','NH','AB','OR','DC','WI','RI','AP','TN','WA',
                                 'MA','AL','ON','CO','AS','VA','AZ','ND','GU','AE','HI','IN','NE','FL','ME',
                                 'UN','SD','NJ','MP','UT','SC','VI','CA','TX','PR','FR','KY','MB','NM','MS',
                                 'MO','NC','OK','GA','ID','MN','PA','MD','AK','IL','IA','VT',
                                 rep('WY', 10), rep('UT', 1000), rep('ID', 20))

first_admit_country_code_list <- c('RQ','KS','DA','CQ','NS','VM','BM','MZ','NI',
                                   'HO','BR','RW','BY','YM','CJ','UK','RS','EC','SP','AQ','ES','SF','AJ','CO',
                                   'AS','SW','CB','PE','US','KE','UG','LY','VQ','CF','HA','IN','GM','GH','CH',
                                   'PK','TW','CA','ML','FR','MX','CS','MO','RP','CG','SU','ID','WZ','AF','JA',
                                   'JO','IS','CM',
                                   rep('US', 1000), rep(NA, 100) )

gender_code_list <- c(rep('M', 100), rep('F', 100), 'N', rep(NA, 25))

residential_housing_code_list <- c('N','R','C','H','0','M','G','S','A')

student_type_code_list <- c('N','2','R','C','T','3','P','H','0','5','1','F','S')

primary_level_class_id_list <- c('JR','SR','FR','GG','SO', NA)

primary_degree_id_list <- c('BS','BM','BAS','MAT','BME','CER1','AA','AB','MACC','AC',
                            'BIS','BA','TR','MA','AS','ND','CER0','BSN','AAS','BFA','MMFT','APE', NA)

credits <- as.integer(rnorm(sample_size, mean = 120, sd = 40)) +
  sample(c(rep(NA, 10), c(rep(1, 100)), c(rep(.5, 50)), c(rep(.3, 50))  ),
         sample_size, replace = TRUE)

## Data Frames ####

ssn <- tibble(
  first = stringr::str_pad(sample(1:999, sample_size, replace = TRUE), 3, pad = '0'),
  second = stringr::str_pad(sample(1:99, sample_size, replace = TRUE), 2, pad = '0'),
  third = stringr::str_pad(sample(1:9999, sample_size, replace = TRUE), 4, pad = '0')
) %>%
  unite(ssn, c('first', 'second', 'third'), sep = '-')

names <-  tibble(
  delete_name = charlatan::ch_name(15000, locale = 'en_US'),
  middle_name = sample( c('Bob', 'Sue', rep(NA, 10)), 15000, replace = TRUE),
  previous_last_name = sample( c('Smith', 'Jones', rep(NA, 3)), 15000, replace = TRUE ),
  previous_first_name = sample( c('Danny', 'Erin', rep(NA, 5)), 15000, replace = TRUE ),
  preferred_first_name = sample( c('Joe', 'Deb', rep(NA, 2)), 15000, replace = TRUE ),
  preferred_middle_name = sample( c('Walter', rep(NA, 100)), 15000, replace = TRUE )
) %>%
  separate(delete_name, into = c('first_name', 'last_name')) %>%
  filter(nchar(first_name) >= 4) %>%
  filter(nchar(last_name) >= 5) %>%
  head(sample_size)

local_address <- tibble(
  zip_1 = stringr::str_pad(sample(1:99999, sample_size, replace = TRUE), 5, pad = '0'),
  zip_2 = stringr::str_pad(sample(1:9999, sample_size, replace = TRUE), 4, pad = '0')
) %>%
  unite(local_address_zip_code, c('zip_1', 'zip_2'), sep = '-')

mailing_address <- tibble(
  zip_1 = stringr::str_pad(sample(1:99999, sample_size, replace = TRUE), 5, pad = '0'),
  zip_2 = stringr::str_pad(sample(1:9999, sample_size, replace = TRUE), 4, pad = '0')
) %>%
  unite(mailing_address_zip_code, c('zip_1', 'zip_2'), sep = '-')

code_stuff <- tibble(
  us_citizenship_code = sample(c('1', '2', '3', '4', '5', '6', '9', NA), sample_size, replace = TRUE),
  first_admit_county_code = sample(first_admit_county_code_list, sample_size, replace = TRUE),
  first_admit_state_code = sample(first_admit_state_code_list, sample_size, replace = TRUE),
  first_admit_country_code = sample(first_admit_country_code_list, sample_size, replace = TRUE),
  residential_housing_code = sample(residential_housing_code_list, sample_size, replace = TRUE)
)

demographic <- tibble(
  student_id = stringr::str_pad(sample(1:999999, sample_size), 8, pad = '0'),
  previous_student_id = NA,
  birth_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size),
  gender_code = sample(gender_code_list, sample_size, replace = TRUE ),
  is_hispanic_latino_ethnicity = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_asian = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_black = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_american_indian_alaskan = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_hawaiian_pacific_islander = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_white = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_international = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_other_race = sample(c(TRUE, FALSE), sample_size, replace = TRUE)
)

major <- tibble(
  primary_major_cip_code = stringr::str_pad(sample(1:99999, sample_size, replace = TRUE), 5, pad = '0'),
  student_type_code = sample(student_type_code_list, sample_size, replace = TRUE),
  primary_level_class_id = sample(primary_level_class_id_list, sample_size, replace = TRUE),
  primary_degree_id = sample(primary_degree_id_list, sample_size, replace = TRUE),
  institutional_cumulative_credits_earned = sample(credits, sample_size, replace = TRUE)
)


## Combine all the data frames into one

fake_student_df <- bind_cols(
  ssn,
  names,
  local_address,
  mailing_address,
  code_stuff,
  demographic,
  major
)

## Create the data frame
usethis::use_data(fake_student_df, overwrite = TRUE)
