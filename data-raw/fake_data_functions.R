## Functions we use to create the fake data sets


library(here)
library(stringr)
library(tidyverse)
library(charlatan)
library(rlang)


fake_id <- function(id_length, col_length, unique) {
  # id_length is the length of the id number
  # col_length is how many id numbers you will need
  # unique is if you want each id to be unique


  # use in integer_list to generate list of integers to sample from
  finish_num <- paste(rep(9, id_length), collapse = "")
  # random selection of integers
  integer_list <- sample(1:finish_num, col_length, replace = unique)
  # format the integers
  stringr::str_pad(integer_list, id_length, pad = '0')
}


# Create true false columns
tf <- function(sample_size) {
  sample(c(TRUE, FALSE), sample_size, replace = TRUE)
}

# create act score columns
act <- function(sample_size) {
  sample(c(1:36, NA), sample_size, replace = TRUE)
}

# add NA values to a vector
add_na <- function(num_na, vec) {
  c(vec, rep(NA, {{num_na}} )) %>% sample()
}

# Create a tibble of names
names <- function(sample_size, extra) {

  tibble(
    delete_name = charlatan::ch_name(extra, locale = 'en_US'),
    middle_name = sample( c('Bob', 'Sue', rep(NA, 10)), extra, replace = TRUE),
    previous_last_name = sample( c('Smith', 'Jones', rep(NA, 3)), extra, replace = TRUE ),
    previous_first_name = sample( c('Danny', 'Erin', rep(NA, 5)), extra, replace = TRUE ),
    preferred_first_name = sample( c('Joe', 'Deb', rep(NA, 2)), extra, replace = TRUE ),
    preferred_middle_name = sample( c('Walter', rep(NA, 100)), extra, replace = TRUE )
  ) %>%
    separate(delete_name, into = c('first_name', 'last_name')) %>%
    filter(nchar(first_name) >= 4) %>%
    filter(nchar(last_name) >= 5) %>%
    head(sample_size)
}

# Create a tibble of social security numbers
ssn <- function(sample_size) {
  tibble(
    first = stringr::str_pad(sample(1:999, sample_size, replace = TRUE), 3, pad = '0'),
    second = stringr::str_pad(sample(1:99, sample_size, replace = TRUE), 2, pad = '0'),
    third = stringr::str_pad(sample(1:9999, sample_size, replace = TRUE), 4, pad = '0')
  ) %>%
    unite(ssn, c('first', 'second', 'third'), sep = '-')
}

# Create a tibble of zip codes
address <- function(sample_size) {
  tibble(
    zip_1 = stringr::str_pad(sample(1:99999, sample_size, replace = TRUE), 5, pad = '0'),
    zip_2 = stringr::str_pad(sample(1:9999, sample_size, replace = TRUE), 4, pad = '0')
  ) %>%
    unite(local_address_zip_code, c('zip_1', 'zip_2'), sep = '-')
}

# put the columns in alphabetic order
order_cols <- function(data_df) {
  data_df %>%
    select(order(colnames(data_df)))
}

# randomly permute a column
stir <- function(data_df, col) {
  col_name <- ensym(col)
  data_df %>% mutate( {{col_name}} := sample(data_df[[ col_name ]], nrow(data_df), replace = FALSE))
}

