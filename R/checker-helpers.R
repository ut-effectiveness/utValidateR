
#' Returns number of days since each date in x
#'
#' @param x a Date vector or similar
#' @export
get_age <- function(x) {
  lubridate::interval(x, Sys.Date()) %/% lubridate::days(x=1)
}

age_years <- function(birthdate) {
  lubridate::interval(birthdate, Sys.Date()) / lubridate::years(x=1)
}

age_in_range <- function(birthdate, lwr, upr) {
  age <- age_years(birthdate)
  age > lwr & age < upr
}

#' Number of years since the specified date
years_since <- function(date, from = lubridate::today()) {
  lubridate::interval(date, from) / lubridate::years(1)
}

date_before_present_year <- function(date, year = format(Sys.Date(), "%Y")) {
  # TODO: what desired missingness behavior? Assuming NA OK
  out <- date < lubridate::ymd(paste0(year), "-01-01")
  out | is.na(date)
}

#' Returns a dataframe with columns <rule>_activity_date, <rule>_error_age
#'
#' @param df the dataframe to check
#' @param datecol Name of the relevant activity_date column
#' @param rule Name of the rule, e.g "S03b"
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom stats setNames
#' @export
get_activity_dates <- function(df, datecol, rule) {

  if (!exists(datecol, where = df)) {
    warning(sprintf("Could not find column %s", datecol))
    return(NULL)
  }

  out <- tibble(activity_date = df[[datecol]]) %>%
    mutate(error_age = get_age(.data$activity_date)) %>%
    setNames(paste(rule, names(.), sep = "_"))
  out
}

#' Regex to check ssn, from:
#' https://www.geeksforgeeks.org/how-to-validate-ssn-social-security-number-using-regular-expression
is_valid_ssn <- function(ssn) {
  ssn_regex <- "^(?!666|000|9\\d{2})\\d{3}-(?!00)\\d{2}-(?!0{4})\\d{4}$"
  matches_regex(ssn, ssn_regex)
}

#' Regex to check zip code, from:
#' https://regexlib.com/Search.aspx?k=us+zip+code&c=-1&m=-1&ps=20&AspxAutoDetectCookieSupport=1
#' TODO: Add/audit rules to match data inventory
is_valid_zip_code <- function(zipcode) {
  zipcode_regex <- "^\\d{5}(-\\d{4})?$"
  matches_regex(zipcode, zipcode_regex)
}

#' Returns TRUE for duplicated elements of x; FALSE otherwise
#'
#' @param x a vector
#' @export
is_duplicated <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

#' Wrapper around stringr::str_detect()
#'
#' @param string passed to str_detect()
#' @param pattern passed to str_detect()
#' @export
matches_regex <- function(string, pattern) {
  stringr::str_detect(string, pattern)
}

#' Returns TRUE for elements of x with non-alphabet characters; FALSE otherwise
#'
#' @param x a character vector
#' @export
has_nonalpha <- function(x) {
  alpha_regex <- "[^[:alpha:]]+$"
  out <- stringr::str_detect(x, alpha_regex)
  out
}




# Unclear where to put this
valid_student_type_codes <- c('N','2','R','C','T','3','P','H','0','5','1','F','S')



is_valid_act_score <- function(x) {
  # TODO: How to treat NAs? Valid or no? Saying not valid for now
  !is.na(x) & is.numeric(x) & x >= 0 & x <= 36
}

is_valid_act_scores <- function() {
  is_valid_act_score(act_composite_score) &
    is_valid_act_score(act_english_score) &
    is_valid_act_score(act_math_score) &
    is_valid_act_score(act_reading_score) &
    is_valid_act_score(act_science_score)
}

is_valid_class_level <- function(class_level_id) {
  valid_ids <- c('JR','SR','FR','GG','SO') # TODO: verify these are the correct ids
  class_level_id %in% valid_ids
}

# TODO: find out what makes a previous id valid. (S05a)
is_valid_previous_id <- function(previous_id) {
  !is.na(previous_id) & !matches_regex(previous_id, "^0*$")
}

# get_iso_country_codes <- function() {
#
# }

is_valid_country_code <- function(country_code) {
  # iso_country_codes <- get_iso_country_codes() # TODO: get valid ISO country code list
  valid_iso_country_codes <- setdiff(iso_countries$iso_alpha2, "")
  country_code %in% valid_iso_country_codes
}

is_valid_student_id <- function(student_id) {
  # TODO: what makes a student_id valid?
  !is.na(student_id)
}

is_valid_credits <- function(credits) {
  is.numeric(credits) & !is.na(credits) & credits >= 0 & credits < 10000
}

is_valid_gpa <- function(gpa) {
  !is.na(gpa) & is.numeric(gpa) & gpa >= 0 & gpa <= 5
}

#' Used in rule S13 and G06a
is_valid_gender <- function(gender) {
  toupper(gender) %in% c("M", "F")
}

is_valid_student_type <- function(student_type) {
  # TODO: verify valid codes (these are the ones used in fake_student_df)
  valid_student_type_codes <- c('N','2','R','C','T','3','P','H','0','5','1','F','S')
  student_type %in% valid_student_type_codes
}

is_freshmen_type <- function(student_type) {
  # TODO: verify freshmen type code. Also, sql references both FF and FH. Do I need to tell these apart?
  student_type == "F" # Guessing for now.
}

is_hs_type <- function(student_type) {
  # TODO: verify highschool type code
  student_type %in% "H" # Guessing for now
}

is_undergrad_type <- function(student_type) {
  # TODO: find out how student_type is coded
  undergrad_types <- c() # Fill this in!
  student_type %in% undergrad_types
}

is_grad_type <- function(student_type) {
  # TODO: find out how student_type is coded
  grad_types <- c() # Fill this in!
  student_type %in% grad_types
}

is_grad_level <- function(level) {
  level == "GG" # TODO: verify
}

is_undergrad_level <- function(level) {
  level %in% c('JR','SR','FR','SO') # TODO: verify
}


is_missing_chr <- function(name) {
  # Per sql code, name is missing if null (NA) or empty string.
  out <- is.na(name) | name == ""
  out
}


# Checks that a character vector is alpha-only (plus apostrophe)
is_alpha_chr <- function(name, missing_ok = TRUE) {
  stopifnot(is.character(name))
  missingvec <- is_missing_chr(name)

  alpha_regex <- "^[a-zA-Z']*$" # Allows apostrophe per sql code
  alphavec <- stringr::str_detect(name, alpha_regex)

  if (missing_ok) {
    out <- missingvec | alphavec
  } else {
    out <- !missingvec & alphavec
  }

  out
}

#' Checks whether a county is in Utah.
is_utah_county <- function(county_code) {
  # TODO: how are counties encoded? For now using same logic as sql--97 and 99 are out-of-state
  !(as.numeric(county_code) %in% c(97, 99))
}

is_us_state <- function(state) {
  us_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                 "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
                 "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                 "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  state %in% us_states
}

is_us_county <- function(county) {
  # TODO: how are counties encoded? For now matching the logic in the sql code
  !(county %in% 97)
}
