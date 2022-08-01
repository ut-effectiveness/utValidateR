

#' NA-imputing double-equals replacement
#'
#' Returns TRUE if x and y are equal OR both are missing, otherwise FALSE
#'
#' @param x,y logical vectors
#' @export
equivalent <- function(x, y) {
  out <- (x == y) | (is.na(x) & is.na(y))
  out[is.na(out)] <- FALSE
  out
}

#' Checks whether an object is Date class
#'
#' @param x object to check
#' @export
is.Date <- function(x) inherits(x, "Date")

#' Computes age for birthdate and returns a logical vector indicating whether
#'  the age is between lwr and upr years
#'
#' @param birthdate Date-valued vector
#' @param lwr lower bound on age in years
#' @param upr upper bound on age in years
#'
#' @export
#'
age_in_range <- function(birthdate, lwr, upr) {
  stopifnot(is.Date(birthdate))
  age <- lubridate::interval(birthdate, Sys.Date()) / lubridate::years(x=1)
  age > lwr & age < upr
}


#' Checks whether a number is between lwr and upr
#'
#' Returns TRUE for x between lwr and upr, FALSE otherwise. Missing maps to FALSE.
#'
#' @param x Numeric vector
#' @param lwr,upr Limits on x, inclusive
#' @export
in_range <- function(x, lwr, upr) {
  !is.na(x) & x >= lwr & x <= upr
}

#' Checks whether a given date falls befor the present year
#'
#' Used in S12a, G05a
#'
#' @param date Date-valued vector to check
#' @param year defaults to the present year (as of `Sys.Date()`)
#' @export
date_before_present_year <- function(date, year = format(Sys.Date(), "%Y")) {
  # TODO: what desired missingness behavior? Assuming NA OK
  out <- date < lubridate::ymd(paste0(year), "-01-01")
  out | is.na(date) # Does not flag missing dates (that's for another check)
}


#' Returns TRUE for duplicated elements of x; FALSE otherwise
#'
#' @param x a vector
#' @export
is_duplicated <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

#' Validity checks on various values
#'
#'
#' @param x Vector to check
#' @export
is_valid_act_score <- function(x) {
  # Sql indicates missing is not OK
  !is.na(x) & is.numeric(x) & x >= 0 & x <= 36
}


#' @describeIn is_valid_act_score ssn
#' @export
is_valid_ssn <- function(x) {

  # Regex to check ssn, from:
  # https://www.geeksforgeeks.org/how-to-validate-ssn-social-security-number-using-regular-expression
  ssn_regex <- "^(?!666|000|9\\d{2})\\d{3}-(?!00)\\d{2}-(?!0{4})\\d{4}$"
  stringr::str_detect(x, ssn_regex)
}


#' @describeIn is_valid_act_score zip_code
#' @export
is_valid_zip_code <- function(x) {

  # Regex to check zip code, from:
  # https://regexlib.com/Search.aspx?k=us+zip+code
  zipcode_regex <- "^\\d{5}(-\\d{4})?$"
  stringr::str_detect(x, zipcode_regex)
}

#' @describeIn is_valid_act_score SSID
#' @export
is_valid_ssid <- function(x) {
  stopifnot(is.character(x))
  passes <- is.na(x) | (nchar(x) == 7) & (substring(x, 1, 1) %in% c("1", "2"))
  passes
}

#' @describeIn is_valid_act_score 4-digit year in 2000-2099 as character string
#' @export
is_valid_year <- function(x) {
  pass0 <- !is_missing_chr(x)
  pass1 <- grepl("^20\\d{2}$", x) # valid if 2000-2099
}


#' @describeIn is_valid_act_score student_id
#' @export
is_valid_student_id <- function(x) {
  !is_missing_chr(x) # & nchar(x) == 9 # TODO: verify no need to check length or anything else
}

#' @describeIn is_valid_act_score previous_student_id
#' @export
is_valid_previous_id <- function(x) {
  # TODO: learn what constitutes a valid previous id. For now I'm assuming it's
  # any string of all 0's, since that's what the sql indicates. Missing should
  # be OK though, right?
  is_missing_chr(x) | !stringr::str_detect(x, "^0+$")
}

#' @describeIn is_valid_act_score first_admit_country_code
#' @export
is_valid_country_code <- function(x) {
  valid_iso_country_codes <- setdiff(iso_countries$iso_alpha2, "")
  x %in% valid_iso_country_codes
}


#' @describeIn is_valid_act_score credits_earned
#' @export
is_valid_credits <- function(x) {
  is.numeric(x) & !is.na(x) & x >= 0 & x < 10000
}

#' @describeIn is_valid_act_score character-valued credits (min_credits and max_credits)
#' @export
is_valid_credits_chr <- function(x) {
  # Invalid if: empty string, length > 4, credits > 99.9, credits < 0, non-numeric, AND (not 0, not 0.0)
  x_num <- as.numeric(x)
  passes <- x != "" &
    nchar(x) <= 4 &
    x_num <= 99.9 &
    x_num >= 0 &
    !is.na(x_num)

  out <- passes
  out
}

#' @describeIn is_valid_act_score gpa
#' @export
is_valid_gpa <- function(x) {
  !is.na(x) & is.numeric(x) & x >= 0 & x <= 5
}

#' @describeIn is_valid_act_score student_type_code
#' @export
is_valid_student_type <- function(x) {
  # TODO: verify valid codes (these are the ones used in fake_student_df)
  valid_student_type_codes <- c('N','2','R','C','T','3','P','H','0','5','1','F','S')
  x %in% valid_student_type_codes
}

#' @describeIn is_valid_act_score room occupancy
#' @export
is_valid_occupancy <- function(x) {
  # Checks: empty string, length > 4, value > 9999, value < 0, non-number, AND not 0
  x_num <- as.numeric(x)
  passes <- (x != "") &
    (nchar(x) > 4) &
    (x_num <= 9999) &
    (x_num >= 0) &
    !is.na(x_num)

  out <- (x_num %in% 0) | passes
  out
}

#' @describeIn is_valid_act_score matching a vector of values
#' @param valid_values vector of valid values x can take
#' @param missing_ok if TRUE (default), do not flag `NA` or `""` values
#' @export
is_valid_values <- function(x, valid_values, missing_ok = TRUE) {
  passes <- x %in% valid_values
  if (missing_ok) {
    passes <- is_missing_chr(x) | passes
  }
  passes
}

#' @describeIn is_valid_act_score course reference number
#' @export
is_valid_course_reference_number <- function(x) {
  # Invalid if: empty string, length > 5, value > 99999, value < 0, non-numeric.
  x_num <- as.numeric(x)
  passes <- nchar(x) <= 5 &
    x_num >= 0 &
    x_num <= 99999 &
    !is.na(x_num)

  out <- is_missing_chr(x) | passes # Another rule checks missingness.
}

#' @describeIn is_valid_act_score graduation date
#' @importFrom lubridate month day days
#' @export
is_valid_graduation_date <- function(x) {
  # Between 6/30 and 7/1 -- although I find this suspicious
  # TODO: add check on year if data allow it
  month(x + days(1)) == 7 & day(x + days(1)) <= 2
}

#' Helper functions for student_type  and level_class_id categories
#'
#' @param student_type vector of student_type_code values
#' @export
is_freshmen_type <- function(student_type) {
  # TODO: verify freshmen type code. Also, sql references both FF and FH. Do I need to tell these apart?
  student_type == "F" # Guessing for now.
}

#' @describeIn is_freshmen_type returns TRUE for high-school student_types
#' @export
is_hs_type <- function(student_type) {
  # TODO: verify highschool type code
  student_type %in% "H" # Guessing for now
}

#' @describeIn is_freshmen_type returns TRUE for undergrad student_types
#' @export
is_undergrad_type <- function(student_type) {
  # TODO: find out how student_type is coded
  undergrad_types <- c('P', 'T', 'R', 'C', 'F', 'H')
  student_type %in% undergrad_types
}

#' @describeIn is_freshmen_type returns TRUE for grad-school student_types
#' @export
is_grad_type <- function(student_type) {
  # TODO: find out how student_type is coded
  grad_types <- c('1', '5', '2', '4') # Fill this in!
  student_type %in% grad_types
}

#' @describeIn is_freshmen_type returns TRUE for undergrad level_class_ids
#' @export
is_undergrad_level <- function(level) {
  level %in% c('JR','SR','FR','SO') # TODO: verify
}

#' @describeIn is_freshmen_type returns TRUE for grad-school level_class_ids
#' @param level vector of primary_level_class_id values
#' @export
is_grad_level <- function(level) {
  level == "GG" # TODO: verify
}



#' Checks that a character vector is alpha-only (plus apostrophe)
#'
#' @param x Character vector
#' @param missing_ok if TRUE (default), do not flag `NA` or `""` values
#' @export
is_alpha_chr <- function(x, missing_ok = TRUE) {
  stopifnot(is.character(x))
  missingvec <- is_missing_chr(x)

  alpha_regex <- "^[a-zA-Z']*$" # Allows apostrophe per sql code
  alphavec <- stringr::str_detect(x, alpha_regex)

  if (missing_ok) {
    out <- missingvec | alphavec
  } else {
    out <- !missingvec & alphavec
  }
  out
}


#' Checker for missing character values
#'
#' @describeIn is_alpha_chr returns TRUE for NA or empty-string values
#' @export
is_missing_chr <- function(x) {
  # Per sql code, x is missing if null (NA) or empty string.
  stopifnot(is.character(x))
  out <- is.na(x) | x == ""
  out
}


#' Checks whether a county is in Utah.
#'
#' @param county_code Vector of first_admit_county_code values
#' @export
is_utah_county <- function(county_code) {
  # Per USHE logic: 99 is US but not UT, 97 is non-US
  nchar(county_code) %in% 2 & !(county_code %in% c("97", "99"))
}

#' @describeIn is_utah_county Checks whether a county is in USA
#' @export
is_us_county <- function(county_code) {
  # Per USHE logic: 99 is US but not UT, 97 is non-US
  nchar(county_code) == 2 & !(county_code %in% "97")
}

#' @describeIn is_utah_county Checks whether a state code is in USA
#'
#' @param state first_admit_state_code
#' @export
is_us_state <- function(state) {
  us_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                 "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
                 "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                 "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  state %in% us_states
}

#' @describeIn is_utah_county Checks whether a state code is not in USA
#'
#' @param state first_admit_state_code
#' @export
is_nonus_state <- function(state) {
  # Only return TRUE if a state code is specified (a 2-digit code) but not one of the US states
  (nchar(state) == 2) & !is_us_state(state)
}

#' Helper for troubleshooting and communicating known needs
#'
#' Primarily intended to aid in the development of this package
#'
#' @param msg Message to be displayed
#' @export
TODO <- function(msg) {
  stop()
}
