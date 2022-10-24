

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

#' Computes age from startdate and returns a logical vector indicating whether
#'  the age is between lwr and upr years
#'
#' @param startdate Date-valued vector
#' @param lwr lower bound on age in years
#' @param upr upper bound on age in years
#'
#' @export
#'
age_in_range <- function(startdate, lwr, upr) {
  stopifnot(is.Date(startdate))
  age <- lubridate::interval(startdate, Sys.Date()) / lubridate::years(x=1)
  out <- age > lwr & age < upr
  out & !is.na(out) # map missing to FALSE
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

#' Checks whether a given date falls before the present year
#'
#' Used in S12a, G05a
#'
#' @param date Date-valued vector to check
#' @param year defaults to the present year (as of `Sys.Date()`)
#' @export
date_before_present_year <- function(date, year = format(Sys.Date(), "%Y")) {
  # TODO: what desired missingness behavior? Assuming NA OK
  out <- date < lubridate::ymd(paste0(year, "-01-01"))

  out | is.na(date) # Does not flag missing dates (that's for another check)
}


#' Returns TRUE for duplicated elements of x; FALSE otherwise
#'
#' @param x a vector
#' @param count_missing if TRUE, count multiple missing values as duplicates,
#'  otherwise treat as incomparable. Defaults to FALSE.
#' @export
is_duplicated <- function(x, count_missing = FALSE) {
  if (is.array(x)) x <- as.data.frame(x) # results from `cbind`ing vectors

  out <- duplicated(x) | duplicated(x, fromLast = TRUE)

  # by default duplicated() counts multiple missing as duplicated. We want default
  # to be not to count missing as duplicated, i.e. is_duplicated(c(NA, NA)) == c(F, F)
  if (!count_missing) {
    missingvec <- is.na(x)
    if (is.array(missingvec)) missingvec <- apply(missingvec, 1, any)
    out[missingvec] <- FALSE
  }

  out
}

#' Validity checks on various values
#'
#' Checks whether input matches a vector of values
#'
#' @param x Vector to check
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

#' @describeIn is_valid_values ACT score7
#' @export
is_valid_act_score <- function(x, missing_ok = TRUE) {
  # Sql indicates missing is not OK
  out <- is.numeric(x) & x >= 0 & x <= 36

  out <- if(missing_ok) {
    out | is.na(x)
  } else {
    out & !is.na(x)
  }
  out
}


#' @describeIn is_valid_values ssn
#' @export
is_valid_ssn <- function(x, missing_ok = TRUE) {

  # Regex to check ssn, from:
  # https://www.geeksforgeeks.org/how-to-validate-ssn-social-security-number-using-regular-expression
  # ssn_regex <- "^(?!666|000|9\\d{2})\\d{3}-(?!00)\\d{2}-(?!0{4})\\d{4}$"
  ssn_regex <- "^\\d{3}-\\d{2}-\\d{4}$" # Simpler, to accommodate dummy data (with technically invalid ssn!)
  matches <- stringr::str_detect(x, ssn_regex)
  out <- if (missing_ok) {
    matches | is.na(x)
  } else {
    matches & !is.na(x)
  }
  out
}

#' Helper function for regex matching with missingness toggle
#'
#' @inheritParams is_valid_zip_code
#' @param regex regular expression string
#' @export
matches_regex <- function(x, regex, missing_ok) {
  matches <- stringr::str_detect(x, regex)
  out <- if (missing_ok) {
    matches | is_missing_chr(x)
  } else {
    matches & !is_missing_chr(x)
  }
  out
}

#' @describeIn is_valid_values zip_code
#' @export
is_valid_zip_code <- function(x, missing_ok = TRUE) {

  # Regex to check zip code, from:
  # https://regexlib.com/Search.aspx?k=us+zip+code
  zipcode_regex <- "^\\d{5}(-\\d{4})?$"
  out <- matches_regex(x, zipcode_regex, missing_ok = missing_ok)
  out
}

#' @describeIn is_valid_values 4-digit year in 2000-2099 as character string
#' @export
is_valid_year <- function(x, missing_ok = FALSE) {
  year_regex <- "^20\\d{2}$" # valid if 2000-2099
  out <- matches_regex(x, year_regex, missing_ok = missing_ok)
  out
}


#' @describeIn is_valid_values student_id, currently just a missingness check
#' @export
is_valid_student_id <- function(x) {
  !is_missing_chr(x) # & nchar(x) == 9 # TODO: verify no need to check length or anything else
}

#' @describeIn is_valid_values previous_student_id
#' @export
is_valid_previous_id <- function(x) {
  # TODO: learn what constitutes a valid previous id. For now I'm assuming it's
  # any string of all 0's, since that's what the sql indicates. Missing should
  # be OK though, right?
  is_missing_chr(x) | !stringr::str_detect(x, "^0+$")
}


#' @describeIn is_valid_values credits_earned
#' @export
is_valid_credits <- function(x, missing_ok = FALSE) {
  out <- is.numeric(x) & x >= 0 & x < 10000

  out <- if (missing_ok) {
    out | is.na(x)
  } else {
    out & !is.na(x)
  }
  out
}

#' @describeIn is_valid_values character-valued credits (min_credits and max_credits)
#' @export
is_valid_credits_chr <- function(x) {
  # Invalid if: empty string, length > 4, credits > 99.9, credits < 0, non-numeric, AND (not 0, not 0.0)
  x_num <- suppressWarnings(as.numeric(x))
  passes <- x != "" &
    nchar(x) <= 4 &
    x_num <= 99.9 &
    x_num >= 0 &
    !is.na(x_num)

  out <- passes
  out
}

#' @describeIn is_valid_values gpa
#' @export
is_valid_gpa <- function(x, missing_ok = FALSE) {
  out <- !is.na(x) & is.numeric(x) & x >= 0 & x <= 5 & !is.character(x)

  out <- if (missing_ok) {
    out | is.na(x)
  } else {
    out & !is.na(x)
  }
  out
}

#' @describeIn is_valid_values room occupancy
#' @export
is_valid_occupancy <- function(x) {
  # Checks: empty string, length > 4, value > 9999, value < 0, non-number, AND not 0
  x_num <- suppressWarnings(as.numeric(x))
  passes <- (x != "") &
    (nchar(x) <= 4) &
    (x_num <= 9999) &
    (x_num >= 0) &
    !is.na(x_num)

  out <- (x_num %in% 0) | passes # per USHE sampe sql code, zero is OK
  out
}

#' @describeIn is_valid_values course reference number
#' @export
is_valid_course_reference_number <- function(x) {
  # Invalid if: empty string, length > 5, value > 99999, value < 0, non-numeric.
  x_num <- suppressWarnings(as.numeric(x))
  passes <- nchar(x) <= 5 &
    x_num >= 0 &
    x_num <= 99999 &
    !is.na(x_num)

  out <- is_missing_chr(x) | passes # Another rule checks missingness.
}

#' @describeIn is_valid_values graduation date
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
  student_type %in% "F" # Guessing for now.
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
  undergrad_types <- c('P', 'T', 'R', 'C', 'F', 'H') # 7/27/22 slack message from Justin
  student_type %in% undergrad_types
}

#' Returns TRUE if level is one of "FR", "SO", "JR", "SR", and FALSE otherwise
#'
#' @param level character vector
#' @export
is_undergrad_level <- function(level) {
  level %in% c('JR','SR','FR','SO') # 7/27/22 slack message from Justin
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
  out <- matches_regex(x, alpha_regex, missing_ok = missing_ok)
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
  nchar(county_code) %in% 3 &
    (county_code %in% c("001", "003", "005", "007", "009", "011", "013", "015", "017",
                        "019", "021", "023", "025", "027", "029", "031", "033", "035",
                        "037", "039", "041", "043", "045", "047", "049", "051", "053",
                        "055", "057"))
}

#' @describeIn is_utah_county Checks whether a state code is in USA
#'
#' @param state first_admit_state_code
#' @export
is_us_state <- function(state) {
  us_states <- c("AA", "AE", "AK", "AL", "AP", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL",
                 "FM", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                 "MH", "MI", "MN", "MS", "MO", "MP", "MT", "NE", "NV", "NH", "NJ", "NM",
                 "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "PW", "RI", "SC", "SD", "TN",
                 "TX", "UT", "VA", "VI", "VT", "WA", "WV", "WI", "WY")
  state %in% us_states
}

#' @describeIn is_utah_county Checks whether a state code is not in USA
#'
#' @param state first_admit_state_code
#' @export
is_nonus_state <- function(state) {
  # Only return TRUE if a state code is specified (a 2-digit code) but not one of the US states
  matches_regex(state, "^[A-Z]{2}$", missing_ok = FALSE) & !is_us_state(state)
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

#' Helper functions for missing building numbers
#' @describeIn Missing Building check whether a time or day exist and if the building number is missing
#' @param building_number vector of building_number values
#' @param start_time vector of start_time values
#' @param meet_days vector of meet_day values
#' @export
is_missing_building_number <- function(building_number, start_time, meet_days) {
    case_when((is.na(start_time) | is.na(meet_days)) ~ TRUE,
            is.na(building_number) ~ FALSE,
            !is.na(building_number) ~ TRUE)
}

