
#' Returns number of days since each date in x
#'
#' @param x a Date vector or similar
#' @export
get_age <- function(x) {
  lubridate::interval(x, Sys.Date()) %/% lubridate::days(x=1)
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
ssn_regex <- "^(?!666|000|9\\d{2})\\d{3}-(?!00)\\d{2}-(?!0{4})\\d{4}$"

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
