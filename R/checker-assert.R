# Assertion functions for checkers


#' Assertion for non-missing values
#'
#' @param vec vector to check
#' @param expr_chr Character representation of generating expression, for messaging
#' @importFrom assertthat assert_that noNA
assert_no_missing <- function(vec) {
  msg <- sprintf("Missing values found when evaluating rule")
  assert_that(noNA(vec), msg = msg)
}

#' Assertion for logical type
#'
#' @inheritParams assert_no_missing
assert_logical <- function(vec) {
  msg <- sprintf("Non-logical vector created when evaluating rule")
  assert_that(is.logical(vec), msg = msg)
}

#' Assertion for correct vector length
#'
#' @param df dataframe being checked
#' @inheritParams assert_no_missing
assert_length <- function(vec, df) {
  msg <- sprintf("Wrong-length vector created when evaluating rule")
  assert_that(length(vec) == nrow(df), msg = msg)
}
