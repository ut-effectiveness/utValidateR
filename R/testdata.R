

#' Returns a dataframe containing info needed for `do_checks()` plus expected outcome for specified rules
#'
#' This data is specified by UT Tech, and located in inst/testdata/
#'
#' @param file which file to get. Currently only "student" is available
#' @export
get_test_data <- function(file = c("student")) {

  file <- match.arg(file)

  checklist <- get_checklist(file, "database")

  # Unclear whether these will change in future
  expected_value_column = "Expected value"  # csv column with expected outcome
  rule_name_column = "USHE rule"  # csv column with rule name

  col_spec <- get_col_spec(file = file)
  test_data_csv <- get_test_data_csv_loc(file = file)
  testdf0 <- readr::read_csv(test_data_csv, col_types = col_spec)
  testdf0$row_number <- 1:nrow(testdf0) # original row number before removing rows in check_*()

  testdf <- testdf0 %>%
    check_expected_values(colname = expected_value_column) %>%
    check_rule_names(checklist = checklist, colname = rule_name_column)

  testdf
}

#' QC-check on expected values supplied in testdata dataframe (from csv)
#'
#' Removes rows with a warning when column values are not in `valid_values`
#'
#' @param df dataframe to check
#' @param colname Column name containing expected values
#' @param valid_values which values should be considered valid?
check_expected_values <- function(df, colname = "Expected value",
                                  valid_values = c("pass", "fail")) {
  stopifnot(exists(colname, where = df))

  validvals <- df[[colname]] %in% valid_values

  if (sum(!validvals) > 0) {
    warning("The following rows of test data have bad expected values and ",
            "were removed: ",
            paste(df$row_number[!validvals], collapse = ", "),
            "\n")
  }
  df[validvals, ]
}

#' QC-check on rule names--these should all match rule names in checklist
#'
#' Rows with non-matching names are removed with a warning
#'
#' @param df dataframe to check
#' @param checklist dataframe containing rule specification
#' @param colname Column name for `df` containing rule names
check_rule_names <- function(df, checklist, colname = "USHE rule") {
  stopifnot(exists(colname, where = df))

  df[[colname]] <- gsub("_", "", df[[colname]], fixed = TRUE) # Supplied with underscores but don't want them

  rules <- unique(checklist$rule)
  validrules <- df[[colname]] %in% c(NA_character_, "", rules) # Empty/missing for "pass" rows

  if (sum(!validrules) > 0) {
    warning("The following rows of test data have bad rule names and ",
            "were removed: ",
            paste(df$row_number[!validrules], collapse = ", "),
            "\n")
  }
  df[validrules, ]
}

#' Returns the location of the csv containing test data (with expected outcomes) for the given file
#'
#' @param file e.g. "student" (actually that's the only currently valid value)
get_test_data_csv_loc <- function(file = c("student")) {

  file <- match.arg(file)

  if (file == "student") {
    out <- system.file("testdata", "student_unit_test.csv", package = "utValidateR")
  } else {
    stop(sprintf("file %s has no available csv location", file))
  }

  out
}

#' Returns the spec (output of `readr::cols()`) for the specified file
#'
#' @inheritParams get_test_data_csv_loc
get_col_spec <- function(file = c("student")) {
  if (file == "student") {
    out <- readr::cols(
      ssn = "c",
      ssid = "c",
      first_name = "c",
      last_name = "c",
      middle_name = "c",
      previous_last_name = "c",
      name_suffix = "c",
      previous_first_name = "c",
      preferred_first_name = "c",
      preferred_middle_name = "c",
      local_address_zip_code = "c",
      mailing_address_zip_code = "c",
      us_citizenship_code = "c",
      first_admit_county_code = "c",
      first_admit_state_code = "c",
      first_admit_country_code = "c",
      residential_housing_code = "c",
      student_id = "c",
      previous_student_id = "c",
      birth_date = "D",
      gender_code = "c",
      is_hispanic_latino_ethnicity = "l",
      is_asian = "l",
      is_black = "l",
      is_american_indian_alaskan = "l",
      is_hawaiian_pacific_islander = "l",
      is_white = "l",
      is_international = "l",
      is_other_race = "l",
      primary_major_cip_code = "c",
      student_type_code = "c",
      primary_level_class_id = "c",
      primary_degree_id = "c",
      institutional_cumulative_credits_earned = "d",
      institutional_cumulative_gpa = "d",
      full_time_part_time_code = "c",
      transfer_cumulative_credits_earned = "d",
      secondary_major_cip_code = "c",
      act_composite_score = "d",
      act_english_score = "d",
      act_math_score = "d",
      act_reading_score = "d",
      act_science_score = "d",
      high_school_graduation_date = "D",
      is_pell_eligible = "l",
      is_pell_awarded = "l",
      is_bia = "l",
      primary_major_college_id = "c",
      primary_major_college_desc = "c",
      secondary_major_college_id = "c",
      secondary_major_college_desc = "c",
      level_id = "c",
      term_id = "c",
      sgbstdn_activity_date = "D",
      spriden_activity_date = "D",
      sabsupl_activity_date = "D",
      ipeds_award_level = "c",
      primary_major_desc = "c",
      secondary_major_desc = "c"
    )
  } else {
    stop(sprintf("file %s has no available spec", file))
  }

  out
}
