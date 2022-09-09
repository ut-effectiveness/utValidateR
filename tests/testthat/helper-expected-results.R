# Helpers for rule verification tests

expect_no_failures <- function(do_checks_result_row,
                               fail_values = c("Failure", "Warning")) {

  dfrow <- dplyr::select(do_checks_result_row, dplyr::ends_with("_status"))

  assertthat::assert_that(is.data.frame(dfrow),
                          nrow(dfrow) == 1L)
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(dfrow), arg = "do_checks_result_row")
  act$rules <- gsub("_status", "", names(dfrow))
  act$failures <- unlist(act$val) %in% fail_values

  # 2. Call expect()
  # Since rules can return errors outside of the actual data validation (e.g.
  # missing fields, wrong data type, etc.), we only consider a failure if the
  # rule does not error and the returned value is "Fail" or "Warning".

  expect(
    sum(act$failures) == 0,
    sprintf("Failures were encountered for rules %s in test data row %i",
            paste(act$rules[act$failures], collapse = ", "),
            do_checks_result_row$row_number)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

expect_rule_fails <- function(do_checks_result_row,
                              rule_name,
                              fail_values = c("Failure", "Warning")) {
  dfrow <- dplyr::select(do_checks_result_row, dplyr::ends_with("_status"))

  elem_name <- paste0(rule_name, "_status")
  assertthat::assert_that(is.data.frame(dfrow),
                          nrow(dfrow) == 1L)
  if (!exists(elem_name, where = dfrow)) {
    stop(sprintf("Column %s is not present in do_checks_result_row", elem_name))
  }


  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(dfrow), arg = "do_checks_result_row")
  act$rules <- gsub("_status", "", names(dfrow))
  act$failures <- unlist(act$val) %in% fail_values

  # 2. Call expect()
  # Since rules can return errors outside of the actual data validation (e.g.
  # missing fields, wrong data type, etc.), we only consider a failure if the
  # rule does not error and the returned value is "Fail" or "Warning".

  expect(
    dfrow[[elem_name]] %in% fail_values,
    sprintf("Rule %s did not fail for test data row %i; check result was:\n %s\n",
            rule_name,
            do_checks_result_row$row_number,
            act$val[[elem_name]])
  )

  # 3. Invisibly return the value
  invisible(act$val)
}



get_test_data <- function(file = "student", checklist,
                          expected_value_column = "Expected value",
                          rule_name_column = "USHE rule") {
  col_spec <- get_col_spec(file = file)
  test_data_csv <- get_test_data_csv_loc(file = file)
  testdf0 <- readr::read_csv(test_data_csv, col_types = col_spec)
  testdf0$row_number <- 1:nrow(testdf0) # original row number before removing rows in check_*()

  testdf <- testdf0 %>%
    check_expected_values(colname = expected_value_column) %>%
    check_rule_names(checklist = checklist, colname = rule_name_column)

  testdf
}

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

get_test_data_csv_loc <- function(file = "student") {
  if (file == "student") {
    # out <- "data-raw/student_unit_test.csv"
    out <- test_path("fixtures", "student_unit_test.csv")
  } else {
    stop(sprintf("file %s has no available csv location", file))
  }

  out
}


get_col_spec <- function(file = "student") {
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
