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


