


test_that("student database checks match expected outcomes in test data", {
  student_checks <- checklist %>%
    dplyr::filter(type == "Database", file == "Student")

  student_test_data <- get_test_data("student", student_checks)
  student_res <- do_checks(student_test_data, student_checks, aux_info)

  for (i in 1:nrow(student_res)) {
    row_i <- student_res[i, ]
    rule_i <- gsub("_", "", row_i$`USHE rule`, fixed = TRUE)
    eres_i <- row_i$`Expected value`

    expect_true(eres_i %in% c("pass", "fail"))

    if (eres_i == "pass") {
      expect_no_failures(row_i)
    } else if (eres_i == "fail") {
      expect_rule_fails(row_i, rule_i)
    } else {
      stop("Expected value must be either 'pass' or 'fail'")
    }
  }
})
