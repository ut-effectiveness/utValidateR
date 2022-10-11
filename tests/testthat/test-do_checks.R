
test_that("do_checks returns dataframe with correct structure", {

  withr::local_package("rlang")

  test_checklist <- tibble(
    rule = c("rule1", "rule2"),
    type = c("Database", "USHE"),
    activity_date = c("actdate1", NA),
    status = c("Error", "Warning"),
    checker = list(expr(var1 > var2), expr(var2 == aux_val))
  )

  test_aux <- list(aux_val = 2)

  test_df <- tibble(
    id = c("row1", "row2"),
    var1 = c(2, 1),
    var2 = c(1, 2),
    actdate1 = Sys.Date()
  )

  # Assign result of do_checks; more checks will be run on the result
  expect_silent(
    test_res <- do_checks(test_df, test_checklist, test_aux)
  )

  # No activity_date or age for rule2, since it's a USHE check and has no actdate
  expected_columns <- c(names(test_df), "rule1_status", "rule2_status",
                        "rule1_activity_date", "rule1_error_age")

  expect_s3_class(test_res, "data.frame")
  expect_named(test_res, expected_columns, ignore.order = TRUE)
  expect_equal(test_res$rule1_status, c("Pass", "Error"))
  expect_equal(test_res$rule2_status, c("Warning", "Pass"))

  # Looking for variable not present in aux_info
  expect_warning(bad_check <- do_checks(test_df, test_checklist, aux_info = list()),
                 regexp = "1 rules removed")
  expect_false(exists("rule2_status", where = bad_check))
})


test_that("error age is computed correctly for database checks", {

  withr::local_package("rlang")
  withr::local_package("lubridate")

  test_checklist <- tibble(
    rule = c("rule1", "rule2"),
    type = c("Database", "Database"),
    activity_date = c("actdate1", "actdate2"),
    status = c("Error", "Warning"),
    checker = list(expr(var1 > var2), expr(var2 == var1))
  )

  test_df <- tibble(
    id = c("row1", "row2"),
    var1 = c(2, 1),
    var2 = c(1, 2),
    actdate1 = c(today(), today() - days(11)) # actdate2 deliberately not present
  )

  # Warn that actdate2 is not present, as here rule2 is a Database check
  expect_warning(test_res <- do_checks(test_df, test_checklist, list()),
                 regexp = "actdate2")

  expect_equal(test_res$rule1_error_age, c(0, 11))
})
