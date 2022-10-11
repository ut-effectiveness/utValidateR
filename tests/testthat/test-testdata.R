
test_that("get_test_data returns desired dataframe", {

  data(checklist, package = "utValidateR", envir = environment())
  studentdf <- suppressWarnings(get_test_data("student"))

  expect_s3_class(studentdf, "data.frame")
  expect_gt(nrow(studentdf), 0)
})

test_that("get_test_data output is valid do_checks input", {
  data("checklist", package = "utValidateR", envir = environment())
  data("aux_info", package = "utValidateR", envir = environment())
  studentdf <- suppressWarnings(get_test_data("student"))

  student_checks <- dplyr::filter(checklist, type == "Database", file == "Student")

  check_res <- suppressWarnings(do_checks(studentdf, checklist = student_checks, aux_info = aux_info))

  expect_s3_class(check_res, "data.frame")
  expect_gt(nrow(check_res), 0)
  expect_true(any(grepl("_status$", names(check_res)))) # appended status columns
})
