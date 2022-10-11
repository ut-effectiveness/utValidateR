
test_that("default get_checklist returns full cheklist", {
  data("checklist", package = "utValidateR", envir = environment())
  full_checklist <- get_checklist()

  expect_equal(checklist, full_checklist)
})

test_that("get_checklist args filter the full checklist", {
  cl_full <- get_checklist()
  cl_s <- get_checklist(file = "student")
  cl_c <- get_checklist(file = "course")

  expect_gt(nrow(cl_s), 0)
  expect_gt(nrow(cl_c), 0)

  expect_lt(nrow(cl_s), nrow(cl_full))
  expect_lt(nrow(cl_c), nrow(cl_full))

  # filter by both file and type
  cl_sd <- get_checklist(file = "student", type = "database")
  cl_cu <- get_checklist(file = "course", type = "ushe")

  expect_lt(nrow(cl_sd), nrow(cl_s))
  expect_lt(nrow(cl_cu), nrow(cl_c))

})
