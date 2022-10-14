
test_that("equivalent function works", {
  expect_true(equivalent("abc", "abc"))
  expect_true(equivalent(NA, NA)) # the main difference between equivalent() and `==`
  expect_false(equivalent("abc", "ab"))
  expect_false(equivalent("abc", NA))

  # vectorization
  expect_true(all(equivalent(c("abc", NA),
                             c("abc", NA))))
})

test_that("is.Date works", {
  expect_true(is.Date(Sys.Date()))
  expect_true(is.Date(lubridate::ymd("2022-01-01")))
  expect_false(is.Date("2022-01-01"))
  expect_false(is.Date(20220101))
  expect_false(is.Date(NA))
})


test_that("age_in_range works", {
  withr::local_package("lubridate")
  year_offset <- runif(100, 0, 30)
  ref_date <- ymd("1990-01-01")
  startdates <- ref_date + days(round(year_offset * 365.25))

  age_days <- get_age(startdates) # get_age result is in days

  age_years_lwr <- floor((age_days - 2) / 365.25)
  age_years_upr <- ceiling((age_days + 2) / 365.25)

  expect_true(all(age_in_range(startdates, age_years_lwr, age_years_upr)))
  expect_false(age_in_range(as.Date(NA), -Inf, Inf)) # Missing values map to FALSE
})

test_that("in_range works", {
  xmin <- -100
  xmax <- 100
  x <- runif(100, xmin, xmax)

  expect_true(all(in_range(x, xmin, xmax)))
  expect_true(all(in_range(x, x - 0.1, x + 0.1)))
  expect_false(in_range(NA, 0, 1)) # Missing values map to FALSE
})

test_that("date_before_present_year works", {
  withr::local_package("lubridate")
  cur_year <- year(Sys.Date())
  ref_date <- ymd(paste0(cur_year, "-01-01"))

  prior_dates <- ref_date - days(c(1, round(runif(100, 0, 9999))))
  post_dates <- ref_date + days(c(0, round(runif(100, 0, 9999))))

  expect_true(all(date_before_present_year(prior_dates)))
  expect_false(any(date_before_present_year(post_dates)))

  expect_true(date_before_present_year(as.Date(NA))) # Missing values are OK (checked in a different rule)
})

test_that("is_duplicated works", {

  vec1 <- c("a",   "b",  "b",  NA,    NA,    NA)
  out1 <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  out2 <- c(FALSE, TRUE, TRUE, TRUE,  TRUE,  TRUE)

  expect_equal(is_duplicated(vec1), out1)
  expect_equal(is_duplicated(vec1, count_missing = FALSE), out1)
  expect_equal(is_duplicated(vec1, count_missing = TRUE), out2)

  # Case when x is a dataframe--look for duplicated combinations of columns
  df1 <- data.frame(v1 = vec1,
                    v2 = c(2, 2, 2, 2, NA, NA))

  out3 <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  out4 <- c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  expect_equal(is_duplicated(df1), out3)
  expect_equal(is_duplicated(df1, count_missing = FALSE), out3)
  expect_equal(is_duplicated(df1, count_missing = TRUE),  out4)
})

test_that("is_valid_act_score works", {
  input  <- c(-1,    0,    13.3, 36,   37,    NA   )
  output <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)

  expect_equal(is_valid_act_score(input), output)
})

test_that("is_valid_ssn works", {
  input <- c("333-33-3333", "123456789", "123", "",    NA   )
  out1  <- c(TRUE,          FALSE,       FALSE, FALSE, TRUE )
  out2  <- c(TRUE,          FALSE,       FALSE, FALSE, FALSE)

  expect_equal(is_valid_ssn(input), out1)
  expect_equal(is_valid_ssn(input, missing_ok = TRUE),  out1) # verify default
  expect_equal(is_valid_ssn(input, missing_ok = FALSE), out2)
})

test_that("is_valid_zip_code works", {
  input <- c("54534", "54534-9999", "00000", "abcde", "123456", "",    NA   )
  out1  <- c(TRUE,    TRUE,         TRUE,    FALSE,   FALSE,    TRUE,  TRUE )
  out2  <- c(TRUE,    TRUE,         TRUE,    FALSE,   FALSE,    FALSE, FALSE)

  expect_equal(is_valid_zip_code(input), out1)
  expect_equal(is_valid_zip_code(input, missing_ok = FALSE), out2)
  expect_equal(is_valid_zip_code(input, missing_ok = TRUE), out1)
})

test_that("is_valid_year works", {
  # Note: this function currently just checks that year is of the form "20xx"
  input <- c("1999", "2000", "20000", "2021.5", "2099", "",    NA   )
  out1  <- c(FALSE,  TRUE,    FALSE,  FALSE,    TRUE,   FALSE, FALSE)
  out2  <- c(FALSE,  TRUE,    FALSE,  FALSE,    TRUE,   TRUE,  TRUE )

  expect_equal(is_valid_year(input), out1)
  expect_equal(is_valid_year(input, missing_ok = TRUE), out2)
  expect_equal(is_valid_year(input, missing_ok = FALSE), out1)
})

test_that("is_valid_student_id works", {
  # Note: this function currently only checks for missingness (and empty strings)
  input <- c("123", "abc", "1234567890", "!@#$", "  ", "",    NA   )
  out1  <- c(TRUE,  TRUE,  TRUE,         TRUE,   TRUE, FALSE, FALSE)
  expect_equal(is_valid_student_id(input), out1)
})

test_that("is_valid_previous_id works", {
  input <- c("000", "abc", "1234567890", "!@#$", "  ", "",   NA  )
  out1  <- c(FALSE, TRUE,  TRUE,         TRUE,   TRUE, TRUE, TRUE)
  expect_equal(is_valid_previous_id(input), out1)
})

test_that("is_valid_credits works", {
  input <- c(-1,    0,    1,    100.1, 9999,  10000, NA   )
  out1  <- c(FALSE, TRUE, TRUE, TRUE,  TRUE,  FALSE, FALSE)
  out2  <- c(FALSE, TRUE, TRUE, TRUE,  TRUE,  FALSE, TRUE)
  expect_equal(is_valid_credits(input), out1)
  expect_equal(is_valid_credits(input, missing_ok = TRUE), out2)
})

test_that("is_valid_credits_chr works", {
  input <- c("0034", "12345", "abc", " ",   "000", "-1",  "",    NA)
  out1  <- c(TRUE,   FALSE,   FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE)
  expect_equal(is_valid_credits_chr(input), out1)
})


test_that("is_valid_gpa works", {
  input <- c(-1,    0,    1.23, 5.00, 5.01,  NA)
  out1  <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  expect_equal(is_valid_gpa(input), out1)
})

test_that("is_valid_occupancy works", {
  input <- c("1234", "12345", "abcd", "-1",  "0000", "0001", "",    NA)
  out1  <- c(TRUE,   FALSE,   FALSE,  FALSE, TRUE,  TRUE,   FALSE, FALSE)
  expect_equal(is_valid_occupancy(input), out1)
})

test_that("is_valid_course_reference_number works", {
  input <- c("-1", "0000", "12345", "123456", "abcde", "",   NA)
  out1  <- c(FALSE, TRUE,  TRUE,    FALSE,    FALSE,   TRUE, TRUE) # Missing values are allowed
  expect_equal(is_valid_course_reference_number(input), out1)
})

test_that("is_valid_graduation_date works", {
  skip("need to clarify this rule. SQL uses g_fis_year in a couple different ways")
})

test_that("is_valid_values works", {
  valid_vals <- c("abc", "123", "@#$%^", "")

  input <- c("abc", "abcd", "123", "@#$%^", "",   NA)
  out1  <- c(TRUE,  FALSE,  TRUE,  TRUE,    TRUE, TRUE)
  out2  <- c(TRUE,  FALSE,  TRUE,  TRUE,    TRUE, FALSE)

  expect_equal(is_valid_values(input, valid_vals), out1)
  expect_equal(is_valid_values(input, valid_vals, missing_ok = TRUE),  out1)
  expect_equal(is_valid_values(input, valid_vals, missing_ok = FALSE), out2)
})

test_that("is_freshmen_type works", {
  message("TODO: verify freshmen student_type_code")
  input <- c("F", "S",    "FF",  "FH",  "",    NA)
  out1  <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(is_freshmen_type(input), out1)
})

test_that("is_hs_type works", {
  message("TODO: verify highschool student_type_code")
  input <- c("H", "S",    "F",   "FH",  "",    NA)
  out1  <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(is_hs_type(input), out1)
})


test_that("is_undergrad_type works", {
  input <- c("P",  "T",  "R",  "C",  "F",  "H",  "p",   "FR",  "",    NA)
  out1  <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(is_undergrad_type(input), out1)
})

test_that("is_undergrad_level works", {
  input <- c("FR", "SO", "JR", "SR", "fr",  "F",   "",    NA)
  out1  <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(is_undergrad_level(input), out1)
})

test_that("is_alpha_chr works", {
  input <- c("abc", "ABC", "abc-", "123", "ab'c", "",    NA)
  out1  <- c(TRUE,  TRUE,  FALSE,  FALSE, TRUE,   TRUE,  TRUE)
  out2  <- c(TRUE,  TRUE,  FALSE,  FALSE, TRUE,   FALSE, FALSE)

  expect_equal(is_alpha_chr(input), out1)
  expect_equal(is_alpha_chr(input, missing_ok = TRUE),  out1)
  expect_equal(is_alpha_chr(input, missing_ok = FALSE), out2)
})

test_that("is_missing_chr works", {
  input <- c("abc", "123", "!@#$", " ",   "",   NA)
  out1  <- c(FALSE, FALSE, FALSE,  FALSE, TRUE, TRUE)
  expect_equal(is_missing_chr(input), out1)
})

test_that("is_utah_county works", {
  input <- c("001", "097",  "029", "035",  "100", "29",  "",     NA   )
  out1  <- c(TRUE,  FALSE,  TRUE,  TRUE,   FALSE, FALSE, FALSE , FALSE)
  expect_equal(is_utah_county(input), out1)
})

test_that("is_us_state works", {
  message("TODO: Should DC, PR be considered states?")
  input <- c("UT", "WI", "PR", "DC", "ut",   "Utah", "",    NA)
  out1  <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE,  FALSE, FALSE)
  expect_equal(is_us_state(input), out1)
})

test_that("is_nonus_state works", {
  input <- c("UT",  "SP", "PR", "DC",  "sp",  "Spain", "",    NA)
  out1  <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,   FALSE, FALSE)
  expect_equal(is_nonus_state(input), out1)
})

test_that("TODO function errors", {
  expect_error(TODO())
  expect_error(TODO("abcd"))
})
