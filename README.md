
<!-- README.md is generated from README.Rmd. Please edit that file -->

# utValidateR <img src="man/figures/README-ut_ie_logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/dsu-effectiveness/utValidateR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dsu-effectiveness/utValidateR?branch=master)
<!-- badges: end -->

The goal of utValidateR is to validate elements within the warehouse ETL
pipeline, or elements in the reporting layer. The functions in this
library are used in USHE and IPEDS reporting. The functions also for the
basis for the campus data audits.

## Installation

You can install the development version of utValidateR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dsu-effectiveness/utValidateR", ref = "develop")
```

## Contributing

Changes to the R code for performing each rule check should be made in
data-raw/checklist.R, in the `rule_spec` object. Functions used by these
checks are defined in R/checker-helpers.R. Data objects referenced in
the checker functions are provided in `aux_info`, which can be modified
in data-raw/aux_info.R.

See `vignette("development", package = "utValidateR")` (or view its
source in vignettes/development.Rmd) for more information on development
workflows and needs.

Specific questions or requests can be submitted as github issues.

## Using

The following is an example of applying `do_checks()` to the supplied
`fake_student_df` dataset. Further examples and orientation are provided
in vignettes/database-checks.Rmd, which (if installed with
`build_vignettes = TRUE`) can be viewed via
`vignette("database-checks")`.

``` r
library(utValidateR)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data("fake_student_df")
data("checklist")
data("aux_info")

student_checks <- get_checklist(file = "student", type = "database") %>% 
  glimpse()
#> Rows: 70
#> Columns: 9
#> $ rule          <chr> "S00a", "S00b", "S03a", "S03c", "S04b", "S05a", "S06a", …
#> $ ref_rule      <chr> "S00a", "S00b", "S03a", "S03c", "S04b", "S05a", "S06a", …
#> $ description   <chr> "duplicate student_id's", "duplicate ssn", "missing stud…
#> $ status        <chr> "Failure", "Failure", "Failure", "Failure", "Failure", "…
#> $ type          <chr> "Database", "Database", "Database", "Database", "Databas…
#> $ activity_date <chr> "spriden_activity_date", "spbpers_activity_date", "spbpe…
#> $ banner        <chr> "banner.spriden_id", "banner.spbpers_ssn", "banner.spbpe…
#> $ checker       <list> <!is_duplicated(cbind(student_id, term_id))>, <!is_dupl…
#> $ file          <chr> "Student", "Student", "Student", "Student", "Student", "…

student_check_result <- suppressMessages(suppressWarnings( # suppressing rather verbose error descriptions
  do_checks(df_tocheck = fake_student_df, 
            checklist = student_checks, 
            aux_info = aux_info)
))

glimpse(student_check_result)
#> Rows: 10,000
#> Columns: 159
#> $ ssn                                     <chr> "991-59-3442", "066-26-2729", …
#> $ first_name                              <chr> "Justine", "Bryant", "Cher", "…
#> $ last_name                               <chr> "Daniel", "Pagac", "Hauck", "G…
#> $ middle_name                             <chr> NA, NA, NA, NA, "Bob", NA, NA,…
#> $ previous_last_name                      <chr> NA, NA, "Jones", "Jones", "Jon…
#> $ previous_first_name                     <chr> NA, NA, NA, NA, NA, "Danny", N…
#> $ preferred_first_name                    <chr> "Joe", NA, "Joe", "Deb", "Joe"…
#> $ preferred_middle_name                   <chr> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ local_address_zip_code                  <chr> "13979-8912", "96921-0688", "8…
#> $ mailing_address_zip_code                <chr> "13074-7031", "37697-2663", "6…
#> $ us_citizenship_code                     <chr> "3", "9", NA, "4", "4", "4", "…
#> $ first_admit_county_code                 <chr> "11", "53", "15", "39", "49", …
#> $ first_admit_state_code                  <chr> "UT", "UT", "UT", "UT", "UT", …
#> $ first_admit_country_code                <chr> "US", "US", "US", "CS", "US", …
#> $ residential_housing_code                <chr> "M", "0", "S", "R", "M", "N", …
#> $ student_id                              <chr> "00712652", "00550034", "00259…
#> $ previous_student_id                     <lgl> NA, NA, NA, NA, NA, NA, NA, NA…
#> $ birth_date                              <date> 2003-02-04, 2018-08-27, 1978-…
#> $ gender_code                             <chr> "F", "M", "F", "F", "M", "M", …
#> $ is_hispanic_latino_ethnicity            <lgl> TRUE, TRUE, FALSE, FALSE, FALS…
#> $ is_asian                                <lgl> TRUE, FALSE, FALSE, TRUE, TRUE…
#> $ is_black                                <lgl> FALSE, TRUE, TRUE, FALSE, FALS…
#> $ is_american_indian_alaskan              <lgl> FALSE, TRUE, FALSE, TRUE, TRUE…
#> $ is_hawaiian_pacific_islander            <lgl> TRUE, FALSE, TRUE, TRUE, FALSE…
#> $ is_white                                <lgl> TRUE, FALSE, TRUE, FALSE, TRUE…
#> $ is_international                        <lgl> TRUE, TRUE, FALSE, FALSE, FALS…
#> $ is_other_race                           <lgl> FALSE, FALSE, FALSE, TRUE, TRU…
#> $ primary_major_cip_code                  <chr> "93947", "42064", "43531", "53…
#> $ student_type_code                       <chr> "5", "0", "0", "5", "C", "1", …
#> $ primary_level_class_id                  <chr> "SR", "SR", "JR", NA, "FR", "S…
#> $ primary_degree_id                       <chr> "BM", "BSN", NA, "BIS", "BS", …
#> $ institutional_cumulative_credits_earned <dbl> 84.0, 25.5, 109.0, 83.0, 115.3…
#> $ institutional_cumulative_gpa            <dbl> 2.88, 3.35, 2.41, 2.85, 2.54, …
#> $ full_time_part_time_code                <chr> "F", "F", "F", "P", "F", "F", …
#> $ transfer_cumulative_credits_earned      <dbl> 45.0, 159.0, NA, 185.0, 119.0,…
#> $ secondary_major_cip_code                <chr> "99407", "35353", "08267", "09…
#> $ act_composite_score                     <int> 21, 36, 7, 30, 32, 33, 16, 1, …
#> $ act_english_score                       <int> 22, 17, 24, 24, 4, 28, 36, 16,…
#> $ act_math_score                          <int> 14, 23, 10, 12, 24, 32, 30, 22…
#> $ act_reading_score                       <int> 4, 8, 32, 28, 2, 6, 33, 12, 19…
#> $ act_science_score                       <int> 35, 14, 1, 19, 12, 14, 11, 34,…
#> $ high_school_graduation_date             <date> 2015-12-05, 2011-10-17, 1991-…
#> $ is_pell_eligible                        <lgl> TRUE, NA, FALSE, TRUE, FALSE, …
#> $ is_pell_awarded                         <lgl> TRUE, FALSE, TRUE, FALSE, TRUE…
#> $ is_bia                                  <lgl> TRUE, TRUE, FALSE, TRUE, FALSE…
#> $ primary_major_college_id                <chr> "HS", "HO", "MA", "HO", "NU", …
#> $ primary_major_college_desc              <chr> "Coll of Sci, Engr & Tech", "C…
#> $ secondary_major_college_id              <chr> "HI", "ED", "EF", "ED", "FA", …
#> $ secondary_major_college_desc            <chr> "Coll of Sci, Engr & Tech", "M…
#> $ level_id                                <chr> "00", "GR", "UG", "NC", "UG", …
#> $ term_id                                 <dbl> 201420, 201440, 201440, 202020…
#> $ sgbstdn_activity_date                   <date> 2020-12-24, 2021-06-28, 2020-…
#> $ spriden_activity_date                   <date> 2020-08-15, 2021-03-25, 2021-…
#> $ sabsupl_activity_date                   <date> 2018-09-27, 2018-10-02, 2020-…
#> $ S00a_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S00a_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S00a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S00b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S03a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S03c_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S04b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S05a_status                             <chr> "`is_valid_previous_id(previou…
#> $ S06a_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S06a_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S06a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S06b_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S06b_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S06b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S06c_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S06c_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S06c_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S06d_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S06d_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S06d_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S06e_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S06e_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S06e_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S07a_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S07a_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S07a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S07b_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S07b_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S07b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S08a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S08b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S09a_status                             <chr> "Pass", "Pass", "Failure", "Pa…
#> $ S09b_status                             <chr> "Failure", "Failure", "Failure…
#> $ S10a_activity_date                      <date> 2018-09-27, 2018-10-02, 2020-…
#> $ S10a_error_age                          <dbl> 1461, 1456, 903, 560, 858, 150…
#> $ S10a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S11a_activity_date                      <date> 2018-09-27, 2018-10-02, 2020-…
#> $ S11a_error_age                          <dbl> 1461, 1456, 903, 560, 858, 150…
#> $ S11a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S12_status                              <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S13_status                              <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S16a_status                             <chr> "Failure", "Failure", "Failure…
#> $ S17c_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S17c_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S17c_status                             <chr> "`TODO(\"How to compare to pre…
#> $ S17d_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S17d_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S17d_status                             <chr> "`TODO(\"same question as S17c…
#> $ S18a_status                             <chr> "Pass", "Pass", "Pass", "Failu…
#> $ S19a_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S19a_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S19a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S20a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S21_status                              <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S21a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S22a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S23c_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S24a_status                             <chr> "Pass", "Pass", "Failure", "Pa…
#> $ S25a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S26b_status                             <chr> "`TODO(\"Not relevant to compa…
#> $ S27a_activity_date                      <date> 2018-09-27, 2018-10-02, 2020-…
#> $ S27a_error_age                          <dbl> 1461, 1456, 903, 560, 858, 150…
#> $ S27a_status                             <chr> "Pass", "Pass", "Pass", "Failu…
#> $ S27b_activity_date                      <date> 2018-09-27, 2018-10-02, 2020-…
#> $ S27b_error_age                          <dbl> 1461, 1456, 903, 560, 858, 150…
#> $ S27b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S27c_activity_date                      <date> 2018-09-27, 2018-10-02, 2020-…
#> $ S27c_error_age                          <dbl> 1461, 1456, 903, 560, 858, 150…
#> $ S27c_status                             <chr> "Pass", "Pass", "Pass", "Failu…
#> $ S30a_status...124                       <chr> "Failure", "Failure", "Failure…
#> $ S30a_status...125                       <chr> "Failure", "Failure", "Failure…
#> $ S34a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S34b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S34c_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S34e_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S35a_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S35a_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S35a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S35b_activity_date                      <date> 2020-08-15, 2021-03-25, 2021-…
#> $ S35b_error_age                          <dbl> 773, 551, 490, 1538, 642, 388,…
#> $ S35b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S36a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S37a_status                             <chr> "Failure", "Failure", "Failure…
#> $ S38a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S39a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S40a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S41a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S42a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S43b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S44c_status                             <chr> "Pass", "Pass", "Failure", "Pa…
#> $ S46a_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S46a_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S46a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S46b_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S46b_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S46b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S47a_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S47a_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S47a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S47b_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S47b_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S47b_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
#> $ S48a_activity_date                      <date> 2020-12-24, 2021-06-28, 2020-…
#> $ S48a_error_age                          <dbl> 642, 456, 732, 1364, 498, 1134…
#> $ S48a_status                             <chr> "Pass", "Pass", "Pass", "Pass"…
```
