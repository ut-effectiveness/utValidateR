
<!-- README.md is generated from README.Rmd. Please edit that file -->

# utValidateR <img src="man/figures/README-ut_ie_logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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
devtools::install_github("dsu-effectiveness/utValidateR")
```

## Fake data set

`utValidateR` contains a simulated data set. This data simulates the
structure of the data pulled from the data warehouse when submitting the
Student file to USHE. Use this data set to test your validation
functions. The dataset loads automatically when you load the
`utValidateR` package.

``` r
library(DT)
View(head(fake_student_df))
```
