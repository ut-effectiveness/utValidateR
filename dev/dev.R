# This file should be used to keep track of your development throughout the project.


# Add Dev Package Dependency ####
# install.packages("devtools")
devtools::install_github("dsu-effectiveness/utValidateR")

usethis::use_dev_package("utValidateR")



## Student #####
# Run this code to create the folder in data-raw that houses the code to generate
# fake data set.
usethis::use_data_raw(name = "fake_student_df", open = FALSE)

usethis::use_data_raw(name = 'fake_student_course_validation', open = FALSE)

usethis::use_data_raw(name = 'fake_graduation_validation', open = FALSE)

usethis::use_data_raw(name = 'fake_buildings_validation', open = FALSE)

