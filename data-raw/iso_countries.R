## code to prepare `iso_countries` dataset goes here

# Comes from table in https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
iso_countries <- read.csv("sandbox/iso-countries.csv")
usethis::use_data(iso_countries, overwrite = TRUE, internal = TRUE)
