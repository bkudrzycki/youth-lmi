## code to prepare `country_list` dataset goes here

country_list <- read.csv(here("data-raw", "country_list.csv"))
usethis::use_data(country_list, overwrite = TRUE)

