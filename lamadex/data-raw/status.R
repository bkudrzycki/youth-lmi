## code to prepare `status` dataset goes here

status <- read.csv("data-raw/employment_sex_status_ilostat.csv")
usethis::use_data(status, overwrite = TRUE)
