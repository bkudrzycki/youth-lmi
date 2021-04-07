## code to prepare `occupation` dataset goes here

occupation <- read.csv("data-raw/occupation_sex_age_ilostat.csv")
usethis::use_data(occupation, overwrite = TRUE)
