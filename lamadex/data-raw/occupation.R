## code to prepare `occupation` dataset goes here

occupation <- read.csv(here("data-raw","occupation_sex_age_ilostat.csv"))
usethis::use_data(occupation, overwrite = TRUE)
