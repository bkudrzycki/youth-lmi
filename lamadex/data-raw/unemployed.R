## code to prepare `unemployed` dataset goes here

unemployed <- read.csv("data-raw/unemployed_sex_edu_ilostat.csv")
usethis::use_data(unemployed, overwrite = TRUE)
