## code to prepare `employed` dataset goes here

employed <- read.csv(here("data-raw","employed_sex_edu_ilostat.csv"))
usethis::use_data(employed, overwrite = TRUE)
