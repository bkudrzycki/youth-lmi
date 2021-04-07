# neet rate

neet <- read.csv(here("data-raw","neet_sex_ilostat.csv"))
usethis::use_data(neet, overwrite = TRUE)
