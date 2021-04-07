## unemployment rate

unemployment_rate <- read.csv("data-raw/unemployment_rate_sex_age_ilostat.csv")
usethis::use_data(unemployment_rate, overwrite = TRUE)
