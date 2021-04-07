## informal work rate

informal <- read.csv("data-raw/informality_age_sex_Bonnet.csv") %>%
  rename("ref_area.label" = Country, time = Year, "Sex: Total" = X15.24, "Sex: Male" = "X15.24..Men.", "Sex: Female" = "X15.24..Women.")
informal$ref_area.label <- informal$ref_area.label %>% ## fix country names to match ILOSTAT for joining
  recode("Congo, Democratic Republic of" = "Congo, Democratic Republic of the",
         "Czech Republic" = "Czechia",
         "Lao Peoples Democratic Republic" = "Lao People's Democratic Republic",
         "Republic of Moldova" = "Moldova, Republic of",
         "Venezuela" = "Venezuela, Bolivarian Republic of",
         "West Bank and Gaza Strip" = "Occupied Palestinian Territory",
         "Cabo Verde" = "Cape Verde")

usethis::use_data(informal, overwrite = TRUE)
