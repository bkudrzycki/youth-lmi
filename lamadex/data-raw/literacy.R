## load literacy rates


literacy <- read.csv(here("data-raw","literacy_sex_unesco.csv")) %>%
  rename("ref_area.label" = Country,
         "obs_value" = Value,
         "time" = Time)
literacy$sex.label <- literacy$Indicator %>%
  recode("Youth literacy rate, population 15-24 years, female (%)" = "Sex: Female",
         "Youth literacy rate, population 15-24 years, male (%)" = "Sex: Male",
         "Youth literacy rate, population 15-24 years, both sexes (%)" = "Sex: Total")
literacy$ref_area.label <- literacy$ref_area.label %>%
  recode("United Republic of Tanzania" = "Tanzania, United Republic of",
         "Bolivia (Plurinational State of)" = "Bolivia",
         "China, Macao Special Administrative Region" = "Macau, China",
         "Democratic Republic of the Congo" = "Congo, Democratic Republic of the",
         "Iran (Islamic Republic of)" = "Iran, Islamic Republic of",
         "Venezuela (Bolivarian Republic of)" = "Venezuela, Bolivarian Republic of",
         "Palestine" = "Occupied Palestinian Territory",
         "Republic of Moldova" = "Moldova, Republic of",
         "Cabo Verde" = "Cape Verde")

usethis::use_data(literacy, overwrite = TRUE)
