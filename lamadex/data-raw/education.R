## load educational attainment rates

education <- read.csv("data-raw/education_sex_dhs.csv") %>%
  rename("ref_area.label" = Country)
education$ref_area.label <- education$ref_area.label %>%
  recode("Kyrgyz Republic" = "Kyrgyzstan",
         "Congo Democratic Republic" = "Congo, Democratic Republic of the",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Tanzania" = "Tanzania, United Republic of",
         "Moldova" = "Moldova, Republic of",
         "Vietnam" = "Viet Nam")

usethis::use_data(education, overwrite = TRUE)
