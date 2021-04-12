## load test scores

test_scores <- read.csv("data-raw/test_scores_sex_wb.csv") %>%
  pivot_longer(cols = c(5:length(.)), names_to = "time", names_prefix = "X", values_to = "obs_value") %>%
  rename("sex.label" = Series.Name,
         "ref_area.label" = Economy.Name) %>%
  mutate(time = as.numeric(substring(time, 1, 4)),
         obs_value = as.numeric(na_if(obs_value, "..")))

test_scores$sex.label <- test_scores$sex.label %>%
  recode("Harmonized Test Scores, Female" = "Sex: Female",
         "Harmonized Test Scores, Male" = "Sex: Male",
         "Harmonized Test Scores" = "Sex: Total")
test_scores$ref_area.label <- test_scores$ref_area.label %>%
  recode("Congo, Dem. Rep." = "Congo, Democratic Republic of the",
         "Yemen, Rep." = "Yemen",
         "Gambia, The" = "Gambia",
         "Egypt, Arab Rep." = "Egypt",
         "Lao PDR" = "Lao People's Democratic Republic",
         "Congo, Rep." = "Congo",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Tanzania" = "Tanzania, United Republic of",
         "Kyrgyz Republic" = "Kyrgyzstan",
         "Iran, Islamic Rep." = "Iran, Islamic Republic of",
         "Moldova" = "Moldova, Republic of",
         "Slovak Republic" = "Slovakia",
         "Vietnam" = "Viet Nam",
         "Czech Republic" = "Czechia",
         "Macao SAR, China" = "Macau, China",
         "Hong Kong SAR, China" = "Hong Kong, China",
         "Korea, Rep." = "Korea, Republic of",
         "Macedonia, FYR" = "North Macedonia",
         "West Bank and Gaza" = "Occupied Palestinian Territory")

usethis::use_data(test_scores, overwrite = TRUE)
