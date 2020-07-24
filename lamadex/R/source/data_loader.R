neet <- read.csv("./data/raw/neet_sex_ilostat.csv")

unemployment_rate <- read.csv("./data/raw/unemployment_rate_sex_age_ilostat.csv")

employed <- read.csv("./data/raw/employed_sex_edu_ilostat.csv")

unemployed <- read.csv("./data/raw/unemployed_sex_edu_ilostat.csv")

working_pov <- read.csv("./data/raw/workingpoverty_sex_ilostat.csv")

underemp <- read.csv("./data/raw/underemployment_sex_ilostat.csv")

informal <- read.csv("./data/raw/informality_age_sex_Bonnet.csv") %>%
  rename("ref_area.label" = Country, time = Year, "Sex: Total" = X15.24, "Sex: Male" = "X15.24..Men.", "Sex: Female" = "X15.24..Women.")
informal$ref_area.label <- informal$ref_area.label %>% ## fix country names to match ILOSTAT for joining
  recode("Congo, Democratic Republic of" = "Congo, Democratic Republic of the",
         "Czech Republic" = "Czechia",
         "Lao Peoples Democratic Republic" = "Lao People's Democratic Republic",
         "Republic of Moldova" = "Moldova, Republic of",
         "Venezuela" = "Venezuela, Bolivarian Republic of",
         "West Bank and Gaza Strip" = "Occupied Palestinian Territory",
         "Cabo Verde" = "Cape Verde")

status <- read.csv("./data/raw/employment_sex_status_ilostat.csv")

occupation <- read.csv("./data/raw/occupation_sex_age_ilostat.csv")

education <- read.csv("./data/raw/education_sex_dhs.csv") %>%
  rename("ref_area.label" = Country)
education$ref_area.label <- education$ref_area.label %>%
  recode("Kyrgyz Republic" = "Kyrgyzstan",
         "Congo Democratic Republic" = "Congo, Democratic Republic of the",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Tanzania" = "Tanzania, United Republic of",
         "Moldova" = "Moldova, Republic of",
         "Vietnam" = "Viet Nam")

literacy <- read.csv("./data/raw/literacy_sex_unesco.csv") %>%
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

test_scores <- read.csv("./data/raw/test_scores_sex_wb.csv") %>%
  rename("sex.label" = Indicator.Name,
         "ref_area.label" = Economy.Name,
         "obs_value" = "X2017..YR2017.") %>%
  mutate(time = 2017)
test_scores$obs_value <- as.numeric(levels(test_scores$obs_value))[test_scores$obs_value] ## recode factors as numeric
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

## create list of all 12 indicators to serve as main input to the rank_generator function
dfList <- list(neet, unemployment_rate, employed, unemployed, working_pov, underemp, informal, status, occupation, education, literacy, test_scores)
