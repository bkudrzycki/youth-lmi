library(tidyverse)
rm(list=ls())

setwd("~/polybox/Youth Employment/1b Index/youth-lmi")

library(ylmi.package, lib("./ylmipackage/ylmi.package"))

## initialize index with country names to be included in the index

index <- read.csv("./data/country_codes.csv") %>%
  select(country = Country, country_code = "Alpha.3.code") %>%
  mutate(country_code = substr(country_code, 2, 4)) #remove space in front of country code

## fix country names to match ILOSTAT for later joins
index$country <- index$country %>%
  recode("Virgin Islands, British" = "British Virgin Islands",
         "Congo, the Democratic Republic of the" = "Congo, Democratic Republic of the",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Faroe Islands" = "Faeroe Islands",
         "Hong Kong" = "Hong Kong, China",
         "Kyrgyz Republic" = "Kyrgyzstan",
         "Macao, China" = "Macau, China",
         "Macedonia, the former Yugoslav Republic of" = "North Macedonia",
         "Palestinian Territory, Occupied" = "Occupied Palestinian Territory",
         "Taiwan" = "Taiwan, China",
         "Tanzania" = "Tanzania, United Republic of",
         "Czech Republic" = "Czechia")

## add missing countries
index <- index %>%
  add_row(country = "Kosovo", country_code = "XKX") %>%
  add_row(country = "Curaçao", country_code = "CUW") %>%
  add_row(country = "Eswatini", country_code = "SWZ") %>%
  add_row(country = "Macau, China", country_code = "MAC") %>%
  add_row(country = "South Sudan", country_code = "SSD")

## read in neet rate data
neet <- read.csv("./data/raw/neet_sex_ilostat.csv")

## read in relative unemployment rate data
relative_unemp <- read.csv("./data/raw/unemployment_sex_age_ilostat.csv")

## read in employment and unemployment data
empxeduc <- read.csv("./data/raw/employment_edu_ilostat.csv")
uexeduc <- read.csv("./data/raw/unemployment_edu_ilostat.csv")

## calculate transition dimension using "transDim" function
index <- transDim(index, neet, relative_unemp, empxeduc, uexeduc, bygender = FALSE, lastyear = 2009)

## read in working poverty rate
workingpov <- read.csv("./data/raw/workingpoverty_sex_ilostat.csv")

## time-related underemployment
underemp <- read.csv("./data/raw/underemployment_sex_ilostat.csv")
employment <- empxeduc

## youth informality rate
informal <- read.csv("./data/raw/informality_age_sex_Bonnet.csv") %>%
  rename("ref_area.label" = Country, time = Year, "Sex: Total" = X15.24, "Sex: Male" = "X15.24..Men.", "Sex: Female" = "X15.24..Women.") %>%
  pivot_longer(cols = c("Sex: Total","Sex: Male","Sex: Female"), names_to = "sex.label", values_to = "obs_value") ## pivot to longer form

informal$ref_area.label <- informal$ref_area.label %>% ## fix country names to match ILOSTAT for joining
  recode("Congo, Democratic Republic of" = "Congo, Democratic Republic of the",
         "Czech Republic" = "Czechia",
         "Lao Peoples Democratic Republic" = "Lao People's Democratic Republic",
         "Republic of Moldova" = "Moldova, Republic of",
         "Venezuela" = "Venezuela, Bolivarian Republic of")

employment <- read.csv("./data/raw/employment_sex_age_status_ilostat.csv")

occupations <- read.csv("./data/raw/occupation_sex_age_ilostat.csv")

## calculate working conditions dimension using "workcondDim" function
index <- workcondDim(index, workingpov, underemp, employment, informal, occupations)

nosecondary <- read.csv("./data/raw/education_sex_dhs.csv") %>%
  mutate("Sex: Female" = rowSums(.[c(3:5)]),
         "Sex: Male" = rowSums(.[c(12:14)]),
         time = substr(Survey, 0, 4)) %>%
  mutate("Sex: Total" = rowMeans(.[c(22:23)])) %>% ## Total is naively calculated as the mean of male and female (see paper)
  rename("ref_area.label" = Country) %>%
  pivot_longer(cols = c("Sex: Total","Sex: Male","Sex: Female"), names_to = "sex.label", values_to = "obs_value") ## pivot to longer form

nosecondary$ref_area.label <- nosecondary$ref_area.label %>%
  recode("Kyrgyz Republic" = "Kyrgyzstan",
         "Congo Democratic Republic" = "Congo, Democratic Republic of the",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Tanzania" = "Tanzania, United Republic of")

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
       "Republic of Moldova" = "Moldova, Republic of")

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

index <- educDim(nosecondary, literacy, test_scores, bygender = FALSE, lastyear = 2009)

## generate overall index score

rank <- index %>%
  mutate(final_score = ifelse(rowSums(is.na(.))<3, rowMeans(.[c(6,13,17)], na.rm = TRUE),NA)) %>%
  filter(!is.na(final_score)) %>%
  select(country, country_code, final_score)

