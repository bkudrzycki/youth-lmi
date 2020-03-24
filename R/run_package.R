library(tidyverse)
library(roxygen2)
rm(list=ls())

setwd("~/polybox/Youth Employment/1b Index/youth-lmi")

library(ylmi.package, lib("./ylmipackage/ylmi.package"))

## initialize index with country names to be included in the index
country_list <- read.csv("./data/country_codes.csv") %>%
  select(ref_area.label = Country, country_code = "Alpha.3.code") %>%
  mutate(country_code = substr(country_code, 2, 4)) #remove space in front of country code

## fix country names to match ILOSTAT for later joins
country_list$ref_area.label <- country_list$ref_area.label %>%
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
country_list <- country_list %>%
  add_row(ref_area.label = "Kosovo", country_code = "XKX") %>%
  add_row(ref_area.label = "Curaçao", country_code = "CUW") %>%
  add_row(ref_area.label = "Eswatini", country_code = "SWZ") %>%
  add_row(ref_area.label = "Macau, China", country_code = "MAC") %>%
  add_row(ref_area.label = "South Sudan", country_code = "SSD")

## read in dataframes from raw csv files

neet <- read.csv("./data/raw/neet_sex_ilostat.csv")

relative_unemp <- read.csv("./data/raw/unemployment_sex_age_ilostat.csv") %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>%
  mutate(obs_value = `Age (Youth, adults): 15-24`/`Age (Youth, adults): 25+`)  ## calculate relative unemployment rate

## use mismatch function to calculate mismatch rate
employed <- read.csv("./data/raw/employment_sex_edu_ilostat.csv")
unemployed <- read.csv("./data/raw/unemployment_edu_ilostat.csv")
mismatch <- mismatch(employed, unemployed)
rm(unemployed)

workingpov <- read.csv("./data/raw/workingpoverty_sex_ilostat.csv")

## calculate underemployed rate
total_employed <- employed %>%
  filter(classif1.label == "Age (Aggregate bands): 15-24", classif2.label == "Education (ISCED-11): Total") ## total youth employed
underemp <- read.csv("./data/raw/underemployment_sex_ilostat.csv")
underemp <- full_join(underemp, total_employed, by=c("ref_area.label", "time", "sex.label")) %>%
  mutate(obs_value = 100 * obs_value.x / obs_value.y) ## number of youth underemployed divided by total number of youth employed
rm(employed, tot_employed)

## read in and reformat informality rate
informal <- read.csv("./data/raw/informality_age_sex_Bonnet.csv") %>%
  rename("ref_area.label" = Country, time = Year, "Sex: Total" = X15.24, "Sex: Male" = "X15.24..Men.", "Sex: Female" = "X15.24..Women.") %>%
  pivot_longer(cols = c("Sex: Total","Sex: Male","Sex: Female"),
               names_to = "sex.label",
               values_to = "obs_value",
               names_ptypes = list(sex.label = factor(levels = c("Sex: Total","Sex: Male","Sex: Female")))) ## pivot to longer form

informal$ref_area.label <- informal$ref_area.label %>% ## fix country names to match ILOSTAT for joining
  recode("Congo, Democratic Republic of" = "Congo, Democratic Republic of the",
         "Czech Republic" = "Czechia",
         "Lao Peoples Democratic Republic" = "Lao People's Democratic Republic",
         "Republic of Moldova" = "Moldova, Republic of",
         "Venezuela" = "Venezuela, Bolivarian Republic of")

## calculate vulnerable rate
vulnerable <- read.csv("./data/raw/employment_sex_age_status_ilostat.csv") %>%  ## read in employment by sex, age, and status in work data
  pivot_wider(names_from = classif2.label, values_from = obs_value) %>%
  filter(classif1.label == "Age (Aggregate bands): 15-24", obs_status.label != "Unreliable") %>%
  mutate(obs_value = 100 * rowSums(.[14:15]) / .$"Status in employment (ICSE-93): Total") ## divide own account/family by total working

elementary <- read.csv("./data/raw/occupation_sex_age_ilostat.csv") %>%
  filter(classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24"),
         classif2.label %in% c("Occupation (ISCO-08): 9. Elementary occupations",
                               "Occupation (ISCO-08): Total"),
         obs_status.label != "Unreliable") %>%
  pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>%
  mutate(obs_value = 100 * rowSums(.[c(11,13)]) / rowSums(.[c(10,12)]))

## SAFF: rate of youth in working in skilled agriculture, fishery or forestry
saff <- read.csv("./data/raw/occupation_sex_age_ilostat.csv") %>%
  filter(classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24"),
         classif2.label %in% c("Occupation (ISCO-08): 6. Skilled agricultural, forestry and fishery workers",
                               "Occupation (ISCO-08): Total"),
         obs_status.label != "Unreliable") %>%
  pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>%
  mutate(obs_value = 100 * rowSums(.[c(11,13)]) / rowSums(.[c(10,12)]))

nosecondary <- read.csv("./data/raw/education_sex_dhs.csv") %>%
  mutate("Sex: Female" = rowSums(.[c(3:5)]),
         "Sex: Male" = rowSums(.[c(12:14)]),
         time = substr(Survey, 0, 4)) %>%
  mutate("Sex: Total" = rowMeans(.[c(22:23)])) %>% ## Total is naively calculated as the mean of male and female (see paper)
  rename("ref_area.label" = Country) %>%
  pivot_longer(cols = c("Sex: Total","Sex: Male","Sex: Female"),
               names_to = "sex.label",
               values_to = "obs_value",
               names_ptypes = list(sex.label = factor(levels = c("Sex: Total","Sex: Male","Sex: Female")))) ## pivot to longer form

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

dfList <- list("neet"=neet, "relative_unemp"=relative_unemp, "mismatch"=mismatch, "workingpov"=workingpov, "underemp"=underemp, "informal"=informal, "vulnerable"=vulnerable, "elementary"=elementary, "saff"=saff, "nosecondary"=nosecondary, "literacy"=literacy, "test_scores"=test_scores)

rank <- rank_generator(dfList, country_list, bygender = FALSE, lastyear = 2009)
rm(dfList)

