library(tidyverse)

rm(list=ls())

setwd("~/polybox/Youth Employment/1b Index/youth-lmi")

library(ylmi.package, lib("./ylmipackage/ylmi.package"))

## read in country lists according to the World Bank Lending Groups classification
source("./ylmi.package/R/source/countryList.R")

## read in dataframes from raw csv files
neet <- read.csv("./data/raw/neet_sex_ilostat.csv") %>%
  filter(obs_status.label != "Unreliable")

relative_unemp <- read.csv("./data/raw/unemployment_sex_age_ilostat.csv") %>%
  filter(obs_status.label != "Unreliable") %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>%
  mutate(obs_value = `Age (Youth, adults): 15-24`/`Age (Youth, adults): 25+`)  ## calculate relative unemployment rate

## use mismatch function to calculate mismatch rate
employed <- read.csv("./data/raw/employment_sex_edu_ilostat.csv") %>%
  filter(obs_status.label != "Unreliable")
unemployed <- read.csv("./data/raw/unemployment_sex_edu_ilostat.csv") %>%
  filter(obs_status.label != "Unreliable")
mismatch <- mismatch(employed, unemployed)

workingpov <- read.csv("./data/raw/workingpoverty_sex_ilostat.csv")

## calculate underemployed rate
underemp <- read.csv("./data/raw/underemployment_sex_ilostat.csv") %>%
  filter(obs_status.label != "Unreliable")
total_emp <- employed %>%
  filter(classif1.label == "Age (Aggregate bands): 15-24", classif2.label == "Education (Aggregate levels): Total")
underemp <- full_join(underemp, total_emp, by=c("ref_area.label", "time", "sex.label")) %>%
  mutate(obs_value = 100 * obs_value.x / obs_value.y) ## number of youth underemployed divided by total number of youth employed

## read in and format informality rate
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
  filter(obs_status.label != "Unreliable") %>%
  pivot_wider(names_from = classif2.label, values_from = obs_value) %>%
  filter(classif1.label == "Age (Aggregate bands): 15-24", obs_status.label != "Unreliable") %>%
  mutate(obs_value = 100 * rowSums(.[14:15]) / .$"Status in employment (ICSE-93): Total") ## divide own account/family by total working

## calculate rate of youth in elementary employment
elementary <- read.csv("./data/raw/occupation_sex_age_ilostat.csv") %>%
  filter(obs_status.label != "Unreliable") %>%
  filter(classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24"),
         classif2.label %in% c("Occupation (ISCO-08): 9. Elementary occupations",
                               "Occupation (ISCO-08): Total"),
         obs_status.label != "Unreliable") %>%
  pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>%
  mutate(obs_value = 100 * rowSums(.[c(11,13)]) / rowSums(.[c(10,12)]))

## SAFF: rate of youth in working in skilled agriculture, fishery or forestry
saff <- read.csv("./data/raw/occupation_sex_age_ilostat.csv") %>%
  filter(obs_status.label != "Unreliable") %>%
  filter(classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24"),
         classif2.label %in% c("Occupation (ISCO-08): 6. Skilled agricultural, forestry and fishery workers",
                               "Occupation (ISCO-08): Total"),
         obs_status.label != "Unreliable") %>%
  pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>%
  mutate(obs_value = 100 * rowSums(.[c(11,13)]) / rowSums(.[c(10,12)]))

## calculate rate of youth in elementary employment
nosecondary <- read.csv("./data/raw/education_sex_dhs.csv") %>%
  mutate("Sex: Female" = rowSums(.[c(3:5)]),
         "Sex: Male" = rowSums(.[c(12:14)]),
         time = substr(Survey, 0, 4)) %>%
  mutate("Sex: Total" = rowMeans(.[c(22:23)])) %>% ## Total is assumed to be average of the male and female rates (see paper)
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

## read in and format literacy rates
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

## read in harmonized test scores
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

## create list of all 12 indicators to serve as main input to the rank_generator function
dfList <- list("neet"=neet, "relative_unemp"=relative_unemp, "mismatch"=mismatch, "workingpov"=workingpov, "underemp"=underemp, "informal"=informal, "vulnerable"=vulnerable, "elementary"=elementary, "saff"=saff, "nosecondary"=nosecondary, "literacy"=literacy, "test_scores"=test_scores)

## choose country list of interest
chosen_list <- country_lists[[3]]

rank <- rank_generator(dfList, chosen_list, bygender = FALSE, lastyear = 2013, impute = FALSE)


#----------
# Clean up

rm(chosen_list, country_lists, dfList, elementary, employed, informal, literacy, mismatch, neet, nosecondary, relative_unemp, saff, test_scores, total_emp, underemp, unemployed, vulnerable, workingpov)

#----------
# some plots

# hist(log(elementary$obs_value))
# hist(log(neet$obs_value))
# hist(log(relative_unemp$obs_value))
# hist(log(mismatch$obs_value))
# hist(log(underemp$obs_value))
# hist(log(vulnerable$obs_value))
# hist(log(saff$obs_value))
# hist(log(informal$obs_value))
# hist(log(workingpov$obs_value))
# hist(log(test_scores$obs_value))
# hist(log(literacy$obs_value))
# hist(log(nosecondary$obs_value))

plot(rank$index_mean, rank$index_geom)
text(rank$index_mean, rank$index_geom, labels=rank$country_code, cex= .7, pos = 3)
