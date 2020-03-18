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

## calculate transition dimension using "transComp" function
index <- transComp(index, neet, relative_unemp, empxeduc, uexeduc, bygender = FALSE, lastyear = 2009)

