library(tidyverse)

lmi_data <- read_excel("~/polybox/Youth Employment/1b Index/youth-lmi/data/last_available_year_YLILI.xlsx")

rescale <- function(x, na.rm = FALSE) (100-x)
rur_rescale <- function(x, na.rm = FALSE) ifelse(x < 1, 100, ifelse(x > 10, 0, (100-(((x-1)/(10-1))*100))))
hts_rescale <- function(x, na.rm = FALSE) (((x-300)/(625-300))*100) 

#rescale variables
indicators <- lmi_data %>% 
  select(country_name, country_abbreviation, SOYN, RUR, YSMR, YWPR_EPMP, YTRUR, SYIE, YVER, SYEO, SYSAF, SYNSE, YIR, HTS) %>% 
  mutate_at(c("SOYN", "YSMR", "YWPR_EPMP", "YTRUR", "SYIE", "YVER", "SYEO", "SYSAF", "SYNSE", "YIR", "YTRUR"), rescale) %>% 
  mutate_at("RUR", rur_rescale) %>% 
  mutate_at("HTS", hts_rescale)

#calculate transition score
transition <- indicators %>% 
  select(country_name, country_abbreviation, SOYN, RUR, YSMR) %>% 
  mutate(is_na = rowSums(is.na(.))) %>% 
  mutate(transition_score = ifelse(is_na<2, rowMeans(.[3:5], na.rm = TRUE),NA))

#calculate working conditions score
working_cond <- indicators %>% 
  select(country_name, country_abbreviation, YWPR_EPMP, YTRUR, SYIE, YVER, SYEO, SYSAF) %>% 
  mutate(is_na = rowSums(is.na(.))) %>%
  mutate(working_cond_score = ifelse(is_na<4, rowMeans(.[3:8], na.rm = TRUE),NA))

#calculate education score
education <- indicators %>% 
  select(country_name, country_abbreviation, SYNSE, YIR, HTS) %>% 
  mutate(is_na = rowSums(is.na(.))) %>% 
  mutate(education_score = ifelse(is_na<2, rowMeans(.[3:5], na.rm = TRUE),NA))

#calculate final YLILI score
ylili <- indicators %>% 
  select(country_name, country_abbreviation) %>% 
  mutate(transition_score = transition$transition_score,
         working_cond_score = working_cond$working_cond_score,
         education_score = education$education_score) %>%
  mutate(YLILI_SCORE = rowMeans(.[3:5], na.rm = FALSE))

             