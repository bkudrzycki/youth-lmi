library(tidyverse)
rm(list=ls())

#NEET rate
mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/neet_sex_ilostat.csv") 

indicators <- mydata %>%
  select(ref_area.label, source.label, sex.label,time, obs_value) %>%
  filter(sex.label=="Sex: Total") %>%
  group_by(ref_area.label) %>% 
  top_n(1, time) %>%
  filter(time > 2009) %>% 
  select(Country=ref_area.label, neet = obs_value)

#relative unemployment rate
mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/unemployment_sex_age_ilostat.csv")

mydata <- mydata %>%
  select(ref_area.label, classif1.label, sex.label, time, obs_value) %>%
  filter(sex.label=="Sex: Total") %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>% 
  mutate(relative_unemp = `Age (Youth, adults): 15-24`/`Age (Youth, adults): 25+`) %>% 
  group_by(ref_area.label) %>% 
  top_n(1, time) %>%
  filter(time > 2009) %>% 
  select(Country=ref_area.label, relative_unemp)

indicators <- full_join(indicators, mydata, by = "Country")

#skills mismatch rate

#read in employment x education data
mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/employment_edu_ilostat.csv")

empxeduc <- mydata %>% 
  select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
  filter(sex.label=="Sex: Total") %>%
  pivot_wider(names_from = c(classif2.label), values_from = obs_value) %>%
  mutate(prim_emp = rowSums(.[5:7], na.rm = TRUE), #sum aggregate educational attainment for youth and total
         sec_emp = rowSums(.[8:9], na.rm = TRUE),
         tert_emp = rowSums(.[c(10:12,14)], na.rm = TRUE)) %>% 
  mutate_all(funs(na_if(.,0))) %>% 
  select(ref_area.label, time, prim_emp, sec_emp, tert_emp, total_emp = "Education (ISCED-11): Total")
  
#read in unemployment x education data

mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/unemployment_edu_ilostat.csv")
  
uexeduc <- mydata %>% 
  select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
  filter(sex.label=="Sex: Total") %>%
  pivot_wider(names_from = c(classif2.label), values_from = obs_value) %>%
  mutate(prim_unemp = rowSums(.[5:7], na.rm = TRUE), #sum aggregate educational attainment for youth and total
         sec_unemp = rowSums(.[8:9], na.rm = TRUE),
         tert_unemp = rowSums(.[c(10:12,15)], na.rm = TRUE))  %>% 
  mutate_all(funs(na_if(.,0))) %>% 
  select(ref_area.label, time, prim_unemp, sec_unemp, tert_unemp, total_unemp = "Education (ISCED-11): Total")
  
#calculate mismatch indicator (for complete indicator, all four data points must be from SAME YEAR)
mismatch <- inner_join(uexeduc, empxeduc, by = c("ref_area.label", "time")) %>% 
  mutate(skills_mismatch = 100*1/3*(abs(prim_emp/total_emp-prim_unemp/total_unemp)+
                             abs(sec_emp/total_emp-sec_unemp/total_unemp)+
                             abs(tert_emp/total_emp-tert_unemp/total_unemp))) %>% 
  group_by(ref_area.label) %>% 
  top_n(1, time) %>%
  filter(time > 2009) %>% 
  select(Country = ref_area.label, skills_mismatch)

indicators <- full_join(indicators, mismatch, by = "Country")
rm(empxeduc, uexeduc, mismatch)

#calculate transition score
rescale <- function(x, na.rm = FALSE) (100-x)
rur_rescale <- function(x, na.rm = FALSE) ifelse(x < 1, 100, ifelse(x > 10, 0, (100-(((x-1)/(10-1))*100))))
hts_rescale <- function(x, na.rm = FALSE) (((x-300)/(625-300))*100) 

indicators <- indicators %>% 
  mutate_at(c("neet", "skills_mismatch"), rescale) %>% 
  mutate_at("relative_unemp", rur_rescale) %>% 
  ungroup(.) %>% 
  mutate(is_na = rowSums(is.na(.))) %>%
  mutate(transition_score = ifelse(is_na<2, rowMeans(.[3:5], na.rm = TRUE),NA))

#youth working poverty rate
mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/YWPR_Erwinraw_2020-04-04.csv") %>%
  mutate(below_320 = rowSums(.[4:5])) %>% 
  rename("Country" = country_name) %>% 
  select(Country, below_320)

indicators <- full_join(indicators, mydata, by = "Country")

#youth underemployment rate
mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/TIME_UNDEMP-ilostat-2020-03-04.csv") %>%
  select(ref_area.label, sex.label, classif1.label, time, obs_value) %>%
  filter(sex.label=="Sex: Total", classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24")) %>% 
  pivot_wider(names_from = c(classif1.label), values_from = obs_value) %>% 
  rename("underemp_15_19" = "Age (Youth bands): 15-19", "underemp_20_24" = "Age (Youth bands): 20-24") %>% 
  mutate(underemp_15_24 = rowSums(.[4:5]))

#read in total employment
to_merge <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/EMPLOYMENT-ilostat-2020-03-10.csv") %>%
  filter(sex.label=="Sex: Total", classif1.label == "Age (Aggregate bands): Total") %>% 
  select(ref_area.label, time, "total_emp" = obs_value)
  
#calculate underemployed rate
mydata <- full_join(mydata, to_merge, by=c("ref_area.label","time"))

rm(to_merge)

mydata <- mydata %>%
  mutate(underemp = underemp_15_24 / total_emp * 100) %>%
  group_by(ref_area.label) %>% 
  top_n(1, time) %>% 
  filter(time > 2009) %>% 
  rename(Country = "ref_area.label") %>% 
  select(Country, underemp)

indicators <- full_join(indicators, mydata, by = "Country")

#read in informal employment rate

mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/informality_age_sex_Bonnet.csv") %>%
  select(Country, informal = X15.24)

mydata$Country <- mydata$Country %>% 
  recode("Congo, Democratic Republic of" = "Congo, Democratic Republic of the",
         "Czech Republic" = "Czechia",
         "Lao Peoples Democratic Republic" = "Lao People's Democratic Republic",
         "Republic of Moldova" = "Moldova, Republic of",
         "Venezuela" = "Venezuela, Bolivarian Republic of")
  
indicators <- full_join(indicators, mydata, by = "Country")

#calculate vulnerable employment rate (own account and family workers)

mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/EMPSTAT_ilostat-2020-03-04.csv") %>% 
  select("Country" = ref_area.label, classif2.label, time, obs_value) %>%
  filter(classif2.label %in%	c("Status in employment (ICSE-93): 3. Own-account workers",
                               "Status in employment (ICSE-93): 5. Contributing family workers",
                               "Status in employment (ICSE-93): Total")) %>% 
  pivot_wider(names_from = classif2.label, values_from = obs_value) %>% 
  rename(own_account = "Status in employment (ICSE-93): 3. Own-account workers",
         family = "Status in employment (ICSE-93): 5. Contributing family workers",
         total = "Status in employment (ICSE-93): Total") %>% 
  mutate(vulnerable_15.24 = rowSums(.[4:5], na.rm = TRUE)) %>% 
  mutate(vulnerable = 100* vulnerable_15.24 / total) %>%
  group_by(Country) %>% 
  top_n(1, time) %>%
  filter(time > 2009) %>% 
  select(Country, vulnerable)

indicators <- left_join(indicators, mydata, by = "Country")

#calculate rate of youth in elementary occupations and rate of youth in working in skilled agriculture, fishery or forestry (SAFF)

mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/occupation_sex_age_ilostat.csv") %>%
  select("Country" = ref_area.label, sex.label, classif1.label, classif2.label, time, obs_value) %>%
  filter(sex.label == "Sex: Total",
         classif1.label %in% c("Age (Youth bands): 15-19",
                               "Age (Youth bands): 20-24"),
         classif2.label %in% c("Occupation (ISCO-08): 9. Elementary occupations",
                               "Occupation (ISCO-08): 6. Skilled agricultural, forestry and fishery workers",
                               "Occupation (ISCO-08): Total")) %>% 
  pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>% 
  mutate(elementary_15.24 = rowSums(.[c(6,9)])) %>% 
  mutate(saff_15.24 = rowSums(.[c(5,8)])) %>% 
  mutate(total_emp_15.24 = rowSums(.[c(4,7)])) %>% 
  mutate(elementary = 100 * elementary_15.24 / total_emp_15.24) %>% 
  mutate(saff = 100 * saff_15.24 / total_emp_15.24) %>% 
  group_by(Country) %>% 
  top_n(1, time) %>%
  filter(time > 2009) %>% 
  select(Country, elementary, saff)

indicators <- full_join(indicators, mydata, by = "Country")

#calculate working conditions score

indicators <- indicators %>%
  mutate_at(c("below_320", "underemp", "informal", "vulnerable", "elementary", "saff"), rescale) %>%
  ungroup(.) %>% 
  mutate(is_na = rowSums(is.na(.[7:12]))) %>%
  mutate(working_cond_score = ifelse(is_na<3, rowMeans(.[7:12], na.rm = TRUE),NA)) %>% 
  select(-is_na)

#calculate percent of youth with less than secondary education

mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/education_sex_dhs.csv") %>% 
  select(c(1:5),c(12:14)) %>% 
  mutate(nosecondary_female = rowSums(.[c(3:5)]),
         nosecondary_male = rowSums(.[c(6:8)])) %>% 
  mutate(nosecondary = rowMeans(.[c(9:10)])) %>% 
  mutate(time = substr(Survey, 0, 4)) %>% 
  group_by(Country) %>% 
  top_n(1, time) %>% 
  filter(time > 2009) %>% 
  select(Country, nosecondary)

#rename countries to match indicators dataframe

mydata$Country <- mydata$Country %>% 
  recode("Kyrgyz Republic" = "Kyrgyzstan",
         "Congo Democratic Republic" = "Congo, Democratic Republic of the",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Tanzania" = "Tanzania, United Republic of")
  
indicators <- full_join(indicators, mydata, by = "Country")

#read in illiteracy rate data
mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/literacy_sex_unesco.csv") %>% 
  select(Country, Indicator, Time, Value) %>% 
  pivot_wider(names_from = Indicator, values_from = Value) %>%
  rename_at(vars(3:5), ~c("Female literacy","Male literacy","Total literacy")) %>% 
  group_by(Country) %>% 
  top_n(1, Time) %>% 
  filter(Time > 2009)

#rename countries to match names in indicators dataframe

mydata$Country <- mydata$Country %>% 
  recode("United Republic of Tanzania" = "Tanzania, United Republic of",
         "Bolivia (Plurinational State of)" = "Bolivia",
         "China, Macao Special Administrative Region" = "Macau, China",
         "Democratic Republic of the Congo" = "Congo, Democratic Republic of the",
         "Iran (Islamic Republic of)" = "Iran, Islamic Republic of",
         "Venezuela (Bolivarian Republic of)" = "Venezuela, Bolivarian Republic of",
         "Palestine" = "Occupied Palestinian Territory",
         "Republic of Moldova" = "Moldova, Republic of")

mydata <- mydata %>% 
  select(Country, literacy_rate = "Total literacy")

indicators <- full_join(indicators, mydata, by = "Country")

#read in harmonized test scores

mydata <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/raw/testscores_erwin_wb.csv") %>% 
  select("country_code" = country_abbreviation, "harm_testscores" = HTS)
  
codes <- read.csv("~/polybox/Youth Employment/1b Index/youth-lmi/data/country_codes.csv") %>% 
  select(Country, country_code = "Alpha.3.code") %>% 
  mutate(country_code = substr(country_code, 2, 4)) #remove space in front of country code

codes$Country <- codes$Country %>% 
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

codes <- codes %>% 
  add_row(Country = "Kosovo", country_code = "XKX") %>% 
  add_row(Country = "Curaçao", country_code = "CUW") %>% 
  add_row(Country = "Eswatini", country_code = "SWZ") %>% 
  add_row(Country = "Macau, China", country_code = "MAC") %>% 
  add_row(Country = "South Sudan", country_code = "SSD")
  

indicators <- full_join(indicators, codes, by = "Country") %>% 
  .[c(1,15,2:14)]
rm(codes)

#match harmonized test scores to countries by country code
indicators <- full_join(indicators, mydata, by = "country_code")

#calculate education score
indicators <- indicators %>%
  mutate_at("nosecondary", rescale) %>%
  mutate_at("harm_testscores", hts_rescale) %>% 
  ungroup(.) %>% 
  mutate(is_na = rowSums(is.na(.[14:16]))) %>%
  mutate(education_score = ifelse(is_na<2, rowMeans(.[14:16], na.rm = TRUE),NA)) %>% 
  select(-is_na)

#calculate index score
indicators <- indicators %>% 
  mutate(is_na = rowSums(is.na(.[c(6,13,17)]))) %>% 
  mutate(index_score = ifelse(is_na==0, rowMeans(.[c(6,13,17)], na.rm = TRUE),NA)) %>% 
  select(-is_na)

rm(mydata, hts_rescale, rescale, rur_rescale)
  
  