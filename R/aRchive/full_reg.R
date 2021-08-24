setwd("~/polybox/Youth Employment/1b Index/youth-lmi")

# Package names
packages <- c("here", "tidyverse", "stargazer", "gtsummary", "openxlsx")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)

devtools::load_all(here("lamadex"))
country_lists <- lamadex::countryLists()

rank <- rank_generator(bygender = "Total", lastyear = 2010, impute = TRUE) %>%
  arrange(desc(index_mean)) %>% filter(country %in% country_lists[[3]][[1]])
male <- rank_generator(bygender = "Male", lastyear = 2010, impute = TRUE) %>%
  select(country, male_index_mean = index_mean) %>% filter(country %in% country_lists[[3]][[1]])
female <- rank_generator(bygender = "Female", lastyear = 2010, impute = TRUE) %>%
  select(country, female_index_mean = index_mean) %>% filter(country %in% country_lists[[3]][[1]])

rank <- rank %>% 
  select(country, country_code, transition_mean, working_conditions_mean, education_mean, index_mean) %>% 
  left_join(male, by = "country") %>% 
  left_join(female, by = "country")

## recode function for world bank data

wb_recode <- function(data) {
  data$`Country.Name` <- data$`Country.Name` %>% 
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
           "West Bank and Gaza" = "Occupied Palestinian Territory",
           "Micronesia, Fed. Sts." = "Micronesia, Federated States of",
           "Korea, Dem. People’s Rep." = "Korea, Democratic People's Republic of",
           "Cabo Verde" = "Cape Verde")
  data %>% rename(country = Country.Name)
}

# load gdp

gdp <- read.csv(here("data", "raw", "gdp_PPP_percap_worldbank.csv")) %>%
  wb_recode() %>% 
  mutate(gdp = X2018) %>% 
  select(country, gdp)

# load productivity growth

prod_growth <- read.csv(here("data","raw","productivity_growth_ilostat.csv")) %>% 
  rename(country_code = ref_area,
         prod_growth19 = obs_value) %>% 
  filter(time == 2019)

# load youth population

pop <- read.csv(here("data","raw","population_age_ilostat.csv")) %>%
  filter(time == 2019,
         sex.label == "Sex: Total") %>% 
  pivot_wider(id_cols = c(ref_area.label, classif1.label, obs_value), names_from = classif1.label, values_from = obs_value)

pop <- pop %>% 
  mutate(youth_ratio = `Age (Youth, adults): 15-24` / `Age (Aggregate bands): Total`) %>%
  rename("country" = "ref_area.label",
         "total_pop" = `Age (Aggregate bands): Total`,
         "youth_pop" = `Age (Youth, adults): 15-24`,
         "adult_pop" = `Age (Youth, adults): 25+`)

# load population growth

pop_growth <- read.csv(here("data","raw","population_growth_wb.csv")) %>%
  wb_recode() %>%
  rename("pop_growth_2005" = X2005,
         "pop_growth_2019" = X2019) %>% 
  select(c(country, pop_growth_2005, pop_growth_2019))

## load minimum wage

mw_usd <- minimum_wage <- read.csv(here("data","raw","minimum_wage_ilostat.csv")) %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>% 
  rename("country" = "ref_area.label",
         "mw_usd" = `Currency: U.S. dollars`) %>% 
  filter(!is.na(`mw_usd`)) %>% 
  group_by(country) %>% 
  top_n(1, time) %>% 
  select(c(country, mw_usd, time))
  
mw_ppp <- minimum_wage <- read.csv(here("data","raw","minimum_wage_ilostat.csv")) %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>% 
  rename("country" = "ref_area.label",
         "mw_ppp" = `Currency: 2017 PPP $`) %>% 
  filter(!is.na(`mw_ppp`)) %>% 
  group_by(country) %>% 
  top_n(1, time) %>% 
  select(c(country, mw_ppp, time))

## load fertility rate

fertility <- read.csv(here("data","raw","fertility_worldbank.csv")) %>%
  wb_recode() %>% 
  pivot_longer(cols = c(5:65), names_to = "time", names_prefix = "X", values_to = "obs_value") %>% 
  filter(time > 2009,
         !is.na(obs_value)) %>%
  group_by(country) %>% 
  top_n(1, time) %>% 
  select(c(country, fertility_rate = obs_value, time))

## load unemployment_rate 

youth_unemp_rate <- unemployment_rate %>% 
rename(country = ref_area.label) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(country) %>%
  top_n(1, time) %>% 
  select(country, youth_unemp_rate = obs_value, time)


## load savings rate

savings_rate <- read.csv(here("data","raw","savings_rate_worldbank.csv")) %>%
  wb_recode() %>% 
  pivot_longer(cols = c(5:65), names_to = "time", names_prefix = "X", values_to = "obs_value") %>% 
  filter(time > 2009,
         !is.na(obs_value)) %>%
  group_by(country) %>% 
  top_n(1, time) %>% 
  select(c(country, savings_rate = obs_value, time))


## load hdi

hdi <- read.xlsx(here("data", "hdi.xlsx")) %>% select(c(country, hdi =`2019`)) %>% 
  mutate(hdi = hdi*100,
         country = substr(country,2,str_length(country)))

hdi$country <- hdi$country %>% 
  recode("Congo (Democratic Republic of the)" = "Congo, Democratic Republic of the",
         "Moldova (Republic of)" = "Moldova, Republic of",
         "Tanzania (United Republic of)" = "Tanzania, United Republic of",
         "Iran (Islamic Republic of)" = "Iran, Islamic Republic of",
         "Palestine, State of" = "Occupied Palestinian Territory")
           

## load remaining correlates
correlates <- read.xlsx(here("data", "raw", "correlates_wb.xlsx")) %>% 
  pivot_longer(c("2016.[YR2016]":"2020.[YR2020]"), names_to = "time") %>% 
  mutate(time = as.numeric(substr(time, 1,4))) %>% 
  rename(series = `Series.Name`) %>% 
  wb_recode() %>% 
  filter(country %in% country_lists[[3]][[1]]) 


y <- levels(factor(correlates$series))

count = 0
# select most recent observation for each variable
for (i in y) {
  x <- correlates %>% 
    filter(series == i) %>% 
    group_by(country) %>% 
    filter(value != "..") %>% 
    top_n(1, time) %>% 
    select(country, value) %>% 
    mutate(value = as.numeric(value))
  assign(paste0("corr", count), x)
  count = count + 1
}


df <- rank %>%
  left_join(gdp, by = "country") %>% 
  left_join(youth_unemp_rate, by = "country") %>% 
  left_join(pop_growth, by = "country") %>% 
  left_join(fertility, by = "country") %>% 
  left_join(minimum_wage, by = "country") %>% 
  left_join(savings_rate, by = "country") %>% 
  left_join(pop, by = "country") %>% 
  left_join(hdi, by = "country") %>% 
  left_join(corr0, by = "country") %>% 
  rename("access_to_elec" = value) %>% 
  left_join(corr1, by = "country") %>% 
  rename("agriculture" = value) %>% 
  left_join(corr4, by = "country") %>% 
  rename("doing_business" = value) %>% 
  left_join(corr5, by = "country") %>% 
  rename("export" = value) %>% 
  left_join(corr6, by = "country") %>% 
  rename("fdi" = value) %>% 
  left_join(corr9, by = "country") %>% 
  rename("manu_value" = value) %>% 
  left_join(corr11, by = "country") %>% 
  rename("urban_pop" = value) %>% 
  mutate(youth_ratio = youth_ratio*100,
         log_gdp = log(gdp))
  


m1 <- lm(data = df, index_mean ~ hdi)

m2 <- lm(data = df, index_mean ~ youth_unemp_rate)

m3 <- lm(data = df, index_mean ~ log_gdp)

m4 <- lm(data = df, index_mean ~ youth_ratio)

m5 <- lm(data = df, index_mean ~ fertility_rate)

m6 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate) 

m7 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export) 

m8 <- lm(data = df, index_mean ~ youth_ratio + fertility_rate + agriculture + manu_value + export + fdi + savings_rate + doing_business + urban_pop + access_to_elec)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, omit.stat = c("f", "adj.rsq", "ser"), column.sep.width = "-10pt", dep.var.labels = c("YLILI Score"), covariate.labels = c("HDI Score", "Youth unemployment rate", "log(GDP)","Youth population ratio", "Fertility rate", "Agriculture (\\% of GDP)", "Manufacturing (\\% of GDP)", "Exports (\\% of GDP)", "FDI (\\% of GDP)", "Savings rate (\\% of GDP)", "Ease of Doing Business", "Urbanization rate", "Access to Electr."), omit = "Constant", column.labels = NULL, model.numbers = FALSE)



m7 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m8 <- lm(data = df, male_index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m9 <- lm(data = df, female_index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m10 <- lm(data = df, transition_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m11 <- lm(data = df, working_conditions_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m12 <- lm(data = df, education_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)
            


stargazer(m7, m8, m9, m10, m11, m12, omit.stat = c("f", "adj.rsq", "ser"), column.sep.width = "-10pt", dep.var.labels = c("YLILI Score", "Male", "Female", "Transition", "working cond.", "education"), covariate.labels = c("log(GDP)","Youth population ratio", "Fertility rate", "Agriculture (\\% of GDP)", "Manufacturing (\\% of GDP)", "Exports (\\% of GDP)"), omit = "Constant", column.labels = NULL, model.numbers = FALSE)


