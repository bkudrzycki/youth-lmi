library(tidyverse)
library(here)
library(ggrepel)

setwd("~/polybox/Youth Employment/1b Index/youth-lmi")

devtools::load_all(here("lamadex"))
source(here("lamadex", "R", "source", "countryList.R"))
source(here("lamadex", "R", "source", "data_loader.R"))

rank <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = TRUE) %>%
  arrange(desc(index_mean))

regions <- read_csv(here("data", "raw", "country_regions.csv")) %>% 
  select("country" = "Country or Area",
         "Region Name",
         "Sub-region Name")

regions$`Sub-region Name` <- regions$`Sub-region Name` %>% #fix country names to match ILOSTAT for joining
  recode("Latin America and the Caribbean" = "Latin America")

regions$country <- regions$country %>% #fix country names to match ILOSTAT for joining
  recode("Democratic Republic of the Congo" = "Congo, Democratic Republic of the",
         "Republic of Moldova" = "Moldova, Republic of",
         "United Republic of Tanzania" = "Tanzania, United Republic of",
         "State of Palestine" = "Occupied Palestinian Territory",
         "Côte d’Ivoire" = "Côte d'Ivoire",
         "Bolivia (Plurinational State of)" = "Bolivia",
         "Cabo Verde" = "Cape Verde",
         "Micronesia (Federated States of)" = "Micronesia, Federated States of",
         "Democratic People's Republic of Korea" = "Korea, Democratic People's Republic of"
  )

rank <- left_join(rank, regions, by = c("country"))

rank <- rank %>% 
  arrange(desc(working_conditions_mean)) %>% 
  mutate(wc_rank = rank(-working_conditions_mean,na.last = "keep")) %>% 
  filter(!is.na(working_conditions_mean))

rank <- rank %>% 
  arrange(desc(transition_mean)) %>% 
  mutate(trans_rank = rank(-transition_mean,na.last = "keep")) %>% 
  filter(!is.na(transition_mean))

rank <- rank %>% 
  arrange(desc(education_mean)) %>% 
  mutate(educ_rank = rank(-education_mean,na.last = "keep")) %>% 
  filter(!is.na(education_mean))

rank$sd <- apply(rank[, c("wc_rank","trans_rank", "educ_rank")],1,sd)

cor(rank[, c("wc_rank","trans_rank", "educ_rank")])

rank[, c("wc_rank","trans_rank", "educ_rank")]

## GDP

gdp <- read.csv(here("data", "raw", "gdp_PPP_percap_worldbank.csv")) %>%
  rename("country" = Country.Name) %>%
  select(country, "gdp"=X2018)

gdp$country <- gdp$country %>%
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

gdp <- left_join(rank, gdp, by = "country") %>% 
  mutate(gdp = gdp/1000)

ggplot(gdp, aes(x = gdp, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  geom_smooth(method = lm, se = FALSE) +
  #ggtitle("Per capita GDP vs YLILI") +
  xlab("GDP per capita (PPP, current international $)") +
  ylab("YLILI score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

summary(lm(formula = index_mean ~ gdp, data = gdp))

# by dimension
summary(lm(formula = transition_mean~ gdp, data = gdp))
summary(lm(formula = working_conditions_mean ~ gdp, data = gdp))
summary(lm(formula = education_mean ~ gdp, data = gdp))

## UNEMPLOYMENT RATE

unemployment_rate <- read.csv(here("data","raw","unemployment_rate_sex_age_ilostat.csv"))

unemployment_rate <- unemployment_rate %>% 
  rename(country = ref_area.label) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(country) %>%
  top_n(1, time)

unemployment_rate <- left_join(rank, unemployment_rate, by = "country")

ggplot(unemployment_rate, aes(x = obs_value, y = index_mean, label = country_code)) +
  geom_point(size = 2) +
  geom_smooth(method = lm,  se = FALSE) +
  xlab("Youth unemployment rate") +
  ylab("YLILI score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)
#+ ggsave(here("score_vs_youth_unemp.png"), width = 20, height = 12, units = "cm")

summary(lm(formula = index_mean ~ obs_value, data = unemployment_rate))

## POPULATION GROWTH

pop_growth <- read.csv(here("data","raw","population_growth_wb.csv")) %>%
  rename("country" = "Country.Name",
         "pop_growth_then" = X2005,
         "pop_growth_now" = X2019) %>% 
  select(c(country, pop_growth_then, pop_growth_now))

pop_growth$country <- pop_growth$country %>%
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

pop_growth <- left_join(rank, pop_growth, by = "country")

ggplot(pop_growth, aes(x = pop_growth_now, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Population Growth vs YLILI") +
  xlab("Population Growth (2019)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ pop_growth_now, data = pop_growth))

ggplot(pop_growth, aes(x = pop_growth_then, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Population Growth vs YLILI") +
  xlab("Population Growth (2005)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ pop_growth_then, data = pop_growth))

ggplot(pop_growth, aes(x = pop_growth_then, y = transition_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Population Growth vs Transition Score") +
  xlab("Population Growth (2019)") +
  ylab("Education Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = transition_mean ~ pop_growth_then, data = pop_growth))

ggplot(pop_growth, aes(x = pop_growth_then, y = working_conditions_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Population Growth vs Working Conditions Score") +
  xlab("Population Growth (2019)") +
  ylab("Education Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = working_conditions_mean ~ pop_growth_then, data = pop_growth))

ggplot(pop_growth, aes(x = pop_growth_then, y = education_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Population Growth vs Education Score") +
  xlab("Population Growth (2019)") +
  ylab("Education Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = education_mean ~ pop_growth_then, data = pop_growth))


#GDP GROWTH

gdp_growth <- read.csv(here("data","raw","gdp_growth_worldbank.csv")) %>%
  rename("country" = "Country.Name",
         "gdp_growth19" = X2019) %>% 
  select(c(country, gdp_growth19))

gdp_growth$country <- gdp_growth$country %>%
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

gdp_growth <- left_join(rank, gdp_growth, by = "country")

ggplot(gdp_growth, aes(x = gdp_growth19, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("GDP Growth vs YLILI") +
  xlab("GDP Growth (2019)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ gdp_growth19, data = gdp_growth))

## INFLATION

inflation <- read.csv(here("data","raw","inflation_worldbank.csv")) %>%
  rename("country" = "Country.Name",
         "inflation19" = X2019) %>% 
  select(c(country, inflation19))

inflation$country <- inflation$country %>%
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

inflation <- left_join(rank, inflation, by = "country")

ggplot(inflation, aes(x = inflation19, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Inflation vs YLILI") +
  xlab("Inflation (2019)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ inflation19, data = inflation))

# PRODUCTIVITY
productivity <- read.csv(here("data","raw","productivity_ilostat.csv")) %>% 
  rename(country_code = ref_area,
         productivity19 = obs_value) %>% 
  mutate(productivity19 = productivity19/1000) %>% 
  filter(time == 2019)

productivity <- left_join(rank, productivity, by = "country_code")

ggplot(productivity, aes(x = productivity19, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Productivity vs YLILI") +
  xlab("2019 Output per worker (Thousands of 2011 GDP constant international $ in PPP)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ productivity19, data = productivity))


# PRODUCTIVITY GROWTH
prod_growth <- read.csv(here("data","raw","productivity_growth_ilostat.csv")) %>% 
  rename(country_code = ref_area,
         prod_growth19 = obs_value) %>% 
  filter(time == 2019)

prod_growth <- left_join(rank, prod_growth, by = "country_code")

ggplot(prod_growth, aes(x = prod_growth19, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Productivity Growth vs YLILI") +
  xlab("Annual growth rate of output per worker (GDP constant 2011 international $ in PPP) (%)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ prod_growth19, data = prod_growth))


## BY ABSOLUTE YOUTH POPULATION AND YOUTH-TO ADULT RATIO

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

pop <- left_join(rank, pop, by = "country")

pop %>% 
  filter(country != "India") %>% 
  ggplot(aes(x = youth_pop, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Absolute Youth Population vs YLILI") +
  xlab("15-24 Population (2019)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

x <- pop %>% 
  filter(country != "India") 
summary(lm(formula = index_mean ~ youth_pop, data = x))

pop$log_pop <- log(pop$total_pop)

pop %>% 
  ggplot(aes(x = log_pop, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Log Youth Population vs YLILI") +
  xlab("Log 15-24 Population (2019)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ log_pop, data = pop))

ggplot(aes(x = youth_ratio, y = index_mean, label = country_code), data = pop) +
  geom_point(size=2) +
  ggtitle("Youth Ratio vs YLILI") +
  xlab("Aged 15-24 as Fraction of Total Population (2019)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ youth_ratio, data = pop))

ggplot(aes(x = youth_ratio, y = working_conditions_mean, label = country_code), data = pop) +
  geom_point(size=2) +
  ggtitle("Youth Ratio vs Working Conditions") +
  xlab("Aged 15-24 as Fraction of Total Population (2019)") +
  ylab("Working conditions score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = working_conditions_mean ~ youth_ratio, data = x))

ggplot(aes(x = youth_ratio, y = education_mean, label = country_code), data = pop) +
  geom_point(size=2) +
  ggtitle("Youth Ratio vs Education Score") +
  xlab("Aged 15-24 as Fraction of Total Population (2019)") +
  ylab("Education score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = education_mean ~ youth_ratio, data = x))


## top quintile (11 countries)
pop_growth <- pop_growth %>% 
  mutate(pop_now_top11 = as.numeric(ntile(-pop_growth_now, 5)==1))

pop_growth <- pop_growth %>% 
  mutate(pop_then_top11 = as.numeric(ntile(-pop_growth_then, 5)==1))

pop_growth %>% dplyr::select(contains("mean"), pop_now_top11) %>% 
  tbl_summary(by =  pop_now_top11,
              statistic = list(all_continuous() ~ "{mean} [{median}] ({sd})"),
              missing = "no") %>% 
  add_p() 


## SAVINGS
savings <- read.csv(here("data","raw","savings_worldbank.csv")) %>%
  rename("country" = "Country.Name",
         "savings19" = X2019) %>% 
  select(c(country, savings19))

savings$country <- savings$country %>%
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

savings <- left_join(rank, savings, by = "country") %>%
  mutate(log_savings = log(savings19))

ggplot(savings, aes(x = log_savings, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Savings vs YLILI") +
  xlab("Log gross domestic savings (current LCU)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ log_savings, data = savings))

## SAVINGS RATE
savings_rate <- read.csv(here("data","raw","savings_rate_worldbank.csv")) %>%
  rename("country" = "Country.Name",
         "savings_rate19" = X2019) %>% 
  select(c(country, savings_rate19))

savings_rate$country <- savings_rate$country %>%
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


savings_rate <- left_join(rank, savings_rate, by = "country")

ggplot(savings_rate, aes(x = savings_rate19, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Savings Rate vs YLILI") +
  xlab("Gross savings (% of GDP)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ savings_rate19, data = savings_rate))


## Fertility

fertility <- read.csv(here("data","raw","fertility_worldbank.csv")) %>%
  rename("country" = "Country.Name",
         "fertility18" = X2018) %>% 
  select(c(country, fertility18))

fertility$country <- fertility$country %>%
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

fertility <- left_join(rank, fertility, by = "country")

ggplot(fertility, aes(x = fertility18, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Fertility vs YLILI") +
  xlab("Fertility rate, total (births per woman, 2018)") +
  ylab("YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE)

summary(lm(formula = index_mean ~ fertility18, data = fertility))



df <-informal %>% mutate(gap = `X25.29..Women.`-`X25.29..Men.`) %>% 
  left_join(gdp, by = c("ref_area.label" = "country")) %>% 
  left_join(pop, by = c("ref_area.label" = "country"))

df %>% filter(gdp < 60000, total_pop > 50000) %>% ggplot(aes(x = gdp, y = gap, size = youth_pop, weight=youth_pop)) +
  geom_point() +
  theme_minimal() +
  geom_text_repel(aes(label=ISO),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  scale_size_area(max_size = 10)
  
  