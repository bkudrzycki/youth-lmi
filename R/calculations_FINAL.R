# Package names
packages <- c("tidyverse", "here", "matrixStats", "gtsummary", "readxl", "ggplot2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)

## ---------------------------------------

devtools::load_all(here("lamadex"))

rank <- rank_generator(bygender = "Total", countries = "dev", years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean))

## max/min test scores, LIC/LMIC scores vs HIC/HMIC scores



df <- test_scores %>% filter(sex.label == "Sex: Total",
                             !is.na(obs_value)) %>%
  mutate(income = ifelse(ref_area.label %in% countryLists()[[3]][[1]], "lics", "hics"),
         SSA = ifelse(ref_area.label %in% countryLists()[[4]][[1]], "SSA", "nonSSA")) %>% 
  group_by(ref_area.label) %>%
  top_n(1, time)

tapply(df$obs_value, df$income, summary)
tapply(df$obs_value, df$SSA, summary)

## standard deviation of rankings across dimensions
sd <- rank %>% dplyr::select(country, transition_mean, working_conditions_mean, education_mean) %>% 
  mutate(transition_rank = dense_rank(desc(transition_mean)),
         wc_rank = dense_rank(desc(working_conditions_mean)),
         education_rank = dense_rank(desc(education_mean)))
sd <- sd %>% mutate(stDev = rowSds(as.matrix(.[5:7])))

## differences in youth/adult ratios in working poverty, unemployment, underemployment, HICs vs LICs, weighted by population
data(unemployment_rate)

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

compress <- function(x) {
  df <- x %>% 
    filter(sex.label == "Sex: Total",
           !is.na(obs_value)) %>%
    group_by(ref_area.label, classif1.label) %>%
    top_n(1, time) %>% 
    dplyr::select(ref_area.label, classif1.label, obs_value) %>% 
    pivot_wider(names_from = classif1.label, values_from = obs_value)
}

df <- compress(unemployment_rate) %>% 
  rename(unemp_youth = `Age (Youth, adults): 15-24`,
         unemp_adults = `Age (Youth, adults): 25+`) %>% 
  na.omit() %>% 
  full_join(compress(underemp) %>% na.omit(), by = "ref_area.label") %>% 
  rename(underemp_youth = `Age (Youth, adults): 15-24`,
         underemp_adults = `Age (Youth, adults): 25+`) %>% 
  full_join(compress(working_pov) %>% na.omit(), by = "ref_area.label") %>% 
  rename(workpov_youth = `Age (Youth, adults): 15-24`,
         workpov_adults = `Age (Youth, adults): 25+`) %>% 
  full_join(pop, by = c("ref_area.label" = "country")) %>% 
  mutate(income = ifelse(ref_area.label %in% countryLists()[[3]][[1]], "lics", "hics")) %>% 
  filter(!is.na(total_pop))

unemp_youth <- df %>% 
  group_by(income) %>% 
  summarise(unemp_youth = weighted.mean(unemp_youth, w = total_pop, na.rm = T)) %>% 
  pivot_wider(names_from = income, values_from = unemp_youth) %>% mutate(var = "unemp_youth")

unemp_adults <- df %>% 
  group_by(income) %>% 
  summarise(unemp_adults = weighted.mean(unemp_adults, w = total_pop, na.rm = T)) %>% 
  pivot_wider(names_from = income, values_from = unemp_adults) %>% mutate(var = "unemp_adults")

underemp_youth <- df %>% 
  group_by(income) %>% 
  summarise(underemp_youth = weighted.mean(underemp_youth, w = total_pop, na.rm = T)) %>% 
  pivot_wider(names_from = income, values_from = underemp_youth) %>% mutate(var = "underemp_youth")

underemp_adults <- df %>% 
  group_by(income) %>% 
  summarise(underemp_adults = weighted.mean(underemp_adults, w = total_pop, na.rm = T)) %>% 
  pivot_wider(names_from = income, values_from = underemp_adults) %>% mutate(var = "underemp_adults")

workpov_youth <- df %>% 
  group_by(income) %>% 
  summarise(workpov_youth = weighted.mean(workpov_youth, w = total_pop, na.rm = T)) %>% 
  pivot_wider(names_from = income, values_from = workpov_youth) %>% mutate(var = "workpov_youth")

workpov_adults <- df %>% 
  group_by(income) %>% 
  summarise(workpov_adults = weighted.mean(workpov_adults, w = total_pop, na.rm = T)) %>% 
  pivot_wider(names_from = income, values_from = workpov_adults) %>% mutate(var = "workpov_adults")


joined <- rbind(unemp_youth, unemp_adults, underemp_youth, underemp_adults, workpov_youth, workpov_adults)

names(joined) <- joined[1,]
joined <- joined[-1,] %>% mutate(var = rownames(.))


## gender comparison


male <- rank_generator(bygender = "Male", years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean))

female <- rank_generator(bygender = "Female", years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean))

diff <- left_join(male, female, by = "country") %>% 
  dplyr::select(country, index_mean.x, index_mean.y) %>% 
  mutate(diff = index_mean.y - index_mean.x) %>% 
  filter(!is.na(diff))

## weighted female vs. male neet
df <- neet  %>%
  group_by(ref_area.label, sex.label) %>%
  top_n(1, time) %>% 
  full_join(pop, by = c("ref_area.label" = "country")) %>% 
  filter(sex.label %in% c("Sex: Male", "Sex: Female"),
         ref_area.label %in% countryLists()[[3]][[1]],
         !is.na(obs_value),
         !is.na(total_pop)) %>% 
  pivot_wider(names_from = sex.label, values_from = obs_value) %>% 
  mutate(neet_diff = `Sex: Female`-`Sex: Male`)

weighted.mean(df$`Sex: Female`, w = df$youth_pop, na.rm = T)
weighted.mean(df$`Sex: Male`, w = df$youth_pop, na.rm = T)

## difference in transition score
mean(male$transition_mean, na.rm = T)-mean(female$transition_mean, na.rm = T)

male <- male %>% left_join(pop, by = "country") %>% filter(!is.na(total_pop))
female <- female %>% left_join(pop, by = "country") %>% filter(!is.na(total_pop))
weighted.mean(male$transition_mean, w = male$youth_pop, na.rm = T) - weighted.mean(female$transition_mean, w = female$youth_pop, na.rm = T)

## difference in working conditions score
mean(male$working_conditions_mean, na.rm = T)-mean(female$working_conditions_mean, na.rm = T)

weighted.mean(male$working_conditions_mean, w = male$youth_pop, na.rm = T) - weighted.mean(female$working_conditions_mean, w = female$youth_pop, na.rm = T)

## difference in education score
mean(male$education_mean, na.rm = T)-mean(female$education_mean, na.rm = T)

weighted.mean(male$education_mean, w = male$youth_pop, na.rm = T) - weighted.mean(female$education_mean, w = female$youth_pop, na.rm = T)

## difference in test scores
weighted.mean(male$test_scores, w = male$youth_pop, na.rm = T) - weighted.mean(female$test_scores, w = female$youth_pop, na.rm = T)

## difference in literacy
weighted.mean(male$literacy, w = male$youth_pop, na.rm = T) - weighted.mean(female$literacy, w = female$youth_pop, na.rm = T)

df <- literacy  %>%
  group_by(ref_area.label, sex.label) %>%
  top_n(1, time) %>% 
  full_join(pop, by = c("ref_area.label" = "country")) %>% 
  filter(sex.label %in% c("Sex: Male", "Sex: Female"),
         ref_area.label %in% countryLists()[[3]][[1]],
         !is.na(obs_value),
         !is.na(total_pop)) %>% 
  dplyr::select(-SDG_IND, -Indicator, -Flag.Codes, -Flags, -TIME) %>% 
  pivot_wider(names_from = sex.label, values_from = obs_value) %>% 
  mutate(literacy_diff = `Sex: Female`-`Sex: Male`)

## difference in no secondary

weighted.mean(male$nosecondary, w = male$youth_pop, na.rm = T) - weighted.mean(female$nosecondary, w = female$youth_pop, na.rm = T)

x <- male %>% dplyr::select(country, male_nosecondary = nosecondary)
y <- female %>% dplyr::select(country, female_nosecondary =nosecondary)

df <- full_join(x,y, by = "country") %>% mutate(nosec_diff = female_nosecondary - male_nosecondary)

## gender index

gindex <- read_xlsx(here("data", "CFR_index.xlsx")) %>% 
  rename("CFR_score" = "OVERALL AVERAGE SCORE",
         "CFR_institutions" = "ACESSING INSTITUTIONS SCORE",
         "CFR_work_incentives" = "PROVIDING INCENTIVES TO WORK SCORE")

gindex$Economy <- gindex$Economy %>% ## fix country names to match ILOSTAT for joining
  recode("Democratic Republic of Congo" = "Congo, Democratic Republic of the",
         "East Timor" = "Timor-Leste",
         "Ivory Coast" = "Côte d'Ivoire",
         "Laos" = "Lao People's Democratic Republic",
         "Micronesia" = "Micronesia, Federated States of",
         "Moldova" = "Moldova, Republic of",
         "Republic of Congo" = "Congo",
         "Syria" = "Syrian Arab Republic",
         "Tanzania" = "Tanzania, United Republic of",
         "Vietnam" = "Viet Nam")

comp <- left_join(comp, gindex, by = c("country" = "Economy"))

summary(lm(formula = index_mean_female ~ CFR_score, data = comp))
summary(lm(formula = index_diff ~ CFR_score, data = comp))

## standard deviations of dimensions

sd(rank$transition_mean, na.rm = T)
sd(rank$working_conditions_mean, na.rm = T)
sd(rank$education_mean, na.rm = T)
13



