# Package names
packages <- c("tidyverse", "here", "corrplot", "stargazer", "data.table", "reshape2", "xtable", "openxlsx")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)
## ---------------------------------------

## load lamadex and list of country lists
devtools::load_all(here("lamadex"))

rank <- rank_generator(bygender = "Total", countries = "dev", years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean))

## Pearson correlation between indicators, dimenstions, and YLILI score

cormat_indices <- rank %>% 
  rename("YLILI score" = index_mean,
         "work cond. ratio" = relative_wc,
         "test scores" = test_scores) %>% 
  dplyr::select(c(3:12,19)) %>% 
  filter(!is.na(`YLILI score`)) %>% 
  as.matrix()

cormat_indices <- cormat_indices %>% 
  cor()

lower <- cormat_indices %>% round(3)
lower[lower.tri(cormat_indices, diag=TRUE)]<-""
lower <- as.matrix(lower)

stargazer(lower,align = T)


## number of indicators available by year

dfList <- compute_indicators()

compress <- function (x) {
  x %>%
    filter(ref_area.label %in% countryLists()[[3]][[1]],
           sex.label == "Sex: Total",
           !is.na(obs_value)) %>%
    summarise(time, ref_area.label) %>% 
    mutate(dummy = 1,) %>% 
    acast(ref_area.label ~ time, value.var = "dummy") %>% 
    t() %>% as.data.frame() %>% rownames_to_column(var ="Year") %>% 
    rename("Cote d'Ivoire" = "Côte d'Ivoire")
}

years_list <- lapply(dfList, compress) %>% 
  rbindlist(fill = TRUE) %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  t() %>% as.data.frame()

names(years_list) = years_list[1,]
years_list = years_list[-1,]

df <- years_list %>% count("num" = years_list$`2000`) %>% 
  rename("2000" = n) %>% 
  full_join(years_list %>% count("num" = years_list$`2001`) %>% rename("2001" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2002`) %>% rename("2002" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2003`) %>% rename("2003" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2004`) %>% rename("2004" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2005`) %>% rename("2005" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2006`) %>% rename("2006" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2007`) %>% rename("2007" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2008`) %>% rename("2008" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2009`) %>% rename("2009" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2010`) %>% rename("2010" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2011`) %>% rename("2011" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2012`) %>% rename("2012" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2013`) %>% rename("2013" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2014`) %>% rename("2014" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2015`) %>% rename("2015" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2016`) %>% rename("2016" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2017`) %>% rename("2017" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2018`) %>% rename("2018" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2019`) %>% rename("2019" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2020`) %>% rename("2020" = n), by = "num") %>% 
  t() %>% as.data.frame()

names(df) = df[1,]
df = df[-1,] %>% rownames_to_column(var ="Year") %>% arrange(desc(Year)) %>% mutate(`10` = NA)
df[is.na(df)] <- 0
print(xtable(df), include.rownames=FALSE)

## Coverage of indicators (%) by year, last available year

compress2 <- function (x, years) {
  varname <- deparse(substitute(x))
  x <- as_tibble(x) %>%
    filter(ref_area.label %in% countryLists()[[3]][[1]],
           sex.label == "Sex: Total",
           between(time, years[1], years[2]),
           !is.na(obs_value)) %>%
    group_by(ref_area.label) %>%
    top_n(1, time) %>% ## include only a single observation per country per year
    dplyr::select(ref_area.label, time)
}

last_year <- lapply(dfList, compress2, years = c(2010, 2020)) %>%
  reduce(full_join, by = "ref_area.label", accumulate == TRUE) %>%
  filter(ref_area.label %in% country_list[[1]])
  

colnames(last_year) <- c("ref_area.label", "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores")

#write.csv(last_year, "/Users/kudrzycb/Desktop/last_year.csv")

## regression tables

male <- rank_generator(bygender = "Male", years = c(2010, 2020), impute = TRUE) %>%
  dplyr::select(country, male_index_mean = index_mean)
female <- rank_generator(bygender = "Female", years = c(2010, 2020), impute = TRUE) %>%
  dplyr::select(country, female_index_mean = index_mean)

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
  dplyr::select(country, gdp)

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
  dplyr::select(c(country, pop_growth_2005, pop_growth_2019))

## load minimum wage

mw_usd <- minimum_wage <- read.csv(here("data","raw","minimum_wage_ilostat.csv")) %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>% 
  rename("country" = "ref_area.label",
         "mw_usd" = `Currency: U.S. dollars`) %>% 
  filter(!is.na(`mw_usd`)) %>% 
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, mw_usd, time))

mw_ppp <- minimum_wage <- read.csv(here("data","raw","minimum_wage_ilostat.csv")) %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>% 
  rename("country" = "ref_area.label",
         "mw_ppp" = `Currency: 2017 PPP $`) %>% 
  filter(!is.na(`mw_ppp`)) %>% 
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, mw_ppp, time))

## load fertility rate

fertility <- read.csv(here("data","raw","fertility_worldbank.csv")) %>%
  wb_recode() %>% 
  pivot_longer(cols = c(5:65), names_to = "time", names_prefix = "X", values_to = "obs_value") %>% 
  filter(time > 2009,
         !is.na(obs_value)) %>%
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, fertility_rate = obs_value, time))

## load unemployment_rate 

youth_unemp_rate <- unemployment_rate %>% 
  rename(country = ref_area.label) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(country) %>%
  top_n(1, time) %>% 
  dplyr::select(country, youth_unemp_rate = obs_value, time)


## load savings rate

savings_rate <- read.csv(here("data","raw","savings_rate_worldbank.csv")) %>%
  wb_recode() %>% 
  pivot_longer(cols = c(5:65), names_to = "time", names_prefix = "X", values_to = "obs_value") %>% 
  filter(time > 2009,
         !is.na(obs_value)) %>%
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, savings_rate = obs_value, time))


## load hdi

hdi <- read.xlsx(here("data", "hdi.xlsx")) %>% dplyr::select(c(country, hdi =`2019`)) %>% 
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
  wb_recode()


y <- levels(factor(correlates$series))

count = 0
# dplyr::select most recent observation for each variable
for (i in y) {
  x <- correlates %>% 
    filter(series == i) %>% 
    group_by(country) %>% 
    filter(value != "..") %>% 
    top_n(1, time) %>% 
    dplyr::select(country, value) %>% 
    mutate(value = as.numeric(value))
  assign(paste0("corr", count), x)
  count = count + 1
}

rank <- rank %>% 
  dplyr::select(country, country_code, transition_mean, working_conditions_mean, education_mean, index_mean) %>% 
  left_join(male, by = "country") %>% 
  left_join(female, by = "country")

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
  mutate(youth_ratio = youth_ratio * 100,
         log_gdp = log(gdp))



m1 <- lm(data = df, index_mean ~ youth_unemp_rate)

m2 <- lm(data = df, index_mean ~ hdi)

m3 <- lm(data = df, index_mean ~ log_gdp)

m4 <- lm(data = df, index_mean ~ youth_ratio)

m5 <- lm(data = df, index_mean ~ fertility_rate)

m6 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate) 

m7 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export) 

m8 <- lm(data = df, index_mean ~ youth_ratio + fertility_rate + agriculture + manu_value + export + fdi + savings_rate + doing_business + urban_pop + access_to_elec)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, omit.stat = c("f", "adj.rsq", "ser"), column.sep.width = "-10pt", dep.var.labels = c("YLILI Score"), covariate.labels = c("Youth unemployment rate", "HDI Score ($\\times$ 100)", "log(GDP)", "Youth pop. ratio ($\\times$ 100)", "Fertility rate", "Agriculture (\\% of GDP)", "Manufacturing (\\% of GDP)", "Exports (\\% of GDP)", "FDI (\\% of GDP)", "Savings rate (\\% of GDP)", "Ease of Doing Business", "Urbanization rate", "Access to Electricity"), omit = "Constant", column.labels = NULL, no.space=TRUE, float = FALSE, notes = "Standard errors shown in parentheses. YLILI score is on a scale of 0-100.")

m9 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m10 <- lm(data = df, male_index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m11 <- lm(data = df, female_index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m12 <- lm(data = df, transition_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m13 <- lm(data = df, working_conditions_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m14 <- lm(data = df, education_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

stargazer(m9, m10, m11, m12, m13, m14, omit.stat = c("f", "adj.rsq", "ser"), column.sep.width = "-10pt", dep.var.labels = c("YLILI Score", "Male", "Female", "Transition", "Working Cond.", "Education"), covariate.labels = c("log(GDP)","Youth pop. ratio ($\\times$ 100)", "Fertility rate", "Agriculture (\\% of GDP)", "Manufacturing (\\% of GDP)", "Exports (\\% of GDP)"), omit = "Constant", column.labels = NULL, model.numbers = FALSE, float = FALSE, notes = "Standard errors shown in parentheses. YLILI score is on a scale of 0-100.")


##alaternativ rankings

raw <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = FALSE) %>%
  arrange(desc(index_mean)) %>%
  filter(!is.na(index_mean)) %>% 
  select(country,
         "raw" = index_mean)

rank <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean)) %>% 
  mutate(rank = rank(-index_mean,na.last = "keep")) %>% 
  filter(!is.na(index_mean))

#remove one indicator at a time, recompile

#transition dimension

new_trans <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:4]))<2, rowMeans(.[3:4], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[5:8]))<3, rowMeans(.[5:8], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[9:11]))<2, rowMeans(.[9:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>% 
    select(c(!!varname))
}

df <- cbind(rank, new_trans(rank, "neet"))
df <- cbind(df, new_trans(rank, "relative_wc"))
df <- cbind(df, new_trans(rank, "mismatch"))

new_wc <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:5]))<2, rowMeans(.[3:5], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[6:8]))<3, rowMeans(.[6:8], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[9:11]))<2, rowMeans(.[9:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>%
    select(c(!!varname))
}

df <- cbind(df, new_trans(rank, "workingpov"))
df <- cbind(df, new_trans(rank, "underemp"))
df <- cbind(df, new_trans(rank, "informal"))
df <- cbind(df, new_trans(rank, "elementary"))

new_educ <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:5]))<2, rowMeans(.[3:5], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[6:9]))<3, rowMeans(.[6:9], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[10:11]))<2, rowMeans(.[10:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>% 
    select(c(!!varname))
}

df <- cbind(df, new_educ(rank, "nosecondary"))
df <- cbind(df, new_educ(rank, "literacy"))
df <- cbind(df, new_educ(rank, "test_scores"))

df <- df %>% select(-index_geom, index_geom)
df <- left_join(df, raw, by = "country")

#score correlations

scores <- df %>% 
  select(c("country", "index_mean", starts_with("no_"), "index_geom", "raw"))

pearson <- cor(as.matrix(scores[,-1]))[,1]
spearman <- cor(as.matrix(scores[,-1]), method = "spearman")[,1]

score_diffs <- abs(scores[2:ncol(scores)]-scores[,2]) %>%
  summarise_if(is.numeric, mean)

score_sd <- scores[2:ncol(scores)]-scores[,2]

score_sd <- score_sd %>% 
  summarise_if(is.numeric, sd)

##differences in rankings
to_rank <- function(x) {rank(-x)}

ranks <- df %>% 
  select(c("country", "index_mean", starts_with("no_"), "index_geom", "raw")) %>% 
  mutate_if(is.numeric, to_rank)

rank_diffs <- abs(ranks[2:ncol(ranks)]-ranks[,2]) %>% 
  summarise_if(is.numeric, mean)

max_rank_diffs <- abs(ranks[2:ncol(ranks)]-ranks[,2]) %>%
  summarise_if(is.numeric, max)

table <- rbind(pearson, score_diffs, score_sd, spearman, rank_diffs, max_rank_diffs)[,-1]

rownames(table) <- c("pearson", "score difference", "score diff sd", "spearman", "rank diff.", "max rank diff")

xtable(table, digits=3)

scores[,2:ncol(scores)] <- round(scores[,2:ncol(scores)],2)

for(i in 1:nrow(scores)) {
  for(j in 2:ncol(scores)) {
    scores[i,j] <- paste0(scores[i,j], " (", ranks[i,j], ")")
  }
}

stargazer(as.matrix(scores), summary = FALSE)

## Table of index outcomes by gender

male <- rank_generator(bygender = "Male", years = c(2010, 2020), impute = TRUE) %>%
  filter(!is.na(index_mean)) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select("YLILI score" = index_mean,
         "Transition" = transition_mean,
         "Share of youth NEET" = neet,
         "Youth skills mismatch rate" = mismatch,
         "Relative working conditions ratio" = relative_wc,
         "Working conditions" = working_conditions_mean,
         "Youth working poverty rate" = workingpov,
         "Youth TR underemployment rate" = underemp,
         "Share of youth in informal employment" = informal,
         "Share of youth in elementary occup." = elementary,
         "Education" = education_mean,
         "Share of youth with no secondary educ." = nosecondary,
         "Youth illiteracy rate" = literacy,
         "Harmonized test scores" = test_scores) %>% t()

female <- rank_generator(bygender = "Female", years = c(2010, 2020), impute = TRUE) %>%
  filter(!is.na(index_mean)) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select("YLILI score" = index_mean,
         "Transition" = transition_mean,
         "Share of youth NEET" = neet,
         "Youth skills mismatch rate" = mismatch,
         "Relative working conditions ratio" = relative_wc,
         "Working conditions" = working_conditions_mean,
         "Youth working poverty rate" = workingpov,
         "Youth TR underemployment rate" = underemp,
         "Share of youth in informal employment" = informal,
         "Share of youth in elementary occup." = elementary,
         "Education" = education_mean,
         "Share of youth with no secondary educ." = nosecondary,
         "Youth illiteracy rate" = literacy,
         "Harmonized test scores" = test_scores) %>% t()

gender_comp <- as.data.frame(cbind(male, female)) %>% mutate("$\\Delta$" = V1-V2) %>% rename("Male" = V1, "Female" = V2)
print(xtable(gender_comp), sanitize.colnames.function = identity)





