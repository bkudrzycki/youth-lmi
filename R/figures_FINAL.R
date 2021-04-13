# Package names
packages <- c("tidyverse", "here", "ggrepel", "gtsummary", "readxl")

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

all_countries <- countryLists()[[9]] %>% 
  mutate(inc_level = ifelse(ref_area.label %in% c(countryLists()[[1]]$ref_area.label, countryLists()[[2]]$ref_area.label), "LIC/LMIC", "HIC/HMIC"))

## UNEMPLOYMENT VS INFORMALITY RATE
unemp_r <- read.csv(here("data", "raw", "unemployment_rate_sex_age_ilostat.csv")) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(ref_area.label) %>%
  top_n(1, time) %>% select(ref_area.label, unemp_r = obs_value)

inform_r <- informal %>% select(ref_area.label, inform_r = "Sex: Total") %>% filter(ref_area.label != "Moldova, Republic of",
                                                                                    ref_area.label != "Ukraine",
                                                                                    ref_area.label != "Mozambique")

pop <- read.csv(here("data","raw","population_age_ilostat.csv")) %>%
  filter(time == 2019,
         sex.label == "Sex: Total") %>% 
  pivot_wider(id_cols = c(ref_area.label, classif1.label, obs_value), names_from = classif1.label, values_from = obs_value)

pop <- pop %>% 
  rename("total_pop" = `Age (Aggregate bands): Total`,
         "youth_pop" = `Age (Youth, adults): 15-24`,
         "adult_pop" = `Age (Youth, adults): 25+`)


df <- left_join(all_countries, unemp_r) %>% left_join(., inform_r) %>% left_join(., pop)

ggplot(df, aes(x = inform_r, y = unemp_r)) +
  geom_point(aes(shape = inc_level, color = inc_level)) +
  xlab("Youth informality rate") +
  ylab("Youth unemployment rate") +
  theme_minimal() +
  geom_smooth(method = "lm", aes(color = inc_level, linetype = inc_level), se = F, size=0.5) +
  scale_linetype_manual(name = "World Bank Income Classification",
                        values = c("LIC/LMIC" = 1, "HIC/HMIC" = 2)) +
  scale_colour_manual(name = "World Bank Income Classification", values = c("gray", "black")) +
  scale_shape_manual(name = "World Bank Income Classification", values=c(17, 16)) +
  #geom_text_repel(aes(label=country_code),size = 3)
  labs(linetype="World Bank Income Classification") +
  theme(legend.position="top") +
  ggsave(here("figures", "inf_unemp.png"), width = 20, height = 12, units = "cm")



## YLILI vs youth unemployment rate

df <- rank %>% left_join(unemp_r, by = c("country" = "ref_area.label"))

ggplot(df, aes(x = unemp_r, y = index_mean, label = country_code)) +
  geom_point(size = 2) +
  xlab("Youth unemployment rate") +
  ylab("YLILI score") +
  theme_minimal() +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  geom_text_repel(aes(label=country_code),size = 3) + 
  ggsave(here("figures", "index_vs_unemp.png"), width = 20, height = 12, units = "cm")

## YLILI vs GDP

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

df <- rank %>% left_join(gdp)

ggplot(df, aes(x = gdp, y = index_mean, label = country_code)) +
  geom_point(size=2) +
  #ggtitle("Per capita GDP vs YLILI") +
  xlab("GDP per capita (PPP, current international $)") +
  ylab("YLILI score") +
  theme_minimal() +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  geom_text_repel(aes(label=country_code),size = 3) + 
  ggsave(here("figures", "index_vs_gdp.png"), width = 20, height = 12, units = "cm")


