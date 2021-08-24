# Package names
packages <- c("tidyverse", "here", "ggrepel", "gtsummary", "readxl", "corrplot", "viridis", "fmsb", "GISTools", "ggcorrplot")

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
  mutate(inc_level = ifelse(ref_area.label %in% c(countryLists()[[1]]$ref_area.label, countryLists()[[2]]$ref_area.label), "LIC/LMIC", "HIC/UMIC"))

## load regions

regions <- read.csv(here("data", "country_regions.csv")) %>% 
  dplyr::select("country" = "Country.or.Area",
                "Region Name" = "Region.Name",
                "Sub-region Name" = "Sub.region.Name",
                "Intermediate Region Name" = "Intermediate.Region.Name")

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


## UNEMPLOYMENT VS INFORMALITY RATE
unemp_r <- read.csv(here("data", "raw", "unemployment_rate_sex_age_ilostat.csv")) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(ref_area.label) %>%
  top_n(1, time) %>% dplyr::select(ref_area.label, unemp_r = obs_value)

inform_r <- informal %>% dplyr::select(ref_area.label, inform_r = "Sex: Total") %>% filter(ref_area.label != "Moldova, Republic of",
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
                        values = c("LIC/LMIC" = 1, "HIC/UMIC" = 2)) +
  scale_colour_manual(name = "World Bank Income Classification", values = c("gray", "black")) +
  scale_shape_manual(name = "World Bank Income Classification", values=c(17, 16)) +
  #geom_text_repel(aes(label=country_code),size = 3)
  labs(linetype="World Bank Income Classification") +
  theme(legend.position="top") +
  ggsave(here("figures", "inf_unemp.png"), width = 20, height = 12, units = "cm")


## YLILI VS YOUTH UNEMPLOYMENT RATE
df <- rank %>% left_join(unemp_r, by = c("country" = "ref_area.label")) %>% 
  left_join(regions) %>% 
  filter(`Region Name` != "Oceania")

ggplot(df, aes(x = unemp_r, y = index_mean, label = country_code)) +
  geom_point(size = 2, aes(color = `Region Name`)) +
  xlab("Youth unemployment rate") +
  ylab("YLILI score") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  geom_text_repel(aes(label=country_code),size = 3) + 
  ggsave(here("figures", "index_vs_unemp.png"), width = 20, height = 10, units = "cm")

## YLILI vs GDP
gdp <- read.csv(here("data", "raw", "gdp_PPP_percap_worldbank.csv")) %>%
  rename("country" = Country.Name) %>%
  dplyr::select(country, "gdp"=X2018)

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

df <- rank %>% left_join(gdp) %>%
  left_join(regions) %>% 
  filter(`Region Name` != "Oceania")

ggplot(df, aes(x = log(gdp), y = index_mean, label = country_code)) +
  geom_point(size=2, aes(color = `Region Name`)) +
  #ggtitle("Per capita GDP vs YLILI") +
  xlab("log GDP per capita (PPP, current international $)") +
  ylab("YLILI score") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  geom_text_repel(aes(label=country_code),size = 3) + 
  ggsave(here("figures", "index_vs_gdp.png"), width = 20, height = 10, units = "cm")

## RAW VS IMPUTED

raw <- rank_generator(impute = FALSE) %>% dplyr::select(country, "index_mean_raw" = index_mean)
df <- left_join(df, raw, by = "country")

ggplot(df, aes(x = index_mean, y = index_mean_raw, label = country_code)) +
  geom_point(aes(color = `Region Name`)) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  xlab("YLILI score - imputed data") +
  ylab("YLILI score - raw data") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_text_repel(aes(label=country_code),size = 3) +
  ggsave(here("figures", "imputed_vs_raw.png"), width = 20, height = 10, units = "cm")

## ARITHMETIC VS GEOMETRIC MEANS

ggplot(df, aes(x = index_mean, y = index_geom, label = country_code)) +
  geom_point(aes(color = `Region Name`)) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  xlab("YLILI score - arithmetic mean") +
  ylab("YLILI score - geometric mean") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_text_repel(aes(label=country_code),size = 3) +
  ggsave(here("figures", "arithmetic_vs_geom.png"), width = 20, height = 10, units = "cm")

## GENDER PLOTS

male <- rank_generator(bygender = "Male", years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean))

female <- rank_generator(bygender = "Female", years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean))

comp <- full_join(male, female, by = c("country", "country_code"), suffix = c("_male", "_female")) %>% 
  full_join(., rank, by = c("country", "country_code")) %>% 
  mutate(index_diff = index_mean_male-index_mean_female) %>% 
  left_join(regions) %>% 
  filter(`Region Name` != "Oceania")

ggplot(comp, aes(x = index_mean_male, y = index_mean_female, label = country_code)) +
  geom_point(aes(color = `Region Name`)) +
  geom_abline(slope = 1) +
  xlab("Male YLILI") +
  ylab("Female YLILI") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  xlim(50,90) +
  ylim(50,90) 
ggsave(here("figures", "male_vs_female.png"), width = 20, height = 10, units = "cm")

ggplot(comp, aes(x = transition_mean_male, y = transition_mean_female, label = country_code)) +
  geom_point(size = 2, aes(color = `Region Name`)) +
  geom_abline(slope = 1) +
  ggtitle("Transition") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  guides(color=FALSE) +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100) 
ggsave(here("figures", "transition_genderdiff.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = working_conditions_mean_male, y = working_conditions_mean_female, label = country_code)) +
  geom_point(size=2, aes(color = `Region Name`)) +
  geom_abline(slope = 1) +
  ggtitle("Working conditions") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  guides(color=FALSE) +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100) 
ggsave(here("figures", "workingcond_genderdiff.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = education_mean_male, y = education_mean_female, label = country_code)) +
  geom_point(size=2, aes(color = `Region Name`)) +
  geom_abline(slope = 1) +
  ggtitle("Education") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  guides(color=FALSE) +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100) 
ggsave(here("figures", "education_genderdiff.png"), width = 20, height = 12, units = "cm")

## CORRELATION PLOT

cormat_indices <- rank %>% 
  rename("work cond. ratio" = relative_wc,
         "test scores" = test_scores,
         "dim: transition" = transition_mean,
         "dim: work cond." = working_conditions_mean,
         "dim: education" = education_mean,
         "YLILI" = index_mean) %>% 
  dplyr::select(c(3:15,19)) %>% 
  filter(!is.na(`YLILI`)) %>% 
  as.matrix()

cormat_indices <- cormat_indices %>% 
  cor()

cormat_indices_spearman <- cormat_indices %>% 
  cor(method ="pearson")

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(cormat_indices)

ggcorrplot(cormat_indices, type = "upper",
           outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           colors = c("#000000", "#7E7E7E", "#FFFFFF"),
           show.diag = TRUE,
           lab = TRUE) +
  ggsave(here("figures", "correlation_matrix_labels.png"), width = 30, height = 18, units = "cm")

ggcorrplot(cormat_indices, type = "upper",
           outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           colors = c("#000000", "#7E7E7E", "#FFFFFF"),
           p.mat = p.mat,
           show.diag = TRUE,
           sig.level = .05) +
  ggsave(here("figures", "correlation_matrix.png"), width = 30, height = 18, units = "cm")

## SPIDER CHARTS

# generating function

spider <- function(x, region) {
  x <- x %>% 
    filter(region2 == region)
  
  x <- x[,-1]
  
  x <- rbind(rep(100,5) , rep(0,5) , x)
  
  # function for adding line breaks in labels where needed
  addline_format <- function(x,...){
    gsub('\\s','\n',x)
  }
  png(filename = paste0("figures/", region, "_spider.png"),
      width = 425, 
      height = 400)
  
  par(omi = c(.5,.5,.5,.5), mar = c(1,.5,1,.5), xpd = FALSE)
  
  radarchart( x , axistype=6,
              maxmin = TRUE,
              #custom polygon
              pcol="black" , pfcol=gray(.5, .5), plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey",
              #custom labels
              vlcex=0.8 ,
              vlabels = addline_format(c("Overall YLILI", "NEET", "Working Conditions Ratio", "Mismatch", "Working Poverty", "Under- employment",  "Informality",  "Elementary", "No Secondary", "Literacy", "Test Scores")),
              title = region)
  
  dev.off()
}

df <- left_join(rank, regions, by = c("country")) %>% 
  filter(!is.na(index_mean))

df <- df %>% 
  mutate(region2 = ifelse(`Region Name` == "Asia", "Asia", `Sub-region Name`))

x <- df %>% 
  dplyr::select(country, region2, "Overall YLILI" = index_mean, "NEET" = neet, "Working\nConditions\nRatio" = relative_wc, "Mismatch" = mismatch, "Working\nPoverty" = workingpov, "Under-\nemployment" = underemp, "Informality" = informal, "Elementary\nOccupations" = elementary, "No\nSecondary\nSchooling" = nosecondary, "Literacy" = literacy, "Test Scores" = test_scores) %>% 
  group_by(region2) %>% 
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
  as.data.frame()

spider(x, "Asia")
spider(x, "Eastern Europe")
spider(x, "Latin America")
spider(x, "Northern Africa")
spider(x, "Sub-Saharan Africa")

x <- df %>% 
  filter(`Sub-region Name` == "Sub-Saharan Africa") %>% 
  dplyr::select(country, `Intermediate Region Name` , "Overall YLILI" = index_mean, "NEET" = neet, "Working\nConditions\nRatio" = relative_wc, "Mismatch" = mismatch, "Working\nPoverty" = workingpov, "Under-\nemployment" = underemp, "Informality" = informal, "Elementary\nOccupations" = elementary, "No\nSecondary\nSchooling" = nosecondary, "Literacy" = literacy, "Test Scores" = test_scores) %>% 
  rename(region2 = `Intermediate Region Name`) %>% 
  group_by(region2) %>% 
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
  as.data.frame()

spider(x, "Eastern Africa")
spider(x, "Middle Africa")
spider(x, "Southern Africa")
spider(x, "Western Africa")