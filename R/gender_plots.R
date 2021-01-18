# Package names
packages <- c("tidyverse", "here", "ggrepel", "gtsummary", "readxl")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
## ---------------------------------------

devtools::load_all(here("lamadex"))
source(here("lamadex", "R", "source", "countryList.R"))
source(here("lamadex", "R", "source", "data_loader.R"))

total <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = TRUE) %>%
  arrange(desc(index_mean))

male <- rank_generator(dfList, country_lists[[3]], bygender = "Male", lastyear = 2010, impute = TRUE) %>%
  arrange(desc(index_mean))

female <- rank_generator(dfList, country_lists[[3]], bygender = "Female", lastyear = 2010, impute = TRUE) %>%
  arrange(desc(index_mean))

comp <- full_join(male, female, by = c("country", "country_code"), suffix = c("_male", "_female"))

comp <- full_join(comp, total, by = c("country", "country_code"))

comp <- comp %>% 
  mutate(index_diff = index_mean_male-index_mean_female)

ggplot(comp, aes(x = index_mean_male, y = index_mean_female, label = country_code)) +
  geom_point() +
  geom_abline(slope = 1) +
  xlab("Male YLILI") +
  ylab("Female YLILI") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  xlim(50,90) +
  ylim(50,90) 
  #ggsave(here("male_vs_female.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = transition_mean_male, y = transition_mean_female, label = country_code)) +
  geom_point(size = 2) +
  geom_abline(slope = 1) +
  ggtitle("Transition") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100) 
  #ggsave(here("transition_genderdiff.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = working_conditions_mean_male, y = working_conditions_mean_female, label = country_code)) +
  geom_point(size=2) +
  geom_abline(slope = 1) +
  ggtitle("Working conditions") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100) 
  #ggsave(here("workingcond_genderdiff.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = education_mean_male, y = education_mean_female, label = country_code)) +
  geom_point(size=2) +
  geom_abline(slope = 1) +
  ggtitle("Education") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100) 
  #ggsave(here("education_genderdiff.png"), width = 20, height = 12, units = "cm")

gindex <- read_excel(here("data", "CFR_index.xlsx")) %>% 
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


ggplot(comp, aes(x = CFR_score, y = index_mean_female, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Workplace Equality Index vs. Female YLILI") +
  xlab("CFR Score") +
  ylab("Female YLILI") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  xlim(25,100) +
  ylim(0,100)

summary(lm(formula = index_mean_female ~ CFR_score, data = comp))

ggplot(comp, aes(x = CFR_score, y = index_diff, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Workplace Equality Index vs. YLILI Gender Diff.") +
  xlab("CFR Score") +
  ylab("Gender difference in YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  xlim(25,100) +
  ylim(-15,15)
  
  #ggsave(here("gender_diff_vs_CFR_score.png"), width = 20, height = 12, units = "cm")
summary(lm(formula = index_diff ~ CFR_score, data = comp))

ggplot(comp, aes(x = CFR_institutions, y = index_diff, label = country_code)) +
  geom_point(size=2) +
  ggtitle("CFR Institutions Score vs. YLILI Gender Diff.") +
  xlab("CFR Institutions Score") +
  ylab("Gender difference in YLILI Score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  xlim(25,100) +
  ylim(-15,15)

#ggsave(here("gender_diff_vs_CFR_institutions.png"), width = 20, height = 12, units = "cm")


ggplot(comp, aes(x = CFR_work_incentives, y = index_diff, label = country_code)) +
  geom_point(size=2) +
  ggtitle("CFR Work Incentives Score vs. YLILI Gender Diff.") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  xlim(25,100) +
  ylim(-15,15)

comp %>% dplyr::select(index_mean_female, CFR_work_incentives) %>% 
  filter(!is.na(index_mean_female)) %>% 
  tbl_summary(by = "CFR_work_incentives",
              statistic = list(all_continuous() ~ "{mean} ({sd})"))

#no differences in either gender_diff or female index score across work incentives categories


## USING QUOTAS DATA

quotas <- read_csv(here("data", "quotas.csv"))

quotas$Country <- quotas$Country %>% ## fix country names to match ILOSTAT for joining
  recode("State of Palestine" = "Occupied Palestinian Territory",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Republic of The Congo (Brazzaville)" = "Congo",
         "Cabo Verde" = "Cape Verde")

comp <- left_join(comp, quotas, by = c("country" = "Country"))


comp %>% dplyr::select(contains("female"), index_diff, "Parliament type") %>% 
  tbl_summary(by = "Parliament type",
              statistic = list(all_continuous() ~ "{mean} [{median}] ({sd})"),
              missing = "no") %>% 
  add_p()

comp %>% dplyr::select(contains("female"), index_diff, "Voluntary political party quotas") %>% 
  tbl_summary(by = "Voluntary political party quotas",
              statistic = list(all_continuous() ~ "{mean} [{median}] ({sd})"),
              missing = "no") %>% 
  add_p()

comp %>% dplyr::select(contains("female"), index_diff, "Single/Lower House > Quota type") %>% 
  tbl_summary(by = "Single/Lower House > Quota type",
              statistic = list(all_continuous() ~ "{mean} [{median}] ({sd})"),
              missing = "no") %>% 
  add_p()

comp %>% dplyr::select(contains("female"), index_diff, "Single/Lower House > Constitutional quota details") %>% 
  tbl_summary(by = "Single/Lower House > Constitutional quota details",
              statistic = list(all_continuous() ~ "{mean} [{median}] ({sd})"),
              missing = "no") %>% 
  add_p()

comp %>% dplyr::select(contains("female"), index_diff, "Single/Lower House > Electoral law quota details") %>% 
  tbl_summary(by = "Single/Lower House > Electoral law quota details",
              statistic = list(all_continuous() ~ "{mean} [{median}] ({sd})"),
              missing = "no") %>% 
  add_p()

#no differences in either gender_diff or female index scores across different types of gender quota legislation in the political sphere
            