# Package names
packages <- c("tidyverse", "here", "ggrepel")

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
  ylim(50,90) +
  ggsave(here("male_vs_female.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = transition_mean_male, y = transition_mean_female, label = country_code)) +
  geom_point(size = 2) +
  geom_abline(slope = 1) +
  ggtitle("Transition") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  xlim(20,100) +
  ylim(20,100) +
  ggsave(here("transition_genderdiff.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = working_conditions_mean_male, y = working_conditions_mean_female, label = country_code)) +
  geom_point(size=2) +
  geom_abline(slope = 1) +
  ggtitle("Working conditions") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  xlim(20,100) +
  ylim(20,100) +
  ggsave(here("workingcond_genderdiff.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = education_mean_male, y = education_mean_female, label = country_code)) +
  geom_point(size=2) +
  geom_abline(slope = 1) +
  ggtitle("Education") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  xlim(20,100) +
  ylim(20,100) +
  ggsave(here("education_genderdiff.png"), width = 20, height = 12, units = "cm")

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

comp <- full_join(comp, gindex, by = c("country" = "Economy"))


ggplot(comp, aes(x = CFR_score, y = index_mean_female, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Gender Quota Index vs. Female YLILI") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  xlim(25,100) +
  ylim(0,100)

ggplot(comp, aes(x = CFR_score, y = index_diff, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Gender quota Index vs. YLILI Gender Diff.") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  xlim(25,100) +
  ylim(-15,15)
  
  #ggsave(here("gender_diff_vs_CFR_score.png"), width = 20, height = 12, units = "cm")


ggplot(comp, aes(x = CFR_institutions, y = index_diff, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Gender quota Index vs. YLILI Gender Diff.") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  geom_smooth(method = lm,  se = FALSE) +
  xlim(25,100) +
  ylim(-15,15)

#ggsave(here("gender_diff_vs_CFR_institutions.png"), width = 20, height = 12, units = "cm")

ggplot(comp, aes(x = CFR_work_incentives, y = index_diff, label = country_code)) +
  geom_point(size=2) +
  ggtitle("Gender quota Index vs. YLILI Gender Diff.") +
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

                     