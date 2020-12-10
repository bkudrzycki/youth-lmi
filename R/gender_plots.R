library(tidyverse)
library(here)
library(ggrepel)

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
