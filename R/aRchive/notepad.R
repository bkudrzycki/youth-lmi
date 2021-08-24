library(tidyverse)
library(here)
library(ggrepel)

setwd("~/polybox/Youth Employment/1b Index/youth-lmi")

devtools::load_all(here("lamadex"))
source(here("R", "countryList.R"))

rank <- rank_generator(bygender = "Total", countries = "dev", years = c(2010, 2020), impute = TRUE) %>%
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

gdp <- read.csv("./data/raw/gdp_PPP_percap_worldbank.csv") %>%
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

total <- left_join(total, gdp, by = "country")

#----------

total <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = FALSE) %>%
  arrange(desc(index_mean))

male <- rank_generator(dfList, country_lists[[3]], bygender = "Male", lastyear = 2010, impute = FALSE) %>%
  arrange(desc(index_mean))

female <- rank_generator(dfList, country_lists[[3]], bygender = "Female", lastyear = 2010, impute = FALSE) %>%
  arrange(desc(index_mean))

#---------- scatterplots for gender analysis

comp <- full_join(male, female, by = c("country", "country_code"), suffix = c("_male", "_female"))

comp <- full_join(comp, total, by = c("country", "country_code"))

write.csv(comp, here("R/complete_raw.csv"))

ggplot(comp, aes(x = index_mean_male, y = index_mean_female, label = country_code)) +
  geom_point() +
  geom_abline(slope = 1) +
  xlab("Male") +
  ylab("Female") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

comp <- full_join(comp, gdp, by = "country")

comp <- comp %>% 
  mutate(index_diff = index_mean_male-index_mean_female)

ggplot(comp, aes(x = gdp, y = index_diff, label = country_code)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("GDP per capita") +
  ylab("YLILI Gender Gap") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3) +
  scale_x_continuous(limits = c(0, 20000))
  
comp2 <- pivot_longer(comp, cols = c("index_mean_male", "index_mean_female"), names_prefix = "index_mean_", values_to = "gender_score", names_to = "Gender")

ggplot(comp2, aes(x = gdp, y = gender_score, label = country_code, color = Gender)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  xlab("GDP per capita") +
  ylab("YLILI Score") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 20000))

rm(comp2)

ggplot(comp, aes(x = transition_mean_male, y = transition_mean_female, label = country_code)) +
  geom_point() +
  geom_abline(slope = 1) +
  xlab("Male transition score") +
  ylab("Female transition score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

ggplot(comp, aes(x = working_conditions_mean_male, y = working_conditions_mean_female, label = country_code)) +
  geom_point() +
  geom_abline(slope = 1) +
  xlab("Male working conditions score") +
  ylab("Female working conditions score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

ggplot(comp, aes(x = education_mean_male, y = education_mean_female, label = country_code)) +
  geom_point() +
  geom_abline(slope = 1) +
  xlab("Male education score") +
  ylab("Female education score") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

comp <- comp %>% 
  mutate(diff_transition = transition_mean_male-transition_mean_female,
         diff_working_conditions = working_conditions_mean_male-working_conditions_mean_female,
         diff_education = education_mean_male-education_mean_female)

ggplot(comp, aes(x = index_mean, y = diff_transition, label = country_code)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  geom_hline(yintercept=0) +
  xlab("YLILI score") +
  ylab("Gender gap: transition") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

ggplot(comp, aes(x = index_mean, y = diff_working_conditions, label = country_code)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  geom_hline(yintercept=0) +
  xlab("YLILI score") +
  ylab("Gender gap: working conditions") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

ggplot(comp, aes(x = index_mean, y = diff_education, label = country_code)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  geom_hline(yintercept=0) +
  xlab("YLILI score") +
  ylab("Gender gap: education") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

ggplot(comp, aes(x = index_mean, y = index_geom, label = country_code)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color = "black", size = .5) +
  xlab("Arithmetic mean") +
  ylab("Geometric mean") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)
  #+ ggsave(here("arithmetic_vs_geom.png"), width = 20, height = 12, units = "cm")

#---------- correlation matrices

library(corrplot)
library(stargazer)

cormat_indices <- rank %>% 
  rename("work cond. ratio" = relative_wc,
         "test scores" = test_scores,
         "dim: transition" = transition_mean,
         "dim: work cond." = working_conditions_mean,
         "dim: education" = education_mean,
         "YLILI" = index_mean) %>% 
  select(c(2:11,13:15,19)) %>% 
  filter(!is.na(`YLILI`)) %>% 
  as.matrix()

cormat_indices <- cormat_indices %>% 
  cor()

cormat_indices_spearman <- cormat_indices %>% 
  cor(method ="spearman")

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

corrplot(cormat_indices, method="circle", type="upper", p.mat = p.mat, sig.level = 0.01, tl.col="black", tl.srt=45)

lower <- cormat_indices %>% round(3)
lower[lower.tri(cormat_indices, diag=TRUE)]<-""
lower <- as.matrix(lower)

cormat_indices <- rank %>% 
  rename("YLILI score" = index_mean,
         "work cond. ratio" = relative_wc,
         "test scores" = test_scores) %>% 
  select(c(2:11,19)) %>% 
  filter(!is.na(`YLILI score`)) %>% 
  as.matrix()

lower <- cormat_indices %>% round(3)
lower[lower.tri(cormat_indices, diag=TRUE)]<-""
lower <- as.matrix(lower)

stargazer(lower,align = T)

#----------

plot <- left_join(x = gdp, y = informal, by = c("country" = "ref_area.label"))
  
country_list <- country_lists[[3]]
plot <- left_join(country_list, plot, by = c("ref_area.label" = "country"))

ggplot(plot, aes(x = gdp, y = `Sex: Total`)) +
  geom_point() +
  xlab("GDP per capita (PPP, current international $)") +
  ylab("Youth informal employment rate") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)

plot <- left_join(gdp, unemployment_rate, by = c("country" = "ref_area.label")) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(country) %>%
  top_n(1, time)

plot <- left_join(country_list, plot, by = c("ref_area.label" = "country"))
  
ggplot(plot, aes(x = gdp, y = obs_value)) +
  geom_point() +
  xlab("GDP per capita (PPP, current international $)") +
  ylab("Youth informal employment rate") +
  theme_minimal() +
  geom_text_repel(aes(label=country_code),size = 3)


plot <- left_join(plot, informal, by = c("ref_area.label")) %>% 
  pivot_longer(cols = c("Sex: Total", obs_value),
               names_to = "indicator",
               values_to = "obs_value",
               names_ptypes = list(sex.label = factor(levels = c("informality","unemployment"))))
             
ggplot(plot, aes(x = gdp, y = obs_value)) +
  geom_point(aes(color = indicator)) +
  xlab("GDP per capita (PPP, current international $)") +
  ylab("Percent") +
  theme_minimal() +
  theme(legend.position="top") +
  geom_text_repel(aes(label=country_code),size = 3) +
  scale_color_discrete(name="",
                      labels=c("Youth unemployment rate", "Youth informal employment rate"))

index <- left_join(rank, gdp, by = "country")

unemployment_rate <- unemployment_rate %>%
  filter(classif1.label == "Age (Youth, adults): 15-24")

unemployment_rate <- filter_helper(unemployment_rate, bygender = "Total", lastyear = 2010) %>%
  rename("country" = ref_area.label)

index <- left_join(index, unemployment_rate, by = "country")


# plot for Isabel

all_countries <- country_lists[[9]] %>% 
  mutate(inc_level = ifelse(ref_area.label %in% c(country_lists[[1]]$ref_area.label, country_lists[[2]]$ref_area.label), "LIC/LMIC", "HIC/HMIC"))
  
unemp_r <- unemployment_rate %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(ref_area.label) %>%
  top_n(1, time) %>% select(ref_area.label, unemp_r = obs_value)

inform_r <-informal %>% select(ref_area.label, inform_r = "Sex: Total")

df <- left_join(all_countries, unemp_r) %>% left_join(., inform_r)

ggplot(df, aes(x = inform_r, y = unemp_r)) +
  geom_point(aes(shape = inc_level, color = inc_level)) +
  xlab("Youth Informality Rate") +
  ylab("Youth Unemployment Rate") +
  theme_minimal() +
  geom_smooth(method = "lm", aes(color = inc_level, linetype = inc_level, weight = pop), se = F, size=0.75) +
  scale_colour_manual(name = "Income Level", values = c("gray", "black")) +
  scale_shape_manual(name = "Income Level", values=c(17, 16)) +
  labs(linetype="Income Level") +
  theme(legend.position="bottom") +
  ggsave(here("inf_unemp.png"), width = 20, height = 12, units = "cm")
  
  #geom_text_repel(aes(label=country_code),size = 3)



# some plots
plot(rank$index_mean, rank$index_geom)
text(rank$index_mean, rank$index_geom, labels=rank$country_code, cex= .7, pos = 3)


# hist(log(elementary$obs_value))
# hist(log(neet$obs_value))
# hist(log(relative_unemp$obs_value))
# hist(log(mismatch$obs_value))
# hist(log(underemp$obs_value))
# hist(log(vulnerable$obs_value))
# hist(log(saff$obs_value))
# hist(log(informal$obs_value))
# hist(log(workingpov$obs_value))
# hist(log(test_scores$obs_value))
# hist(log(literacy$obs_value))
# hist(log(nosecondary$obs_value))

# plot(rank$index_mean, rank$index_geom)
# text(rank$index_mean, rank$index_geom, labels=rank$country_code, cex= .7, pos = 3)
#
# plot(rank$transition_mean, rank$index_mean)
# text(rank$transition_mean, rank$index_mean, labels=rank$country_code, cex= .7, pos = 3)
#
# plot(rank$working_conditions_mean, rank$index_mean)
# text(rank$working_conditions_mean, rank$index_mean, labels=rank$country_code, cex= .7, pos = 3)
#
# plot(rank$education_mean, rank$index_mean)
# text(rank$education_mean, rank$index_mean, labels=rank$country_code, cex= .7, pos = 3) ##education correlates with overall score
#
# plot(rank$elementary, rank$saff)
# text(rank$elementary, rank$saff, labels=rank$country_code, cex= .7, pos = 3)

plot(total$index_mean, female$index_mean)
text(total$index_mean, female$index_mean, labels=total$country_code, cex= .7, pos = 3)
lines(x = c(0,100), y = c(0,100))

plot(total$index_mean, total$index_geom)
text(total$index_mean, total$index_geom, labels=total$country_code, cex= .7, pos = 3)

#plot(total$gdp, total$index_mean)
#text(total$gdp, total$index_mean, labels=total$country_code, cex= .7, pos = 3)
#abline(lm(total$index_mean ~ total$gdp))

df <- left_join(male, female, by = c("country"), suffix = c("_male", "_female"))
df <- left_join(total, df, by = c("country"))

total <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = FALSE)
total <- total %>%
  arrange(desc(index_mean))

female <- rank_generator(dfList, country_lists[[3]], bygender = "Female", lastyear = 2010, impute = FALSE)
female <- female %>%
  arrange(desc(index_mean))

male <- rank_generator(dfList, country_lists[[3]], bygender = "Male", lastyear = 2010, impute = FALSE)
male <- male %>%
  arrange(desc(index_mean))

df <- left_join(male, female, by = c("country", "country_code"), suffix = c("_male", "_female"))
df <- left_join(total, df, by = c("country", "country_code"))

df %>%
  ggplot(aes(x = index_mean_female, y = index_mean_male, label = country_code)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm') +
  geom_abline(intercept = 0, slope = 1) +
  xlim(35, 84) +
  ylim(35, 84) +
  geom_text_repel(aes(label=country_code), size = 3) +
  ggtitle("MALE v FEMALE: ARITHMETIC MEAN (raw)")

df %>%
  ggplot(aes(x = index_geom_female, y = index_geom_male, label = country_code)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm') +
  geom_abline(intercept = 0, slope = 1) +
  xlim(39, 78) +
  ylim(39, 78) +
  geom_text_repel(aes(label=country_code), size = 3) +
  ggtitle("MALE v FEMALE: GEOMETRIC MEAN (raw)")


total_imp <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = TRUE)
total_imp <- total_imp %>%
  arrange(desc(index_mean))

female_imp <- rank_generator(dfList, country_lists[[3]], bygender = "Female", lastyear = 2010, impute = TRUE)
female_imp <- female_imp %>%
  arrange(desc(index_mean))

male_imp <- rank_generator(dfList, country_lists[[3]], bygender = "Male", lastyear = 2010, impute = TRUE)
male_imp <- male_imp %>%
  arrange(desc(index_mean))

df2 <- left_join(male_imp, female_imp, by = c("country", "country_code"), suffix = c("_male", "_female"))
df2 <- left_join(total_imp, df2, by = c("country", "country_code"))

df2 %>%
  ggplot(aes(x = index_mean_female, y = index_mean_male, label = country_code)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm') +
  geom_abline(intercept = 0, slope = 1) +
  xlim(40, 84) +
  ylim(40, 84) +
  geom_text_repel(aes(label=country_code), size = 3) +
  ggtitle("MALE v FEMALE: ARITHMETIC MEAN (imputed)")

df2 %>%
  ggplot(aes(x = index_geom_female, y = index_geom_male, label = country_code)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm') +
  geom_abline(intercept = 0, slope = 1) +
  xlim(40, 84) +
  ylim(40, 84) +
  geom_text_repel(aes(label=country_code), size = 3) +
  ggtitle("MALE v FEMALE: GEOMETRIC MEAN (imputed)")

df3 <- left_join(total, total_imp, by = c("country", "country_code"), suffix = c("_raw", "_impute"))

df3 %>%
  ggplot(aes(x = index_mean_raw, y = index_mean_impute, label = country_code)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(35, 84) +
  ylim(35, 84) +
  geom_text_repel(aes(label=country_code), size = 3)

df3 %>%
  ggplot(aes(x = index_geom_raw, y = index_geom_impute, label = country_code)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(35, 84) +
  ylim(35, 84) +
  geom_text_repel(aes(label=country_code), size = 3)

total %>%
  ggplot(aes(x = index_mean, y = index_geom, label = country_code)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm') +
  xlim(55, 80) +
  ylim(35, 84) +
  geom_text_repel(aes(label=country_code), size = 3) +
  ggtitle("ARITHMETIC v GEOMETRIC")

female %>%
  ggplot(aes(x = index_mean, y = index_geom, label = country_code)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm') +
  xlim(55, 80) +
  ylim(35, 84) +
  geom_text_repel(aes(label=country_code), size = 3) +
  ggtitle("ARITHMETIC v GEOMETRIC: FEMALES")

male %>%
  ggplot(aes(x = index_mean, y = index_geom, label = country_code)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm') +
  xlim(55, 80) +
  ylim(35, 84) +
  geom_text_repel(aes(label=country_code), size = 3) +
  ggtitle("ARITHMETIC v GEOMETRIC: MALES")

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