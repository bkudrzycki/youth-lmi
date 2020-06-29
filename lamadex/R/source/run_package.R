library(tidyverse)

rm(list=ls())

setwd("~/polybox/Youth Employment/1b Index/youth-lmi")

library(lamadex, lib("./lamadex/lamadex"))

#----------
# Load data
## read in country lists according to the World Bank Lending Groups classification
source("./lamadex/R/source/countryList.R") # stored lists: [1]: LICs (30) [2]: LMICs (46) [3]: LICs+LMICs (76) [4]: All countries (244)

## read in dataframes from raw csv files
source("./lamadex/R/source/data_loader.R")

#----------
# Run the package
rank <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = TRUE)
rank <- rank %>%
  arrange(desc(index_mean))

#----------
# Clean up
##rank <- rank %>%
##  filter(!is.na(index_mean)) ## remove unranked countries

## rm(country_lists, dfList, neet, unemployment_rate, employed, unemployed, working_pov, underemp, informal, status, occupation, education, literacy, test_scores)

#----------
## add gdp and unemployment columns
gdp <- read.csv("./data/raw/gdp_PPP_percap_worldbank.csv") %>%
  rename("country" = Country.Name) %>%
  select(country, "gdp"=X2018)

gdp$country <- gdp$country %>%
  recode("Vietnam" = "Viet Nam",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Congo, Dem. Rep." = "Congo, Democratic Republic of the",
         "Gambia, The" = "Gambia",
         "Tanzania" = "Tanzania, United Republic of",
         "Egypt, Arab Rep." = "Egypt",
         "Lao PDR" = "Lao People's Democratic Republic",
         "Kyrgyz Republic" = "Kyrgyzstan",
         "Moldova" = "Moldova, Republic of")

index <- left_join(rank, gdp, by = "country")

unemployment_rate <- unemployment_rate %>%
  filter(classif1.label == "Age (Youth, adults): 15-24")

unemployment_rate <- filter_helper(unemployment_rate, bygender = "Total", lastyear = 2010) %>%
  rename("country" = ref_area.label)

index <- left_join(index, unemployment_rate, by = "country")

write.csv(index, "/Users/kudrzycb/Desktop/index.csv")


# some plots
plot(rank$index_mean, rank$index_geom)
text(rank$index_mean, rank$index_geom, labels=rank$country_code, cex= .7, pos = 3)


plot(rank$vulnerable, rank$informal)
text(rank$vulnerable, rank$informal, labels=rank$country_code, cex= .7, pos = 3)
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

gdp <- read.csv("./data/raw/gdp_PPP_percap_worldbank.csv") %>%
  rename("country" = Country.Name) %>%
  select(country, "gdp"=X2018)

gdp$country <- gdp$country %>%
  recode("Vietnam" = "Viet Name",
         "Cote d'Ivoire" = "Côte d'Ivoire",
         "Congo, Dem. Rep." = "Congo, Democratic Republic of the",
         "Gambia, The" = "Gambia",
         "Tanzania" = "Tanzania, United Republic of",
         "Egypt, Arab Rep." = "Egypt",
         "Lao PDR" = "Lao People's Democratic Republic",
         "Kyrgyz Republic" = "Kyrgystan",
         "Moldova" = "Moldova, Republic of")
total <- left_join(total, gdp, by = "country")

plot(total$gdp, total$index_mean)
text(total$gdp, total$index_mean, labels=total$country_code, cex= .7, pos = 3)
abline(lm(total$index_mean ~ total$gdp))
