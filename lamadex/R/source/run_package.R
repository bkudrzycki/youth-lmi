library(tidyverse)
library(here)
set_here(path='/Users/kudrzycb/polybox/Youth Employment/1b Index/youth-lmi')
#library(lamadex, lib("./lamadex/lamadex")) ##load lamadex package
devtools::load_all(here())


#----------
# Load data
## read in country lists according to the World Bank Lending Groups classification
source(here("R", "source", "countryList.R")) # stored lists: [1]: LICs (30) [2]: LMICs (46) [3]: LICs+LMICs (76) [4]: All countries (244)

## read in dataframes from raw csv files
source(here("R", "source", "data_loader.R"))

#----------
# Run the package

rank <- rank_generator(dfList, country_lists[[3]], bygender = "Total", lastyear = 2010, impute = TRUE) %>%
  arrange(desc(index_mean))

#write.csv(rank, here("R/total_raw.csv"))

#----------
# Clean up
##rank <- rank %>%
##  filter(!is.na(index_mean)) ## remove unranked countries

## rm(country_lists, dfList, neet, unemployment_rate, employed, unemployed, working_pov, underemp, informal, status, occupation, education, literacy, test_scores)


