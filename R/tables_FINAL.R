# Package names
packages <- c("tidyverse", "here", "corrplot", "stargazer")

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
  select(c(3:12,19)) %>% 
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
#dev_countries <- countryLists()[[3]][[1]]

compress <- function (x) {
  x %>%
    as_tibble(.) %>%
    filter(ref_area.label %in% countryLists()[[3]][[1]],
           sex.label == "Sex: Total",
           !is.na(obs_value)) %>%
    group_by(time) %>% 
    summarise(ref_area.label) %>% 
    summarise(n_distinct(time))
}

years_list <- lapply(dfList, compress) %>% 
  reduce(full_join, by = "time", accumulate == TRUE) 

colnames(years_list) <- c("time",  "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores")


dfList <- compute_indicators()

compress2 <- function (x) {
  x %>%
    as_tibble(.) %>%
    filter(ref_area.label %in% countryLists()[[3]][[1]],
           sex.label == "Sex: Total",
           !is.na(obs_value)) %>%
    group_by(time) %>% 
    summarise(length(unique(ref_area.label)))
    
}

years_list <- lapply(dfList, compress2) %>% 
  reduce(full_join, by = c("time"), accumulate == TRUE) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  filter()

colnames(years_list) <- c("time",  "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores")


df <- literacy %>% as.tibble %>% 
  filter(ref_area.label %in% countryLists()[[3]][[1]],
         sex.label == "Sex: Total",
         !is.na(obs_value)) %>%
  group_by(time) %>% 
  summarise(length(unique(ref_area.label)))

years_list <- lapply(dfList, compress2) %>% 
  reduce(full_join, by = c("ref_area.label", "time"), accumulate == TRUE) 
  
       