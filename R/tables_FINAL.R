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
