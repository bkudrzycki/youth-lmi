# Package names
packages <- c("tidyverse", "here", "corrplot", "stargazer", "data.table", "reshape2")

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
    filter(ref_area.label %in% countryLists()[[3]][[1]],
           sex.label == "Sex: Total",
           !is.na(obs_value)) %>%
    summarise(time, ref_area.label) %>% 
    mutate(dummy = 1,) %>% 
    acast(ref_area.label ~ time, value.var = "dummy") %>% 
    t() %>% as.data.frame() %>% rownames_to_column(var ="Year") %>% 
    rename("Cote d'Ivoire" = "Côte d'Ivoire")
}


years_list <- lapply(dfList, compress2) %>% 
  rbindlist(fill = TRUE) %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  t() %>% as.data.frame()

names(years_list) = years_list[1,]
years_list = years_list[-1,]

df <- years_list %>% count("num" = years_list$`2000`) %>% 
  rename("2000" = n) %>% 
  full_join(years_list %>% count("num" = years_list$`2001`) %>% rename("2001" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2002`) %>% rename("2002" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2003`) %>% rename("2003" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2004`) %>% rename("2004" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2005`) %>% rename("2005" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2006`) %>% rename("2006" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2007`) %>% rename("2007" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2008`) %>% rename("2008" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2009`) %>% rename("2009" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2010`) %>% rename("2010" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2011`) %>% rename("2011" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2012`) %>% rename("2012" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2013`) %>% rename("2013" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2014`) %>% rename("2014" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2015`) %>% rename("2015" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2016`) %>% rename("2016" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2017`) %>% rename("2017" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2018`) %>% rename("2018" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2019`) %>% rename("2019" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2020`) %>% rename("2020" = n), by = "num") %>% 
  t() %>% as.data.frame()
  
names(df) = df[1,]
df = df[-1,] %>% rownames_to_column(var ="Year") %>% arrange(desc(Year)) %>% mutate(`10` = NA)
df[is.na(df)] <- 0
print(xtable(df), include.rownames=FALSE)
