library(tidyverse)
library(here)
library(ggrepel)
library(stargazer)
library(xtable)
library(matrixStats)

devtools::load_all(here("lamadex"))

raw <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = FALSE) %>%
  arrange(desc(index_mean)) %>%
  filter(!is.na(index_mean)) %>% 
  select(country,
         "raw" = index_mean)

rank <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean)) %>% 
  mutate(rank = rank(-index_mean,na.last = "keep")) %>% 
  filter(!is.na(index_mean))

#remove one indicator at a time, recompile

#transition dimension

new_trans <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:4]))<2, rowMeans(.[3:4], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[5:8]))<3, rowMeans(.[5:8], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[9:11]))<2, rowMeans(.[9:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>% 
    select(c(!!varname))
}

df <- cbind(rank, new_trans(rank, "neet"))
df <- cbind(df, new_trans(rank, "relative_wc"))
df <- cbind(df, new_trans(rank, "mismatch"))

new_wc <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:5]))<2, rowMeans(.[3:5], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[6:8]))<3, rowMeans(.[6:8], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[9:11]))<2, rowMeans(.[9:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>%
    select(c(!!varname))
}

df <- cbind(df, new_trans(rank, "workingpov"))
df <- cbind(df, new_trans(rank, "underemp"))
df <- cbind(df, new_trans(rank, "informal"))
df <- cbind(df, new_trans(rank, "elementary"))

new_educ <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:5]))<2, rowMeans(.[3:5], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[6:9]))<3, rowMeans(.[6:9], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[10:11]))<2, rowMeans(.[10:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>% 
    select(c(!!varname))
}

df <- cbind(df, new_educ(rank, "nosecondary"))
df <- cbind(df, new_educ(rank, "literacy"))
df <- cbind(df, new_educ(rank, "test_scores"))

df <- df %>% select(-index_geom, index_geom)
df <- left_join(df, raw, by = "country")

#score correlations

scores <- df %>% 
  select(c("country", "index_mean", starts_with("no_"), "index_geom", "raw"))

pearson <- cor(as.matrix(scores[,-1]))[,1]
spearman <- cor(as.matrix(scores[,-1]), method = "spearman")[,1]

score_diffs <- abs(scores[2:ncol(scores)]-scores[,2]) %>%
  summarise_if(is.numeric, mean)

score_sd <- scores[2:ncol(scores)]-scores[,2]
  
score_sd <- score_sd %>% 
  summarise_if(is.numeric, sd)

##differences in rankings
to_rank <- function(x) {rank(-x)}

ranks <- df %>% 
  select(c("country", "index_mean", starts_with("no_"), "index_geom", "raw")) %>% 
  mutate_if(is.numeric, to_rank)

rank_diffs <- abs(ranks[2:ncol(ranks)]-ranks[,2]) %>% 
  summarise_if(is.numeric, mean)

max_rank_diffs <- abs(ranks[2:ncol(ranks)]-ranks[,2]) %>%
  summarise_if(is.numeric, max)
  
table <- rbind(pearson, score_diffs, score_sd, spearman, rank_diffs, max_rank_diffs)[,-1]

rownames(table) <- c("pearson", "score difference", "score diff sd", "spearman", "rank diff.", "max rank diff")

xtable(table, digits=3)

scores[,2:ncol(scores)] <- round(scores[,2:ncol(scores)],2)

for(i in 1:nrow(scores)) {
  for(j in 2:ncol(scores)) {
    scores[i,j] <- paste0(scores[i,j], " (", ranks[i,j], ")")
  }
}

stargazer(as.matrix(scores), summary = FALSE)

rank_sds <- cbind(country = ranks[,1], abs(ranks[2:ncol(ranks)]-ranks[,2]))

rank_sds <- rank_sds %>% 
  mutate(stdev = rowSds(as.matrix(.[3:14]))) %>% 
  select(c(country, stdev))

mean_ranks <- ranks %>% 
  mutate(mean_rank = rowMeans(ranks[,2:ncol(ranks)]),
         diff = abs(mean_rank - index_mean))

mean_ranks <- left_join(mean_ranks, rank_sds, by= "country")        

          