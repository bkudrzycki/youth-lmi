#' rank_generator function
#'
#' @description This function takes a formatted list of dataframes for the following indicators: neet, unemployment rate, employed (in thousands), unemployed (in thousands), working povertyy, underemployment, rate of workers in informal jobs, employed by status, employed by occupation, education, literacy, and harmonized test scores. All these indicators must be disaggregated by sex and limited to the 15-24 age demographic (the unemployment rate must also include data from those 25+). The function returns dimension and index scores, compiled separately for the arithmetic and the geometric mean, for each country according to the desired parameters. These parameters are: gender (male, female, or total), the last year (i.e. the oldest data) to be entered into the index, and whether missing data should be imputed for countries with sufficient but incomplete observations.
#' @title rank_generator
#' @keywords index ranking generator dimensions bygender
#' @export
#' @examples

rank_generator <- function(dfList, country_list, bygender = "Total", lastyear = 2009, impute = FALSE) {
  ## use the compute_indicators helper function to take raw data and calculate indicators as used in the index
  dfList <- compute_indicators(dfList)

  ## apply the filter_helper function to each indicator dataframe in dfList, then append the values to the chosen list of countries
  index <- lapply(dfList, filter_helper, bygender = bygender, lastyear = lastyear) %>%
    reduce(full_join, by = "ref_area.label", accumulate == TRUE)

  index <- right_join(index, country_list, by = "ref_area.label") ## filter data for countries in chosen list

  colnames(index) <- c("ref_area.label", "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores", "country_code")


  ## impute missing values if required (only for dimensions that already meet required number of indicators)
  if (impute == TRUE) {
    index <- impute_helper(index)
  }

  ## rescale all indicators and calculate dimension scores
  rescale <- function(x, na.rm = FALSE) (100-x)
  rur_rescale <- function(x, na.rm = FALSE) ifelse(x < 1, 100, ifelse(x > 3, 0, (100-(((x-1)/(3-1))*100))))
  hts_rescale <- function(x, na.rm = FALSE) (((x-300)/(625-300))*100)
  gm_mean = function(x){
    exp(sum(log(x[x > 0]), na.rm=TRUE) / length(x[!is.na(x)]))
  }

  index <- index %>%
    mutate_at(c("neet",
                "mismatch",
                "workingpov",
                "underemp",
                "informal",
                "elementary",
                "nosecondary"), rescale) %>%
    mutate_at("relative_wc", rur_rescale) %>%
    mutate_at("test_scores", hts_rescale) %>%
    ungroup(.) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[2:4]))<2, rowMeans(.[2:4], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[5:8]))<3, rowMeans(.[5:8], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[9:11]))<2, rowMeans(.[9:11], na.rm = TRUE),NA)) %>%
    mutate(transition_geom = ifelse(rowSums(is.na(.[2:4]))<2, apply(.[2:4], 1, gm_mean),NA)) %>%
    mutate(working_conditions_geom = ifelse(rowSums(is.na(.[5:8]))<3, apply(.[5:8], 1, gm_mean),NA)) %>%
    mutate(education_geom = ifelse(rowSums(is.na(.[9:11]))<2, apply(.[9:11], 1, gm_mean),NA)) %>%
    rename(country = "ref_area.label")

  rank <- index %>%
    mutate(index_mean = ifelse(rowSums(is.na(.[13:15]))==0, rowMeans(.[13:15], na.rm = TRUE),NA)) %>%
    mutate(index_geom = ifelse(rowSums(is.na(.[16:18]))==0, apply(.[16:18], 1, gm_mean),NA)) #%>%
    #dplyr::select(country, country_code, transition_mean, working_conditions_mean, education_mean, transition_geom, working_conditions_geom, education_geom, index_mean, index_geom)

  return(rank)
}
