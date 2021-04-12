#' rank_generator function
#'
#' @description This function calculates the Youth Labor Market Index for Low Income Countries (YLILI) scores and ranks, compiled separately for the arithmetic and the geometric mean, for each country according to desired parameters. These parameters are: gender (male, female, or total), the last year (i.e. the oldest data) to be entered into the index, and whether missing data should be imputed for countries with sufficient but incomplete observations.
#' @title rank_generator
#' @keywords index ranking generator dimensions bygender
#' @export
#' @examples

rank_generator <- function(bygender = "Total", countries = "dev", years = c(2010,2020), impute = TRUE) {
  ## use the compute_indicators helper function to take raw data and calculate indicators as used in the index
  dfList <- compute_indicators()

  if (countries == "all") {
    country_list = countryLists()[[9]]
  } else if (countries == "dev") {
  country_list = countryLists()[[3]]
  }

  ## apply the filter_helper function to each indicator dataframe in dfList, then append the values to the chosen list of countries
  index <- lapply(dfList, filter_helper, bygender = bygender, years = years) %>%
    reduce(full_join, by = "ref_area.label", accumulate == TRUE) %>%
    filter(ref_area.label %in% country_list[[1]]) ## filter data for countries in chosen list

  index <- left_join(country_list, index, by = "ref_area.label")

  colnames(index) <- c("ref_area.label", "country_code", "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores")


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
    ungroup(.)

  ## impute missing values if required (only for dimensions that already meet required number of indicators)
  if (impute == TRUE) {
    index <- impute_helper(index)
  }

  index <- index %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:5]))<2, rowMeans(.[3:5], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[6:9]))<3, rowMeans(.[6:9], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[10:12]))<2, rowMeans(.[10:12], na.rm = TRUE),NA)) %>%
    mutate(transition_geom = ifelse(rowSums(is.na(.[3:5]))<2, apply(.[3:5], 1, gm_mean),NA)) %>%
    mutate(working_conditions_geom = ifelse(rowSums(is.na(.[6:9]))<3, apply(.[6:9], 1, gm_mean),NA)) %>%
    mutate(education_geom = ifelse(rowSums(is.na(.[10:12]))<2, apply(.[10:12], 1, gm_mean),NA)) %>%
    rename(country = "ref_area.label")

  rank <- index %>%
    mutate(index_mean = ifelse(rowSums(is.na(.[13:15]))==0, rowMeans(.[13:15], na.rm = TRUE),NA)) %>%
    mutate(index_geom = ifelse(rowSums(is.na(.[16:18]))==0, apply(.[16:18], 1, gm_mean),NA)) #%>%
    #dplyr::select(country, country_code, transition_mean, working_conditions_mean, education_mean, transition_geom, working_conditions_geom, education_geom, index_mean, index_geom)

  return(rank)
}
