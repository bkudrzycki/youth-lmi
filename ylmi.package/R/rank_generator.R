#' rank_generator function
#'
#' @description This function takes a cleaned list of dataframes for each indicator in the youth labor market index and the list of countries of interest and outputs an index score for each country according to the desired parameters. These parameters are: disaggregation by gender (yes or no), the last year (i.e. the oldest data) to be entered into the index, and whether missing data should be imputed for countries with sufficient but incomplete observations.
#' @title rank_generator
#' @keywords index ranking generator dimensions bygender
#' @export
#' @examples

rank_generator <- function(dfList, country_list, bygender = FALSE, lastyear = 2009, impute = FALSE) {

  ## apply the filter_helper function to each indicator dataframe in dfList, then append the values to the chosen list of countries
  index <- lapply(dfList, filter_helper, index = country_list, bygender = bygender, lastyear = lastyear) %>%
    reduce(left_join, by = "ref_area.label")

  index <- right_join(index, country_list, by = "ref_area.label") ## filter data for countries in chosen list

  colnames(index) <- c("ref_area.label",
                       "neet",
                       "relative_unemp",
                       "mismatch",
                       "workingpov",
                       "underemp",
                       "informal",
                       "vulnerable",
                       "elementary",
                       "saff",
                       "nosecondary",
                       "literacy",
                       "test_scores",
                       "country_code")


  ## impute missing values if required (only for dimensions that already meet required number of indicators)

  if (impute == TRUE) {
    index <- impute_helper(index)
  }

  ## rescale all indicators and calculate dimension scores

  rescale <- function(x, na.rm = FALSE) (100-x)
  rur_rescale <- function(x, na.rm = FALSE) ifelse(x < 1, 100, ifelse(x > 10, 0, (100-(((x-1)/(10-1))*100))))
  hts_rescale <- function(x, na.rm = FALSE) (((x-300)/(625-300))*100)
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }

  index <- index %>%
    mutate_at(c("neet",
                "mismatch",
                "workingpov",
                "underemp",
                "informal",
                "vulnerable",
                "elementary",
                "saff",
                "nosecondary"), rescale) %>%
    mutate_at("relative_unemp", rur_rescale) %>%
    mutate_at("test_scores", hts_rescale) %>%
    ungroup(.) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[2:4]))<2, rowMeans(.[2:4], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[5:10]))<3, rowMeans(.[5:10], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[11:13]))<2, rowMeans(.[11:13], na.rm = TRUE),NA)) %>%
    mutate(transition_geom = ifelse(rowSums(is.na(.[2:4]))<2, apply(.[2:4], 1, gm_mean),NA)) %>%
    mutate(working_conditions_geom = ifelse(rowSums(is.na(.[5:10]))<3, apply(.[5:10], 1, gm_mean),NA)) %>%
    mutate(education_geom = ifelse(rowSums(is.na(.[11:13]))<2, apply(.[11:13], 1, gm_mean),NA)) %>%
    rename(country = "ref_area.label")

  rank <- index %>%
    mutate(index_mean = ifelse(rowSums(is.na(.[15:17]))==0, rowMeans(.[15:17], na.rm = TRUE),NA)) %>%
    mutate(index_geom = ifelse(rowSums(is.na(.[18:20]))==0, apply(.[18:20], 1, gm_mean),NA)) #%>%
    #filter(!is.na(rank)) %>%
    #select(country, country_code, transition_score, working_conditions_score, education_score, index_score)

  return(rank)
}
