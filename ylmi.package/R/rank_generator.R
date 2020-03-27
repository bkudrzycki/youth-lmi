#' rank_generator function
#'
#' @description This function takes a cleaned list of dataframes for each indicator in the youth labor market index and the list of countries of interest and outputs an index score for each country according to the desired parameters. These parameters are: disaggregation by gender (yes or no), the last year (i.e. the oldest data) to be entered into the index, rescaling method (raw, zscore) and aggregation method across indicators and dimensions (arithmetic mean, geometric mean).
#' @title rank_generator
#' @keywords index ranking generator dimensions bygender
#' @export
#' @examples

rank_generator <- function(dfList, country_list, bygender = FALSE, lastyear = 2009) {

  ## apply the filter_helper function to each indicator dataframe in dfList, then append the values to the chosen list of countries
  index <- lapply(dfList, filter_helper, index = country_list, bygender = bygender, lastyear = lastyear) %>%
    reduce(right_join, by = "ref_area.label")

  index <- left_join(index, country_list, by = "ref_area.label")

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

  ## rescale all indicators and calculate dimension scores

  rescale <- function(x, na.rm = FALSE) (100-x)
  rur_rescale <- function(x, na.rm = FALSE) ifelse(x < 1, 100, ifelse(x > 10, 0, (100-(((x-1)/(10-1))*100))))
  hts_rescale <- function(x, na.rm = FALSE) (((x-300)/(625-300))*100)

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
    mutate(transition_score = ifelse(rowSums(is.na(.[2:4]))<2, rowMeans(.[2:4], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_score = ifelse(rowSums(is.na(.[5:10]))<3, rowMeans(.[5:10], na.rm = TRUE),NA)) %>%
    mutate(education_score = ifelse(rowSums(is.na(.[11:13]))<2, rowMeans(.[11:13], na.rm = TRUE),NA)) %>%
    rename(country = "ref_area.label")

  rank <- index %>%
    mutate(index_score = ifelse(rowSums(is.na(.[15:17]))==0, rowMeans(.[15:17], na.rm = TRUE),NA)) %>%
    #filter(!is.na(rank)) %>%
    select(country, country_code, transition_score, working_conditions_score, education_score, index_score)

  return(rank)
}
