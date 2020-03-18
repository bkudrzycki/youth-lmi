#' tranComp function
#'
#' @description This function compiles the transition dimension of the YLMILIC by gender or in aggregate from raw dataframes. Takes the gender option and the last acceptable year for data as arguments.
#' @title transComp
#' @keywords transition
#' @export
#' @examples

transComp <- function(country_list, neet, relative_unemp, empxeduc, uexeduc, bygender = FALSE, lastyear = 2009) {
  rescale <- function(x, na.rm = FALSE) (100-x)
  rur_rescale <- function(x, na.rm = FALSE) ifelse(x < 1, 100, ifelse(x > 10, 0, (100-(((x-1)/(10-1))*100))))

  if(bygender==FALSE){
    neet <- neet %>%
      select(country = ref_area.label, sex.label, time, obs_value) %>%  ## read in neet
      filter_help(., bygender, lastyear) %>%
      rename("neet" = obs_value)

    index <- full_join(country_list, neet, by = "country")

    relative_unemp <- relative_unemp %>%
      select(country = ref_area.label, classif1.label, sex.label, time, obs_value) %>%
      filter(sex.label == "Sex: Total")  %>%
      pivot_wider(names_from = classif1.label, values_from = obs_value) %>%
      mutate(obs_value = `Age (Youth, adults): 15-24`/`Age (Youth, adults): 25+`) %>%
      filter_help(., bygender, lastyear) %>%
      rename("relative_unemp" = obs_value)

    index <- full_join(index, relative_unemp, by = "country")

    mismatch <- mismatch(empxeduc, uexeduc) %>%
      filter_help(., bygender, lastyear) %>%
      rename("mismatch" = obs_value)

    index <- full_join(index, mismatch, by = "country") %>%
      mutate_at(c("neet", "mismatch"), rescale) %>%
      mutate_at("relative_unemp", rur_rescale) %>%
      ungroup(.) %>%
      mutate(transition_score = ifelse(rowSums(is.na(.))<2, rowMeans(.[3:5], na.rm = TRUE),NA))
    return(index)
  }
  else{
    return("Halp")
  }
}
