#' workcondDim function
#'
#' @description This function compiles the working conditions dimension of the YLMILIC by gender or in aggregate from raw dataframes. Takes the gender option and the last acceptable year for data as arguments.
#' @title workcondDim
#' @keywords transition
#' @export
#' @examples

workcondDim <- function(index, workingpov, underemp, employment, informal, occupations, bygender = FALSE, lastyear = 2009) {
  rescale <- function(x, na.rm = FALSE) (100-x)
  if(bygender==FALSE){
    workingpov <- filter_help(workingpov, bygender, lastyear)
    index <- full_join(index, workingpov, by = "country")

    emp_tot <- employment %>%
      filter(classif1.label == "Age (Aggregate bands): 15-24", classif2.label == "Status in employment (ICSE-93): Total")
    underemp <- full_join(underemp, emp_tot, by=c("ref_area.label", "time", "sex.label")) %>%
      mutate(obs_value = 100 * obs_value.x / obs_value.y) ## number of youth underemployed divided by total number of youth employed
    underemp <- filter_help(underemp, bygender, lastyear)
    index <- full_join(index, underemp, by = "country")

    informal <- filter_help(informal)
    index <- full_join(index, informal, by = "country")

    vulnerable <- employment %>%
      pivot_wider(names_from = classif2.label, values_from = obs_value) %>%
      filter(classif1.label == "Age (Aggregate bands): 15-24", obs_status.label != "Unreliable") %>%
      mutate(obs_value = 100 * rowSums(.[14:15]) / .$"Status in employment (ICSE-93): Total") ## divide own account/family by total working youth
    vulnerable <- filter_help(vulnerable, bygender, lastyear) %>%
      drop_na() ## There are some double entries due to "unreliable" data from the ILO. Dropping NAs solved this problem provisionally
    index <- full_join(index, vulnerable, by = "country")

    elementary <- occupations %>%
      filter(classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24"),
             classif2.label %in% c("Occupation (ISCO-08): 9. Elementary occupations",
                                   "Occupation (ISCO-08): Total"),
             obs_status.label != "Unreliable") %>%
      pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>%
      mutate(obs_value = 100 * rowSums(.[c(11,13)]) / rowSums(.[c(10,12)]))
    elementary <- filter_help(elementary, bygender, lastyear) %>%
      drop_na()
    index <- full_join(index, elementary, by = "country")

    ## SAFF: rate of youth in working in skilled agriculture, fishery or forestry
    saff <- occupations %>%
      filter(classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24"),
             classif2.label %in% c("Occupation (ISCO-08): 6. Skilled agricultural, forestry and fishery workers",
                                   "Occupation (ISCO-08): Total"),
             obs_status.label != "Unreliable") %>%
      pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>%
      mutate(obs_value = 100 * rowSums(.[c(11,13)]) / rowSums(.[c(10,12)]))
    saff <- filter_help(saff, bygender, lastyear) %>%
      drop_na()
    index <- full_join(index, saff, by = "country")

    ##rescale all transition indicators and calculate arithmetic mean
    index <- index %>%
      mutate_at(c("workingpov", "underemp", "informal", "vulnerable", "elementary", "saff"), rescale) %>%
      mutate(working_conditions_score = ifelse(rowSums(is.na(.))<3, rowMeans(.[3:7], na.rm = TRUE),NA)) %>%
      filter(!is.na(country_code))
    return(index)
  }
  else{
    return("Halp")
  }
}
