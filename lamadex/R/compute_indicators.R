#' compute_indicators function
#'
#' @description This function generates the list of indicators needed for the generate_rank function, either returning raw measures or performing necessary transformations where needed.
#' @title compute_indicators
#' @keywords compute indicators
#' @export
#' @examples

compute_indicators <- function() {

  ## remove all observations marked by ILO as "unreliable"
  # dfList[c(1:4,6,8,9)] <- map(dfList[c(1:4,6,8,9)], ~filter(.x, obs_status.label != "Unreliable"))

  ## reload indicators into dataframe, calculating indicators from raw data as needed
  load("data/neet.rda")

  ## calculate relative working poverty rate by dividing the unemployment rate for youth by that of all workers over 25 years of age
  load("./data/working_pov.rda")
  relative_workpov <- working_pov %>%
    pivot_wider(names_from = classif1.label, values_from = obs_value) %>%
    mutate(obs_value = `Age (Youth, adults): 15-24`/`Age (Youth, adults): 25+`)

  ## calculate relative underemployment rate by dividing the unemployment rate for youth by that of all workers over 25 years of age
  load("./data/underemp.rda")
  relative_underemp <- underemp %>%
    pivot_wider(names_from = classif1.label, values_from = obs_value) %>%
    mutate(obs_value = `Age (Youth, adults): 15-24`/`Age (Youth, adults): 25+`)

  ## relative working conditions
  relative_wc <- full_join(relative_workpov, relative_underemp, by = c("ref_area.label", "time", "sex.label")) %>%
    mutate(obs_value = (obs_value.x + obs_value.y)/2)

  ## calculate mismatch rate by summing over the differences in the youth employment and unemployment rates by (ILO aggregate) education level
  load("./data/employed.rda")
  employed <- employed %>%
    dplyr::select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
    pivot_wider(names_from = c(classif2.label), values_from = obs_value)

  load("./data/unemployed.rda")
  unemployed <- unemployed %>%
    dplyr::select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
    pivot_wider(names_from = c(classif2.label), values_from = obs_value)

  mismatch <- inner_join(employed, unemployed, by = c("ref_area.label", "time", "sex.label"))

  mismatch <- mismatch %>%
    mutate(obs_value = 100*1/2*(abs(.[[15]]/.[[14]]-.[[33]]/.[[32]]) + # less than basic
             abs(.[[16]]/.[[14]]-.[[34]]/.[[32]]) + # basic
             abs(.[[17]]/.[[14]]-.[[35]]/.[[32]]) + # intermediate
             abs(.[[18]]/.[[14]]-.[[36]]/.[[32]]))) # advanced

  working_pov <- working_pov %>%
    filter(classif1.label == "Age (Youth, adults): 15-24")

  underemp <- underemp %>%
    filter(classif1.label == "Age (Youth, adults): 15-24")

  load("./data/informal.rda")
  informal <- informal %>%
    pivot_longer(cols = c("Sex: Total","Sex: Male","Sex: Female"),
                 names_to = "sex.label",
                 values_to = "obs_value",
                 names_ptypes = list(sex.label = factor(levels = c("Sex: Total","Sex: Male","Sex: Female")))) ## pivot to longer form


  ## calculate elementary work rate - rate of youth working in elementary occupations
  load("./data/occupation.rda")
  elementary <- occupation %>%
    filter(classif1.label %in% c("Age (Youth bands): 15-19", "Age (Youth bands): 20-24"),
           classif2.label %in% c("Occupation (ISCO-08): 9. Elementary occupations",
                                 "Occupation (ISCO-08): Total")) %>%
    pivot_wider(names_from = c(classif1.label, classif2.label), values_from = obs_value) %>%
    mutate(obs_value = 100 * rowSums(.[c(11,13)]) / rowSums(.[c(10,12)]))

  ## calculate rate of youth with no secondary schooling
  load("./data/education.rda")
  nosecondary <- education %>%
    mutate("Sex: Female" = rowSums(.[c(3:5)]),
         "Sex: Male" = rowSums(.[c(12:14)]),
         time = as.integer(substr(Survey, 0, 4))) %>% ## keep only the year number (remove prefix)
    mutate("Sex: Total" = rowMeans(.[c(22:23)])) %>%  ## Total is assumed to be average of the male and female rates (see paper)
    pivot_longer(cols = c("Sex: Total","Sex: Male","Sex: Female"),
                 names_to = "sex.label",
                 values_to = "obs_value",
                 names_ptypes = list(sex.label = factor(levels = c("Sex: Total","Sex: Male","Sex: Female")))) ## pivot to longer form

  load("./data/literacy.rda")

  load("./data/test_scores.rda")

  dfList <- list(neet, relative_wc, mismatch, working_pov, underemp, informal, elementary, nosecondary, literacy, test_scores)

}

