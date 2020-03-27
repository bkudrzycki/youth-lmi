#' mismatch function
#'
#' @description
#' @title mismatch
#' @keywords ilostat clean
#' @export
#' @examples

mismatch <- function(employed, unemployed) {
  employed <- employed %>%
    filter(classif1.label == "Age (Aggregate bands): 15-24") %>%
    select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
    pivot_wider(names_from = c(classif2.label), values_from = obs_value)

  unemployed <- unemployed %>%
    filter(classif1.label == "Age (Aggregate bands): 15-24") %>%
    select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
    pivot_wider(names_from = c(classif2.label), values_from = obs_value)

  mismatch <- inner_join(employed, unemployed, by = c("ref_area.label", "time", "sex.label"))

  mismatch <- mismatch %>%
    mutate(obs_value = 100*1/2*abs(.[[13]]/.[[12]]-.[[30]]/.[[29]]) +
             abs(.[[14]]/.[[12]]-.[[31]]/.[[29]]) +
             abs(.[[15]]/.[[12]]-.[[32]]/.[[29]])) %>%
    select(ref_area.label, time, sex.label, obs_value)

  return(mismatch)
}
