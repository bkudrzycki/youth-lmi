#' mismatch function
#'
#' @description This function calculates the exucation mismatch, taking raw employment-by-education and unemployment-by-education dataframes as inputs
#' @title mismatch
#' @keywords ilostat clean
#' @export
#' @examples
##

mismatch <- function(empxeduc, uexeduc) {
  empxeduc <- empxeduc %>%
    select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
    pivot_wider(names_from = c(classif2.label), values_from = obs_value) %>%
    mutate(prim_emp = rowSums(.[5:7], na.rm = TRUE), #sum aggregate educational attainment for youth and total
           sec_emp = rowSums(.[8:9], na.rm = TRUE),
           tert_emp = rowSums(.[c(10:12,14)], na.rm = TRUE)) %>%
    mutate_all(funs(na_if(.,0))) %>%
    select(ref_area.label, time, sex.label, prim_emp, sec_emp, tert_emp, total_emp = "Education (ISCED-11): Total")

  uexeduc <- uexeduc %>%
    select(ref_area.label, classif2.label, sex.label, time, obs_value) %>%
    pivot_wider(names_from = c(classif2.label), values_from = obs_value) %>%
    mutate(prim_unemp = rowSums(.[5:7], na.rm = TRUE), #sum aggregate educational attainment for youth and total
           sec_unemp = rowSums(.[8:9], na.rm = TRUE),
           tert_unemp = rowSums(.[c(10:12,15)], na.rm = TRUE))  %>%
    mutate_all(funs(na_if(.,0))) %>%
    select(ref_area.label, time, sex.label, prim_unemp, sec_unemp, tert_unemp, total_unemp = "Education (ISCED-11): Total")

  mismatch <- inner_join(uexeduc, empxeduc, by = c("ref_area.label", "time", "sex.label")) %>%
    mutate(obs_value = 100*1/3*(abs(prim_emp/total_emp-prim_unemp/total_unemp)+
                                        abs(sec_emp/total_emp-sec_unemp/total_unemp)+
                                        abs(tert_emp/total_emp-tert_unemp/total_unemp))) %>%
    select(country = ref_area.label, time, sex.label, obs_value)


  return(mismatch)
}
