#' filter_helper function
#'
#' @description This helper function filters the desired observations from the dataframe of each indicator and appends them to the "index" dataframe. Each dataframe passed through this function must have the following columns: ref_area.label (i.e. country name), sex.label, and obs_value.
#' @title filter_helper
#' @keywords filter helper
#' @export
#' @examples index <- filter_helper(neet, index, bygender = TRUE, lastyear= 2009)

filter_helper <- function(df, index, bygender = FALSE, lastyear = 2009) {
  varname <- deparse(substitute(df)) ## save name of input dataframe as string
  varname_male <- paste(varname, "male", sep = "_")
  varname_female <- paste(varname, "female", sep = "_")

  if(bygender == FALSE){
    df <- as_tibble(df) %>%
      filter(sex.label == "Sex: Total") %>%
      filter(time >= lastyear) %>%
      group_by(ref_area.label) %>%
      top_n(1, time) %>% ## include only a single observation per country per year
      select(ref_area.label, obs_value)
    df <- df %>%
      rename(!!varname := obs_value)
  }
  else {
    df <- as_tibble(df) %>%
      filter(sex.label != "Sex: Total") %>%
      pivot_wider(names_from = sex.label, values_from = obs_value) %>%
      filter(time >= lastyear) %>%
      group_by(ref_area.label) %>%
      top_n(1, time) %>%
      select(ref_area.label, "Sex: Male", "Sex: Female")
    df <- df %>%
      rename(!!varname_male := "Sex: Male",
             !!varname_female := "Sex: Female")
  }
}
