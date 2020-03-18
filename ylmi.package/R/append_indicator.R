#' append_indicator function
#'
#' @description This function appends the indicator of interest to the working dataframe
#' @title append_indicator
#' @keywords append
#' @export
#' @examples

append_indicator <- function(index_frame, df, indicator_name, lastyear=2009) {

  df <- df %>%
    select(country = ref_area.label, sex = sex.label, year = time, obs_value) %>%
    pivot_wider(names_from = sex, values_from = obs_value) %>%
    group_by(country) %>%
    top_n(1, year) %>% ## include only a single observation per country per year
    filter(year >= lastyear) %>% ## drop observations before specified cutoff year
    select(country, year, "Sex: Total", "Sex: Male", "Sex: Female")

  varnames <- c(paste(as.name(indicator_name), "total", sep="_"),
                paste(as.name(indicator_name), "male", sep="_"),
                paste(as.name(indicator_name), "female", sep="_"),
                paste(as.name(indicator_name), "year", sep="_"))
  oldnames = c("Sex: Total", "Sex: Male", "Sex: Female", "year")

  df <- df %>%
    rename_at(vars(oldnames), ~ varnames)

  index_frame <- full_join(index_frame, df, by = "country")

  return(index_frame)
}
