filter_help <- function(df, bygender = FALSE, lastyear = 2009) {
  varname <- deparse(substitute(df))
  if(bygender == FALSE){
    df <- df %>%
      filter(sex.label == "Sex: Total") %>%
      filter(time >= lastyear) %>%
      group_by(ref_area.label) %>%
      top_n(1, time) %>% ## include only a single observation per country per year
      select(country = ref_area.label, obs_value)
    df <- df %>%
      rename(!!varname := obs_value)
  }
  else {
    df <- df %>%
      filter(sex.label == -"Sex: Total") %>%
      filter(time >= lastyear) %>%
      group_by(ref_area.label) %>%
      top_n(1, time) %>%
      select(country = ref_area.label, obs_value)
    df <- df %>%
      rename(!!varname := obs_value)
  }
}
