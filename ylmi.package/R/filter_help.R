filter_help <- function(df, bygender = FALSE, lastyear = 2009) {
  if(bygender == FALSE){
  df %>%
      filter(sex.label == "Sex: Total") %>%
      filter(time >= lastyear) %>%
      group_by(country) %>%
      top_n(1, time) %>% ## include only a single observation per country per year
      select(country, obs_value)
  }
  else {
    df %>%
      filter(sex.label == -"Sex: Total") %>%
      filter(time >= lastyear) %>%
      group_by(country) %>%
      top_n(1, time) %>%
      select(country, obs_value)
  }
}
