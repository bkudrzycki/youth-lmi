#' filter_helper function
#'
#' @description This function is used in rank_generator to filter observations on gender (for male, female, or total) and choose the most recent observation for each country, provided that country is no older than the year indicated in the function parameter "last year".
#' @title filter_helper
#' @keywords filter helper
#' @export
#' @examples index <- filter_helper(neet, bygender = TRUE, lastyear= 2009)

filter_helper <- function(df, bygender, lastyear) {
  varname <- deparse(substitute(df)) ## save name of input dataframe as string
  df$sex.label <- substring(df$sex.label,6) ## clip prefix from Sex: Total, Sex: Male and Sex: Female labels

    df <- as_tibble(df) %>%
      filter(sex.label == bygender) %>%
      filter(time >= lastyear) %>%
      group_by(ref_area.label) %>%
      top_n(1, time) %>% ## include only a single observation per country per year
      select(ref_area.label, obs_value)
    df <- df %>%
      rename(!!varname := obs_value)

    }
