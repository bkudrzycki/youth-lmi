#' educDim function
#'
#' @description This function compiles the education dimension of the YLMILIC by gender or in aggregate from raw dataframes. Takes the gender option and the last acceptable year for data as arguments.
#' @title educDim
#' @keywords transition
#' @export
#' @examples

educDim <- function(nosecondary, literacy, test_scores, bygender = FALSE, lastyear = 2009) {
  hts_rescale <- function(x, na.rm = FALSE) (((x-300)/(625-300))*100)
  rescale <- function(x, na.rm = FALSE) (100-x)
  if(bygender==FALSE){
    nosecondary <- filter_help(nosecondary, bygender, lastyear)
    index <- full_join(index, nosecondary, by = "country")

    literacy <- filter_help(literacy, bygender, lastyear)
    index <- full_join(index, literacy, by = "country")

    test_scores <- filter_help(test_scores, bygender, lastyear)
    index <- full_join(index, test_scores, by = "country")

    index <- index %>%
      mutate_at(c("nosecondary"), rescale) %>%
      mutate_at("test_scores", hts_rescale) %>%
      mutate(education_score = ifelse(rowSums(is.na(.[14:16]))<2, rowMeans(.[14:16], na.rm = TRUE),NA))
      ## filter(!is.na(country_code))
  return(index)
  }
  else {
    return("halp")
  }
}

