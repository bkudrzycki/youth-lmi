#' compile function
#'
#' @description This function compiles indicators and generates ranking according to preferences (last year used, sex-dependent or total, and compilation algorithm)
#' @title rank
#' @keywords ilostat clean
#' @export
#' @examples
##

compile <- function(index_frame, lastyear, bysex = FALSE) {
  index <- index_frame %>%
    filter(year >= lastyear)
  return(index)
}
