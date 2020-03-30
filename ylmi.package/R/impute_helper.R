#' impute_helper function
#'
#' @description This helper function imputes missing values conditional on the country having enough observations to be considered for the index.
#' @title impute_helper
#' @keywords impute helper
#' @export
#' @examples

impute_helper <- function(index) {

  find_pct <- function(x, na.rm = TRUE) apply(x, 2, function(c) ecdf(c)(c))

  index <- index %>%
    add_column(transition_pct = rowMeans(find_pct(index[2:4]), na.rm = TRUE)) %>% ## calculate the mean percentile of all dim1 indicators
    mutate(neet_impute = quantile(index$neet, transition_pct, na.rm = TRUE), ## calculate imputed neet, rur, and mismatch values
           rur_impute = quantile(index$relative_unemp, transition_pct, na.rm = TRUE),
           mismatch_impute = quantile(index$mismatch, transition_pct, na.rm = TRUE)) %>%
    add_column(workCond_pct = rowMeans(find_pct(index[5:10]), na.rm = TRUE)) %>% ## repeat for remaining two dimensions
    mutate(workPov_impute = quantile(index$workingpov, transition_pct, na.rm = TRUE),
           underemp_impute = quantile(index$underemp, transition_pct, na.rm = TRUE),
           informal_impute = quantile(index$informal, transition_pct, na.rm = TRUE),
           vulnerable_impute = quantile(index$vulnerable, transition_pct, na.rm = TRUE),
           elementary_impute = quantile(index$elementary, transition_pct, na.rm = TRUE),
           saff_impute = quantile(index$saff, transition_pct, na.rm = TRUE)) %>%
    add_column(education_pct = rowMeans(find_pct(index[5:10]), na.rm = TRUE)) %>%
    mutate(nosecondary_impute = quantile(index$nosecondary, transition_pct, na.rm = TRUE),
           literacy_impute = quantile(index$literacy, transition_pct, na.rm = TRUE),
           testScore_impute = quantile(index$test_scores, transition_pct, na.rm = TRUE))

  index <- index %>%
    ungroup(.) %>%
    mutate(neet = ifelse(rowSums(is.na(index[2:4]))<2, coalesce(neet, neet_impute), neet),
           relative_unemp = ifelse(rowSums(is.na(index[2:4]))<2, coalesce(relative_unemp, rur_impute), relative_unemp),
           mismatch = ifelse(rowSums(is.na(index[2:4]))<2, coalesce(mismatch, mismatch_impute), mismatch)) %>%
    mutate(workingpov = ifelse(rowSums(is.na(index[5:10]))<3, coalesce(workingpov, workPov_impute), workingpov),
           underemp = ifelse(rowSums(is.na(index[5:10]))<3, coalesce(underemp, underemp_impute), underemp),
           informal = ifelse(rowSums(is.na(index[5:10]))<3, coalesce(informal, informal_impute), informal),
           vulnerable = ifelse(rowSums(is.na(index[5:10]))<3, coalesce(vulnerable, vulnerable_impute), vulnerable),
           elementary = ifelse(rowSums(is.na(index[5:10]))<3, coalesce(elementary, elementary_impute), elementary),
           saff = ifelse(rowSums(is.na(index[5:10]))<3, coalesce(saff, saff_impute), saff)) %>%
    mutate(nosecondary = ifelse(rowSums(is.na(.[11:13]))<2, coalesce(nosecondary, nosecondary_impute), nosecondary),
           literacy = ifelse(rowSums(is.na(.[11:13]))<2, coalesce(literacy, literacy_impute), literacy),
           test_scores = ifelse(rowSums(is.na(.[11:13]))<2, coalesce(test_scores, testScore_impute), test_scores)) %>%
    select(-contains(c("impute", "pct")))

    return(index)
}
