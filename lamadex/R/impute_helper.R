#' impute_helper function
#'
#' @description This helper function imputes missing values conditional on the country having enough observations to be considered for the index. The imputation take the average percentile of the existing observations in the dimension and impute the missing indicator(s) by taking the corresponding quantile of the distribution of the missing indicator. For example, if observations for two of three observations in the "education" dimension exist for a country, and this country is in the 90th percentile of countries when the averaged across the two existing indicators, then the function will impute the third, missing dimension to be the 90th percentile of the distribution of this last variable.
#' @title impute_helper
#' @keywords impute helper
#' @export
#' @examples

impute_helper <- function(index) {

  find_pct <- function(x, na.rm = TRUE) apply(x, 2, function(c) ecdf(c)(c))

  if (sum(!is.na(index$neet)) > 1 & sum(!is.na(index$relative_wc)) > 1 & sum(!is.na(index$mismatch)) > 1) {

  index <- index %>%
    add_column(transition_pct = rowMeans(find_pct(index[3:5]), na.rm = TRUE)) %>% ## avg percentile for transition index
    mutate(neet_impute = quantile(index$neet, transition_pct, na.rm = TRUE), ## calculate imputed neet, rur, and mismatch values
           relative_wc_impute = quantile(index$relative_wc, transition_pct, na.rm = TRUE),
           mismatch_impute = quantile(index$mismatch, transition_pct, na.rm = TRUE))

  index <- index %>%
  mutate(neet = ifelse(rowSums(is.na(index[3:5]))<2, coalesce(neet, neet_impute), neet),
         relative_wc = ifelse(rowSums(is.na(index[3:5]))<2, coalesce(relative_wc, relative_wc_impute), relative_wc),
         mismatch = ifelse(rowSums(is.na(index[3:5]))<2, coalesce(mismatch, mismatch_impute), mismatch))

  }

  if (sum(!is.na(index$workingpov)) > 1 & sum(!is.na(index$underemp)) > 1 & sum(!is.na(index$informal)) > 1 & sum(!is.na(index$elementary)) > 1) {

    index <- index %>%
      add_column(workCond_pct = rowMeans(find_pct(index[6:9]), na.rm = TRUE)) %>% ## repeat for remaining two dimensions
      mutate(workPov_impute = quantile(index$workingpov, workCond_pct, na.rm = TRUE),
             underemp_impute = quantile(index$underemp, workCond_pct, na.rm = TRUE),
             informal_impute = quantile(index$informal, workCond_pct, na.rm = TRUE),
             elementary_impute = quantile(index$elementary, workCond_pct, na.rm = TRUE))

    index <- index %>%
      mutate(workingpov = ifelse(rowSums(is.na(index[6:9]))<3, coalesce(workingpov, workPov_impute), workingpov),
             underemp = ifelse(rowSums(is.na(index[6:9]))<3, coalesce(underemp, underemp_impute), underemp),
             informal = ifelse(rowSums(is.na(index[6:9]))<3, coalesce(informal, informal_impute), informal),
             elementary = ifelse(rowSums(is.na(index[6:9]))<3, coalesce(elementary, elementary_impute), elementary))

  }

  if (sum(!is.na(index$nosecondary)) > 1 & sum(!is.na(index$literacy)) > 1 & sum(!is.na(index$test_scores)) > 1) {

    index <- index %>%
      add_column(education_pct = rowMeans(find_pct(index[9:11]), na.rm = TRUE)) %>%
      mutate(nosecondary_impute = quantile(index$nosecondary, education_pct, na.rm = TRUE),
             literacy_impute = quantile(index$literacy, education_pct, na.rm = TRUE),
             testScore_impute = quantile(index$test_scores, education_pct, na.rm = TRUE))

    index <- index %>%
      mutate(nosecondary = ifelse(rowSums(is.na(.[9:11]))<2, coalesce(nosecondary, nosecondary_impute), nosecondary),
             literacy = ifelse(rowSums(is.na(.[9:11]))<2, coalesce(literacy, literacy_impute), literacy),
             test_scores = ifelse(rowSums(is.na(.[9:11]))<2, coalesce(test_scores, testScore_impute), test_scores))

  }

  ## if the dimension has enough indicators, impute the remaining value(s) by filling in ("coalescing") NAs with values from imputed lists
  index <- index %>%
    dplyr::select(-contains(c("impute", "pct")))

    return(index)
}
