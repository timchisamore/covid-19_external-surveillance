#' Cleaning CCM Risk Factors Data
#'
#' This function takes the raw CCM risk factors data and cleans the field names, removes
#' empty fields, and coverts date objects to lubridate date objects.
#'
#' @param raw_ccm_risk_factors_data A tbl_df of our CCM risk factors data
#'
#' @return A tbl_df of our cleaned CCM risk factors data
#' @export
#'
#' @examples
#' cleaning_ccm_risk_factors_data(raw_ccm_risk_factors_data)
cleaning_ccm_risk_factors_data <- function(raw_ccm_risk_factors_data) {
  clean_ccm_risk_factors_data <- raw_ccm_risk_factors_data %>%
    janitor::remove_empty(which = "cols") %>%
    janitor::clean_names() %>%
    mutate(
      across(.cols = contains("date"), .fns = str_extract, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}"),
      across(.cols = contains("date"), .fns = lubridate::ymd)
    )

  return(clean_ccm_risk_factors_data)
}
