#' Getting Close Contact Investigation Numbers
#'
#' This function takes the clean CCM risk factors data and returns the investigation
#' numbers of cases who were close contacts.
#'
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `numeric` vector of investigation numbers.
#' @export
#'
#' @examples
#' `getting_close_contact_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)`
getting_close_contact_investigation_numbers <- function(clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  get_close_contact_investigation_numbers <- clean_ccm_risk_factors_data %>%
    filter(
      risk_factor == "Close contact with a case",
      additional_risk_factor_information == "YES"
    ) %>%
    pull(investigation_number)

  # ensuring all of our generated investigation numbers are valid
  assert_investigation_numbers(investigation_numbers, get_close_contact_investigation_numbers)

  return(get_close_contact_investigation_numbers)
}
