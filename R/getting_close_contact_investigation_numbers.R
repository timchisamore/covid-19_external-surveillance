#' Getting Close Contact Investigation Numbers
#'
#' This function takes the clean CCM risk factors data and returns the investigation
#' numbers of cases who were either close contacts or household contacts.
#'
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a numeric vector of investigation numbers
#' @export
#'
#' @examples
#' getting_close_contact_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_close_contact_investigation_numbers <- function(clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  get_close_contact_investigation_numbers <- clean_ccm_risk_factors_data %>%
    filter(
      risk_factor %in% c(
        "Close contact with a case",
        "Household contact with a case"
      ) &
        (
          is.na(iphis_case_id) |
            (
              !is.na(iphis_case_id) &
                additional_risk_factor_information == "YES"
            )
        )
    ) %>%
    pull(investigation_number)

  # ensuring all of our generated investigation numbers are valid
  assert_investigation_numbers(investigation_numbers, get_close_contact_investigation_numbers)

  return(get_close_contact_investigation_numbers)
}