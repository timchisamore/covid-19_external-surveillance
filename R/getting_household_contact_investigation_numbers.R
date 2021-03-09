#' Getting Household Contact Investigation Numbers
#'
#' This function takes the clean CCM risk factors data and returns the investigation
#' numbers of cases who were household contacts.
#'
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `numeric` vector of investigation numbers.
#' @export
#'
#' @examples
#' `getting_household_contact_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)`
getting_household_contact_investigation_numbers <- function(clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  get_household_contact_investigation_numbers <- clean_ccm_risk_factors_data %>%
    filter(
      risk_factor == "Household contact with a case" &
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
  assert_investigation_numbers(investigation_numbers, get_household_contact_investigation_numbers)

  return(get_household_contact_investigation_numbers)
}
