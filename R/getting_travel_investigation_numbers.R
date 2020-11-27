#' Getting Travel Investigation Numbers
#'
#' This function takes the clean CCM risk factors data and returns the investigation
#' numbers of cases who reported travel during their incubation period.
#'
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a numeric vector of investigation numbers
#' @export
#'
#' @examples
#' getting_travel_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_travel_investigation_numbers <- function(clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  get_travel_investigation_numbers <- clean_ccm_risk_factors_data %>%
    filter(
      risk_factor == "Travel outside province in the last 14 days (specify province or country)" &
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
  assert_investigation_numbers(investigation_numbers, get_travel_investigation_numbers)

  return(get_travel_investigation_numbers)
}
