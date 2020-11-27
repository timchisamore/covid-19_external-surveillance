#' Getting LTCH and RH Resident Investigation Numbers
#'
#' This function takes the clean CCM risk factors data and returns the investigation
#' numbers of cases who were residents of long-term care or retirement homes.
#'
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a numeric vector of investigation numbers
#' @export
#'
#' @examples
#' getting_ltch_or_rh_resident_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_ltch_or_rh_resident_investigation_numbers <- function(clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  # extracting the investigation numbers of any cases who were residents of LTCHs or RHs
  get_ltch_or_rh_resident_investigation_numbers <-
    clean_ccm_risk_factors_data %>%
    filter(
      risk_factor %in% c(
        "Resident of nursing home or other chronic care facility",
        "Resident of long-term care home",
        "Resident of retirement home"
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
  assert_investigation_numbers(investigation_numbers, get_ltch_or_rh_resident_investigation_numbers)

  return(get_ltch_or_rh_resident_investigation_numbers)
}
