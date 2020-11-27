#' Getting Health Care Worker Investigation Numbers
#'
#' This function takes the CCM risk factors data and generates a vector of investigation numbers corresponding
#' to those records where the cases answered yes to a risk factor indicating they are a health care worker
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of our cleaned CCM risk factors data
#'
#' @return a numeric vector of investigation numbers
#' @export
#'
#' @examples
#' getting_health_care_worker_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_health_care_worker_investigation_numbers <-
  function(clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting all investigation numbers
    investigation_numbers <- clean_ccm_investigations_data %>%
      pull(investigation_number)

    # extracting the investigation numbers of any cases who were healthcare providers
    get_health_care_worker_investigation_numbers <- clean_ccm_risk_factors_data %>%
      filter(fct_match(
        risk_factor,
        c(
          "Health care worker",
          "Doctor",
          "Nurse",
          "Dentist",
          "Dental hygienist",
          "Midwife",
          "Other medical technicians",
          "Personal support worker",
          "Respiratory therapist",
          "First responder"
        )
      )
      &
        ((
          !is.na(iphis_case_id) &
            fct_match(additional_risk_factor_information, "YES")
        ) | is.na(iphis_case_id))) %>%
      pull(investigation_number)

    # ensuring all of our generated investigation numbers are valid
    assert_investigation_numbers(investigation_numbers, get_health_care_worker_investigation_numbers)

    return(get_health_care_worker_investigation_numbers)
  }
