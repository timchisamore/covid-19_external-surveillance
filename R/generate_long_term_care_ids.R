#' Generate Long Term Care IDs
#'
#' This function takes the CCM risk factors data and generates a vector of case IDs corresponding
#' to those records where the cases answered yes to a risk factor indicating they were living in
#' long term care facilities
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of our cleaned CCM risk factors data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_long_term_care_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
generate_long_term_care_ids <-
  function(clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting all investigation numbers
    investigation_numbers <- clean_ccm_investigations_data %>%
      pull(investigation_number)

    # extracting the investigation numbers of any cases who were residents of
    # retirement homes or long-term care homes
    long_term_care_ids <- clean_ccm_risk_factors_data %>%
      mutate(risk_factor = str_to_upper(risk_factor)) %>%
      filter(fct_match(
        risk_factor,
        c(
          "RESIDENT OF NURSING HOME OR OTHER CHRONIC CARE FACILITY",
          "RESIDENT OF RETIREMENT HOME",
          "RESIDENT OF LONG-TERM CARE HOME"
        )
      )
      &
        ((
          !is.na(iphis_case_id) &
            fct_match(additional_risk_factor_information, "YES")
        ) | is.na(iphis_case_id))) %>%
      pull(investigation_number)

    # ensuring all of our generated IDs are valid
    assert_generated_ids(investigation_numbers, long_term_care_ids)

    return(long_term_care_ids)
  }
