#' Generate Close Contact IDs
#'
#' This function takes the CCM risk factors data and generates a vector of case IDs corresponding
#' to those records where the cases answered yes to a risk factor indicating they were a close
#' contact of a case
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of our cleaned CCMrisk factors data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_close_contact_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
generate_close_contact_ids <- function(clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  # extracting the investigation numbers of any cases who were close contacts
  close_contact_ids <- clean_ccm_risk_factors_data %>%
    mutate(risk_factor = str_to_upper(risk_factor)) %>%
    filter(fct_match(
      risk_factor,
      c(
        "CLOSE CONTACT WITH A CASE",
        "HOUSEHOLD CONTACT WITH A CASE"
      )
    ) &
      ((
        !is.na(iphis_case_id) &
          fct_match(additional_risk_factor_information, "YES")
      ) | is.na(iphis_case_id))) %>%
    pull(investigation_number)

  # ensuring all of our generated IDs are valid
  assert_generated_ids(investigation_numbers, close_contact_ids)

  return(close_contact_ids)
}
