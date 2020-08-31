#' Generate Healthcare Provider IDs
#'
#' This function takes the CCM risk factors data and generates a vector of case IDs corresponding
#' to those records where the cases answered yes to a risk factor indicating they are a healthcare provider
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of our cleaned CCM risk factors data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_healthcare_provider_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
generate_healthcare_provider_ids <-
  function(clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting all investigation numbers
    investigation_numbers <- clean_ccm_investigations_data %>%
      pull(investigation_number)

    # extracting the investigation numbers of any cases who were healthcare providers
    healthcare_provider_ids <- clean_ccm_risk_factors_data %>%
      mutate(risk_factor = str_to_upper(risk_factor)) %>%
      filter(fct_match(
        risk_factor,
        c(
          "OCCUPATIONAL ? HEALTH CARE WORKER",
          "OCCUPATIONAL - DOCTOR",
          "OCCUPATIONAL - NURSE"
          # "OCCUPATIONAL - DENTIST",
          # "OCCUPATIONAL - DENTAL HYGIENIST",
          # "OCCUPATIONAL - MIDWIFE",
          # "OCCUPATIONAL - PERSONAL SUPPORT WORKER",
          # "OCCUPATIONAL - OTHER MEDICAL TECHNICIANS",
          # "OCCUPATIONAL - RESPIRATORY THERAPIST"
        )
      )
      &
        ((
          !is.na(iphis_case_id) &
            fct_match(additional_risk_factor_information, "YES")
        ) | is.na(iphis_case_id))) %>%
      pull(investigation_number)

    # ensuring all of our generated IDs are valid
    assert_generated_ids(investigation_numbers, healthcare_provider_ids)

    return(healthcare_provider_ids)
  }
