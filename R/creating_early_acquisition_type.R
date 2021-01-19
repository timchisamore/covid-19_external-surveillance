#' Creating Early Acquisition Type
#'
#' This function takes an investigation number, clean CCM investigations,
#' outbreaks, and risk factors data and used Public Health Ontario
#' logic to return how they likely acquired COVID-19 (travel,
#' outbreak related, close  contact, missing or unknown, or no know epi-link).
#' This logic corresponds to cases occuring prior to 2020/01/01.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#' @param clean_ccm_outbreaks_data A `tbl_df` of clean CCM outbreaks data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector indicating how a case acquired COVID-19.
#' @export
#'
#' @examples
#' `creating_early_acquisition_type(investigation_number, clean_ccm_investigations_data, clean_ccm_outbreaks_data, clean_ccm_risk_factors_data)`
creating_early_acquisition_type <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_outbreaks_data,
           clean_ccm_risk_factors_data) {
    # our order of logic is travel > outbreak related > close contact > unknown or pending > no known epi-link
    create_early_acquisition_type <-
      case_when(
        creating_travel(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ) == "Yes" ~ "Travel",
        creating_outbreak_related(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_outbreaks_data
        ) == "Yes" ~ "Outbreak related",
        creating_close_contact(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ) == "Yes" ~ "Close contact",
        creating_unknown_or_pending(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ) == "Yes" ~ "Unknown or pending",
        TRUE ~ "No known epi-link"
      )

    return(create_early_acquisition_type)
  }
