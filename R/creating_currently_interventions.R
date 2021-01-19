#' Creating Currently Interventions
#'
#' This function takes a vector of numeric investigation numbers of cases and determines whether
#' they are currently intubated with invasive ventilation, in ICU, or hospitalized using clean
#' CCM interventions data. Note, these interventions are not mutually exclusive so there
#' may be some issues.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param adjusted_outcome A `character` vector of adjusted outcomes.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#' @param clean_ccm_interventions_data A `tbl_df` of clean CCM interventions data.
#'
#' @return A `character` vector indicating whether a case is currently recieving an intervention.
#' @export
#'
#' @examples
#' `creating_currently_interventions(investigation_number, adjusted_outcome, clean_ccm_investigations_data, clean_ccm_interventions_data)`
creating_currently_interventions <-
  function(investigation_number,
           adjusted_outcome,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    create_currently_interventions <-
      case_when(
        creating_currently_ventilated(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "Ventilated",
        creating_currently_in_icu(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "ICU",
        creating_currently_hospitalized(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "Hospitalized",
        TRUE ~ "None"
      )

    return(create_currently_interventions)
  }
