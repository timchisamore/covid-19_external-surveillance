#' Creating Ever Interventions
#'
#' This function takes a vector of numeric investigation numbers of cases and determines whether
#' they were ever intubated with invasive ventilation, in ICU, or hospitalized using clean
#' CCM interventions data. Note, these interventions are not mutually exclusive so there
#' may be some issues.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#' @param clean_ccm_interventions_data A `tbl_df` of clean CCM interventions data.
#'
#' @return A `character` vector indicating whether a case ever recieved an intervention.
#' @export
#'
#' @examples
#' `creating_ever_interventions(investigation_number, clean_ccm_investigations_data, clean_ccm_interventions_data)`
creating_ever_interventions <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    create_ever_interventions <-
      case_when(
        creating_ever_ventilated(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "Ventilated",
        creating_ever_in_icu(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "ICU",
        creating_ever_hospitalized(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "Hospitalized",
        TRUE ~ "None"
      )

    return(create_ever_interventions)
  }
