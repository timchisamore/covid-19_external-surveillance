#' Getting Ever Interventions
#'
#' This function takes a vector of numeric investigation numbers of cases and determines whether
#' they were ever intubated with invasive ventilation, in ICU, or hospitalized using clean
#' CCM interventions data. Note, these interventions are not mutually exclusive so there
#' may be some issues.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigations data
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating whether a case ever recieved an intervention
#' @export
#'
#' @examples
#' getting_ever_interventions(investigation_number, clean_ccm_investigations_data, clean_ccm_interventions_data)
getting_ever_interventions <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    get_ever_interventions <-
      case_when(
        getting_ever_ventilated(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "Ventilated",
        getting_ever_in_icu(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "ICU",
        getting_ever_hospitalized(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ) == "Yes" ~ "Hospitalized",
        TRUE ~ "None"
      )

    return(get_ever_interventions)
  }
