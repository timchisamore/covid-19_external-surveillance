#' Getting Currently Ventilated
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case is currentlyin intubated with invasive ventilation.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param adjusted_outcome a character vector of adjusted outcomes
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case is currently intubated with invasive ventilation
#' @export
#'
#' @examples
#' getting_currently_ventilated(investigation_number, adjusted_outcome, clean_ccm_investigations_data, clean_ccm_interventions_data)
getting_currently_ventilated <-
  function(investigation_number,
           adjusted_outcome,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who are currently intubated with invasive ventilation
    currently_ventilated_investigation_numbers <- getting_currently_ventilated_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)

    get_currently_ventilated <- investigation_number %in% currently_ventilated_investigation_numbers
    get_currently_ventilated <- if_else(get_currently_ventilated & adjusted_outcome == "Active", "Yes", "No")

    return(get_currently_ventilated)
  }
