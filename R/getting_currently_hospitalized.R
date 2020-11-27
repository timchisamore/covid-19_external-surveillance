#' Getting Currently Hospitalized
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case is currently hospitalized.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param adjusted_outcome a character vector of adjusted outcomes
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case is currently hospitalized
#' @export
#'
#' @examples
#' getting_currently_hospitalized(investigation_number, adjusted_outcome, clean_ccm_investigations_data, clean_ccm_interventions_data)
getting_currently_hospitalized <-
  function(investigation_number,
           adjusted_outcome,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who are currently hospitalized
    currently_hospitalized_investigation_numbers <- getting_currently_hospitalized_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)

    get_currently_hospitalized <- investigation_number %in% currently_hospitalized_investigation_numbers
    get_currently_hospitalized <- if_else(get_currently_hospitalized & adjusted_outcome == "Active", "Yes", "No")

    return(get_currently_hospitalized)
  }
