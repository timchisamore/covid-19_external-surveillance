#' Getting Currently in ICU
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case is currentlyin ICU.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param adjusted_outcome a character vector of adjusted outcomes
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case is currently in ICU
#' @export
#'
#' @examples
#' getting_currently_in_icu(investigation_number, adjusted_outcome, clean_ccm_investigations_data, clean_ccm_interventions_data)
getting_currently_in_icu <-
  function(investigation_number,
           adjusted_outcome,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who are currently in ICU
    currently_in_icu_investigation_numbers <- getting_currently_in_icu_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)

    get_currently_in_icu <- investigation_number %in% currently_in_icu_investigation_numbers
    get_currently_in_icu <- if_else(get_currently_in_icu & adjusted_outcome == "Active", "Yes", "No")

    return(get_currently_in_icu)
  }
