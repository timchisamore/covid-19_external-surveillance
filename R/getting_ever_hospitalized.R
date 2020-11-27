#' Getting Ever Hospitalized
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case was ever hospitalized.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case was ever hospitalized
#' @export
#'
#' @examples
#' getting_ever_hospitalized(investigation_number, clean_ccm_investigations_data, clean_ccm_interventions_data)
getting_ever_hospitalized <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who were ever hospitalized
    ever_hospitalized_investigation_numbers <- getting_ever_hospitalized_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)

    get_ever_hospitalized <- investigation_number %in% ever_hospitalized_investigation_numbers
    get_ever_hospitalized <- if_else(get_ever_hospitalized, "Yes", "No")

    return(get_ever_hospitalized)
  }
