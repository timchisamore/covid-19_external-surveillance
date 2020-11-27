#' Getting Travel
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case reported travel during their incubation period.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a character vector indicating if the case reported travel during their incubation period
#' @export
#'
#' @examples
#' getting_travel(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_travel <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who reported travel during their incubation period
    travel_investigation_numbers <- getting_travel_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    get_travel <- investigation_number %in% travel_investigation_numbers
    get_travel <- if_else(get_travel, "Yes", "No")

    return(get_travel)
  }
