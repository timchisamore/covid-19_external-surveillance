#' Getting LTCH or RH Residents
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case was a LTCH or RH resident.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a character vector indicating if the case was a LTCH or RH resident
#' @export
#'
#' @examples
#' getting_ltch_or_rh_residents(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_ltch_or_rh_residents <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who were LTCH or RH residents
    ltch_or_rh_resident_investigation_numbers <- getting_ltch_or_rh_resident_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    get_ltch_or_rh_residents <- investigation_number %in% ltch_or_rh_resident_investigation_numbers
    get_ltch_or_rh_residents <- if_else(get_ltch_or_rh_residents, "Yes", "No")

    return(get_ltch_or_rh_residents)
  }
