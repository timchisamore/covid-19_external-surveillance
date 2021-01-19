#' Creating LTCH or RH Residents
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case was a LTCH or RH resident.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector indicating if the case was a LTCH or RH resident.
#' @export
#'
#' @examples
#' creating_ltch_or_rh_residents(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)
creating_ltch_or_rh_residents <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who were LTCH or RH residents
    ltch_or_rh_resident_investigation_numbers <- getting_ltch_or_rh_resident_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    create_ltch_or_rh_residents <- investigation_number %in% ltch_or_rh_resident_investigation_numbers
    create_ltch_or_rh_residents <- if_else(create_ltch_or_rh_residents, "Yes", "No")

    return(create_ltch_or_rh_residents)
  }
