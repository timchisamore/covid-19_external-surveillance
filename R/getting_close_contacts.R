#' Getting Close Contacts
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case was a close contact or household contact of a case.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a character vector indicating if the case was a close contact or household contact of a case
#' @export
#'
#' @examples
#' getting_close_contacts(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_close_contacts <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who were close contacts or household contacts of a case
    close_contact_investigation_numbers <- getting_close_contact_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    get_close_contacts <- investigation_number %in% close_contact_investigation_numbers
    get_close_contacts <- if_else(get_close_contacts, "Yes", "No")

    return(get_close_contacts)
  }
