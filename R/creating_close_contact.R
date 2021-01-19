#' Creating Close Contact
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case was a close contact or household contact of a case.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector indicating if the case was a close contact or household contact of a case.
#' @export
#'
#' @examples
#' `creating_close_contact(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)`
creating_close_contact <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who were close contacts or household contacts of a case
    close_contact_investigation_numbers <- getting_close_contact_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    create_close_contact <- investigation_number %in% close_contact_investigation_numbers
    create_close_contact <- if_else(create_close_contact, "Yes", "No")

    return(create_close_contact)
  }
