#' Creating Household Contact
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case was a household contact of a case.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector indicating if the case was a household contact of a case.
#' @export
#'
#' @examples
#' `creating_household_contact(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)`
creating_household_contact <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who were household contacts of a case
    household_contact_investigation_numbers <- getting_household_contact_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    create_household_contact <- investigation_number %in% household_contact_investigation_numbers
    create_household_contact <- if_else(create_household_contact, "Yes", "No")

    return(create_household_contact)
  }
