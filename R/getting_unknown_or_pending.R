#' Getting Unknown or Pending
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case has unkown or pending information
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a character vector indicating if the case has unknown or pending information
#' @export
#'
#' @examples
#' getting_unknown_or_pending(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_unknown_or_pending <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who have unknown or pending information
    unknown_or_pending_investigation_numbers <- getting_unknown_or_pending_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    get_unknown_or_pending <- investigation_number %in% unknown_or_pending_investigation_numbers
    get_unknown_or_pending <- if_else(get_unknown_or_pending, "Yes", "No")

    return(get_unknown_or_pending)
  }
