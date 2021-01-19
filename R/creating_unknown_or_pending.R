#' Creating Unknown or Pending
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case has unkown or pending information.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector indicating if the case has unknown or pending information.
#' @export
#'
#' @examples
#' `creating_unknown_or_pending(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)`
creating_unknown_or_pending <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who have unknown or pending information
    unknown_or_pending_investigation_numbers <- getting_unknown_or_pending_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    create_unknown_or_pending <- investigation_number %in% unknown_or_pending_investigation_numbers
    create_unknown_or_pending <- if_else(create_unknown_or_pending, "Yes", "No")

    return(create_unknown_or_pending)
  }
