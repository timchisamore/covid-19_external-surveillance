#' Creating Travel
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case reported travel during their incubation period.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector indicating if the case reported travel during their incubation period.
#' @export
#'
#' @examples
#' `creating_travel(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)`
creating_travel <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who reported travel during their incubation period
    travel_investigation_numbers <- getting_travel_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    create_travel <- investigation_number %in% travel_investigation_numbers
    create_travel <- if_else(create_travel, "Yes", "No")

    return(create_travel)
  }
