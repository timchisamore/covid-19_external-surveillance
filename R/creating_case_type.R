#' Creating Case Type
#'
#' This function takes a vector of numeric investigation numbers of cases and determines whether
#' they are health care workers, LTCH or RH residents, or community cases using clean
#' CCM risk factors data
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector indicating the type of case.
#' @export
#'
#' @examples
#' `creating_case_type(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)`
creating_case_type <- function(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  create_case_type <- case_when(
    creating_health_care_workers(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data) == "Yes" ~ "Health Care Worker",
    creating_ltch_or_rh_residents(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data) == "Yes" ~ "LTCH or RH Resident",
    TRUE ~ "Community"
  )

  return(create_case_type)
}
