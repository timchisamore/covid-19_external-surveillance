#' Getting Health Care Workers
#'
#' This function takes a vector of investigation numbers and clean CCM risk factors data and returns
#' a vector indicating whether the case was a health care worker.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#'
#' @return a character vector indicating if the case was a health care worker
#' @export
#'
#' @examples
#' getting_health_care_workers(investigation_number, clean_ccm_investigations_data, clean_ccm_risk_factors_data)
getting_health_care_workers <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_risk_factors_data) {
    # extracting the investigation numbers of any cases who were health care workers
    health_care_workers_investigation_numbers <- getting_health_care_worker_investigation_numbers(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    get_health_care_workers <- investigation_number %in% health_care_workers_investigation_numbers
    get_health_care_workers <- if_else(get_health_care_workers, "Yes", "No")

    return(get_health_care_workers)
  }
