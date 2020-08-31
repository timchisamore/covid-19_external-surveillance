#' Reading CCM Risk Factors Data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 risk factors data
#' into our R project from a local folder. Note that the returned object will be a tbl_df
#'
#' @param path a pathway to the CCM risk factors data .csv file
#'
#' @return A tbl_df of our CCM risk factors data
#' @export
#'
#' @examples
#' reading_ccm_risk_factors_data(here::here("data", "raw", "ccm_risk_factors_data.csv"))
reading_ccm_risk_factors_data <- function(path) {
  raw_ccm_risk_factors_data <- read_csv(file = path)

  return(raw_ccm_risk_factors_data)
}
