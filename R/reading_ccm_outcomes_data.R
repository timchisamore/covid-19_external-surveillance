#' Reading CCM Outcomes Data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 outcomes data
#' into our R project from a local folder. Note that the returned object will be a tbl_df
#'
#' @param path a pathway to the CCM outcomes data .csv file
#'
#' @return A tbl_df of our CCM outcomes data
#' @export
#'
#' @examples
#' reading_ccm_outcomes_data(here::here("data", "raw", "ccm_outcomes_data.csv"))
reading_ccm_outcomes_data <- function(path) {
  raw_ccm_outcomes_data <- read_csv(file = path)

  return(raw_ccm_outcomes_data)
}
