#' Reading CCM Interventions Data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 Interventions data
#' into our R project from a local folder. Note that the returned object will be a tbl_df
#'
#' @param path a pathway to the CCM Interventions data .csv file
#'
#' @return A tbl_df of our CCM Interventions data
#' @export
#'
#' @examples
#' reading_ccm_interventions_data(here::here("data", "raw", "ccm_interventions_data.csv"))
reading_ccm_interventions_data <- function(path) {
  raw_ccm_interventions_data <- read_csv(file = path)

  return(raw_ccm_interventions_data)
}
