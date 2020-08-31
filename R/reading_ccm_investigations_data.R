#' Reading CCM Investigations Data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 Investigations data
#' into our R project from a local folder. Note that the returned object will be a tbl_df
#'
#' @param path a pathway to the CCM Investigations data .csv file
#'
#' @return A tbl_df of our CCM Investigations data
#' @export
#'
#' @examples
#' reading_ccm_investigations_data(here::here("data", "raw", "ccm_investigations_data.csv"))
reading_ccm_investigations_data <- function(path) {
  raw_ccm_investigations_data <- read_csv(file = path)

  return(raw_ccm_investigations_data)
}
