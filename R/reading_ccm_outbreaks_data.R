#' Reading CCM Outbreaks Data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 oubtreaks data
#' into our R project from a local folder. Note that the returned object will be a tbl_df
#'
#' @param path a pathway to the CCM outbreaks data .csv file
#'
#' @return A tbl_df of our CCM outbreaks data
#' @export
#'
#' @examples
#' reading_ccm_outbreaks_data(here::here("data", "raw", "ccm_outbreaks_data.csv"))
reading_ccm_outbreaks_data <- function(path) {
  raw_ccm_outbreaks_data <- read_csv(file = path)
  
  return(raw_ccm_outbreaks_data)
}
