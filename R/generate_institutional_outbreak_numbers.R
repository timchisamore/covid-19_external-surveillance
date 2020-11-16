#' Generate Institutional Outbreak Numbers
#' 
#' This function takes the clean CCM outbreaks data and returns a character vector
#' of he outbreak numbers representing institutional outbreaks. This is indicated
#' by the 4 digit master number of the facility at the beginning of the outbreak name.
#'
#' @param clean_ccm_outbreaks_data a tbl_df of cleaned CCM outbreaks data
#'
#' @return a character vector of institutional outbreak numbers
#' @export
#'
#' @examples
#' generate_institutional_outbreak_numbers(clean_ccm_outbreaks_data)
generate_institutional_outbreak_numbers <- function(clean_ccm_outbreaks_data) {
  institutional_outbreak_numbers <- clean_ccm_outbreaks_data %>%
    filter(location_location_type %in% c("Retirement Home", "Long Term Care Home", "Hospital"),
           is.na(date_outbreak_declared_over) | outbreak_status == "Open") %>%
    pull(iphis_outbreak_number)
  
  return(institutional_outbreak_numbers)
}
