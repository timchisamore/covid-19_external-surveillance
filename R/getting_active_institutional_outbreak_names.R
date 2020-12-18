#' Getting Active Institutional Outbreak Names
#'
#' This function takes the clean CCM outbreaks data and returns a character vector
#' of the outbreak names of active institutional outbreaks.
#'
#' @param clean_ccm_outbreaks_data a tbl_df of cleaned CCM outbreaks data
#'
#' @return a character vector of active institutional outbreak names
#' @export
#'
#' @examples
#' getting_active_institutional_outbreak_names(clean_ccm_outbreaks_data)
getting_active_institutional_outbreak_names <- function(clean_ccm_outbreaks_data) {
  get_active_institutional_outbreak_names <- clean_ccm_outbreaks_data %>%
    filter(
      location_location_type %in% c("Retirement Home", "Long Term Care Home", "Hospital", "Child Care Facility / Daycare - Licenced"),
      is.na(date_outbreak_declared_over) | outbreak_status == "Open"
    ) %>%
    pull(outbreak_name)

  return(get_active_institutional_outbreak_names)
}
