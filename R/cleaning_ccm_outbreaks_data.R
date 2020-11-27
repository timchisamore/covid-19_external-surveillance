#' Cleaning CCM Outbreaks Data
#'
#' This function takes the raw CCM outbreaks data and cleans the field names, removes
#' empty fields, and coverts date objects to lubridate date objects.
#'
#' @param raw_ccm_outbreaks_data A tbl_df of our CCM outbreaks data
#'
#' @return A tbl_df of our cleaned CCM outbreaks data
#' @export
#'
#' @examples
#' cleaning_ccm_outbreaks_data(raw_ccm_outbreaks_data)
cleaning_ccm_outbreaks_data <- function(raw_ccm_outbreaks_data) {
  clean_ccm_outbreaks_data <- raw_ccm_outbreaks_data %>%
    janitor::remove_empty(which = "cols") %>%
    janitor::clean_names() %>%
    mutate(
      across(.cols = contains("date"), .fns = str_extract, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}"),
      across(.cols = contains("date"), .fns = lubridate::ymd)
    )

  return(clean_ccm_outbreaks_data)
}
