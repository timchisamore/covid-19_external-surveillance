#' Cleaning CCM Interventions Data
#'
#' This function takes the raw CCM interventions data and cleans the field names, removes
#' empty fields, and coverts date objects to lubridate date objects.
#'
#' @param raw_ccm_interventions_data A tbl_df of our CCM interventions data
#'
#' @return A tbl_df of our cleaned CCM interventions data
#' @export
#'
#' @examples
#' cleaning_ccm_interventions_data(raw_ccm_interventions_data)
cleaning_ccm_interventions_data <- function(raw_ccm_interventions_data) {
  clean_ccm_interventions_data <- raw_ccm_interventions_data %>%
    janitor::remove_empty(which = "cols") %>%
    janitor::clean_names() %>%
    mutate(
      across(.cols = contains("date"), .fns = str_extract, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}"),
      across(.cols = contains("date"), .fns = lubridate::ymd)
    )

  return(clean_ccm_interventions_data)
}
