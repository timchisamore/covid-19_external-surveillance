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
      across(.cols = contains("date"), .fns = str_remove_all, pattern = "\\."),
      across(.cols = contains("date"), .fns = lubridate::parse_date_time, orders = c("%Y-%m-%d, %H:%M %p", "%Y-%m-%d"))
    )

  return(clean_ccm_interventions_data)
}
