#' Cleaning CCM Outcomes Data
#'
#' This function takes the raw CCM outcomes data and cleans the field names, removes
#' empty fields, and coverts date objects to lubridate date objects.
#'
#' @param raw_ccm_outcomes_data A tbl_df of our CCM outcomes data
#'
#' @return A tbl_df of our cleaned CCM outcomes data
#' @export
#'
#' @examples
#' cleaning_ccm_outcomes_data(raw_ccm_outcomes_data)
cleaning_ccm_outcomes_data <- function(raw_ccm_outcomes_data) {
  clean_ccm_outcomes_data <- raw_ccm_outcomes_data %>%
    janitor::remove_empty(which = "cols") %>%
    janitor::clean_names() %>%
    mutate(
      across(.cols = contains("date"), .fns = str_remove_all, pattern = "\\."),
      across(.cols = contains("date"), .fns = lubridate::parse_date_time, orders = c("%Y-%m-%d, %H:%M %p", "%Y-%m-%d"))
    )

  return(clean_ccm_outcomes_data)
}
