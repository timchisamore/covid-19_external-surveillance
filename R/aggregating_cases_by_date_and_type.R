#' Aggregating Cases by Reported Date and Case Type
#'
#' This function aggregates cases by reported date and case type to facilitate
#' plotting of this information.
#'
#' @param get_tableau_linelist a tbl_df of COVID-19 linelist data
#'
#' @return a tbl_df of aggregate cases by reported date and case type
#' @export
#'
#' @examples
#' aggregating_cases_by_reported_date_and_case_type(get_tableau_linelist)
aggregating_cases_by_reported_date_and_case_type <- function(get_tableau_linelist) {
  aggregate_cases_by_reported_date_and_case_type <- get_tableau_linelist %>%
    count(
      reported_date,
      case_type
    ) %>%
    # using padr to pad our data and filling the gaps with 0
    pad(
      interval = "day",
      end_val = lubridate::today(),
      by = "reported_date",
      group = c("case_type")
    ) %>%
    fill_by_value(value = 0)

  return(aggregate_cases_by_reported_date_and_case_type)
}
