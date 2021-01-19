#' Creating External Tableau Linelist Data
#'
#' This function takes the internal tableau linelist, which includes cases up
#' until the datetime the data was pulled, and filters for cases reported
#' until 4 PM the previous day.
#'
#' @param create_internal_tableau_linelist_data A `tbl_df` of internal tableau linelist data.
#' @param current_date A`Date` indicating the current date.
#'
#' @return A `tbl_df` of external tableau linelist data.
#' @export
#'
#' @examples
#' `creating_external_tableau_linelist_data(create_internal_tableau_linelist_data, lubridate::today())`
creating_external_tableau_linelist_data <- function(create_internal_tableau_linelist_data, current_date) {
  # this data is accruate up until 4 PM the previous day
  external_cutoff_date <- lubridate::ymd_hms(str_c(current_date + lubridate::days(x = -1), "16:00:00"))

  create_external_tableau_linelist_data <- create_internal_tableau_linelist_data %>%
    filter(reported_date <= external_cutoff_date)

  return(create_external_tableau_linelist_data)
}
