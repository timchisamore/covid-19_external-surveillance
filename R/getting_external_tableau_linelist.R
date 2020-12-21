#' Getting External Tableau Linelist
#' 
#' This function takes the internal tableau linelist, which includes cases up
#' until the datetime the data was pulled, and filters for cases reported
#' until 4 PM the previous day.
#'
#' @param get_internal_tableau_linelist a tbl_df of internal tableau linelist data
#'
#' @return a tbl_df of external tableau linelist data
#' @export
#'
#' @examples
#' getting_external_tableau_linelist(get_internal_tableau_linelist)
getting_external_tableau_linelist <- function(get_internal_tableau_linelist) {
  # this data is accruate up until 4 PM the previous day
  external_cutoff_date <- lubridate::ymd_hms(str_c(lubridate::today() + lubridate::days(x = -1), "16:00:00"))
  
  get_external_tableau_linelist <- get_internal_tableau_linelist %>%
    filter(reported_date <= external_cutoff_date)
  
  return(get_external_tableau_linelist)
}
