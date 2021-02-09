#' Getting Change in Case Count
#'
#' This function returns the change in cases between the current case count
#' and the previous case count and overwrites the old case count with the new
#' case count.
#'
#' @param get_tableau_linelist a tbl_df of COVID-19 linelist data
#'
#' @return a numeric indicating the change in cases
#' @export
#'
#' @examples
#' getting_change_in_case_count(get_tableau_linelist)
getting_change_in_case_count <- function(get_tableau_linelist) {
  old_case_count <- readRDS(here("data", "clean", "old_case_count.RDS"))
  new_case_count <- nrow(get_tableau_linelist)
  get_change_in_case_count <- new_case_count - old_case_count
  saveRDS(new_case_count, here("data", "clean", "old_case_count.RDS"))

  return(get_change_in_case_count)
}
