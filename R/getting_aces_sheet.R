#' Getting ACES Sheet
#'
#' This function simply returns the tbl_df in our list of cleaned data that corresponds
#' to a specific sheet from our cleaned ACES data
#'
#' @param clean_aces_data a list of tbl_df of clean ACES data
#' @param sheet the name of the sheet we want to get
#'
#' @return A tbl_df of specific data from our list of clean data
#' @export
#'
#' @examples
#' getting_aces_sheet(clean_aces_data, "ed_visits")
getting_aces_sheet <- function(clean_aces_data, sheet) {
  get_aces_sheet <- pluck(clean_aces_data, sheet)
  return(get_aces_sheet)
}
