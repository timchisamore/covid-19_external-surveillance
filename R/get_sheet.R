#' Get Sheet
#'
#' This function simply returns the tbl_df in our list of cleaned data that corresponds
#' to a specific sheet from our cleaned data
#'
#' @param clean_data a list of clean data
#' @param sheet the name of the sheet we want to get
#'
#' @return A tbl_df of specific data from our list of clean data
#' @export
#'
#' @examples
#' get_sheet(clean_iphis_data, "outcomes_5")
get_sheet <- function(clean_data, sheet) {
  sheet <- pluck(clean_data, sheet)
  return(sheet)
}
