#' Cleaning Municipal Conversion Data
#'
#' This function takes the raw municipal conversion data and cleans it by removing empty
#' columns, formatting the field names, and replacing _ with spaces in city names.
#'
#' @param raw_municipal_conversion_data a tbl_df of raw municipal conversion data
#'
#' @return a tbl_df of clean municipal conversion data
#' @export
#'
#' @examples
#' cleaning_municipal_conversion_data(raw_municipal_conversion_data)
cleaning_municipal_conversion_data <- function(raw_municipal_conversion_data) {
  clean_municipal_conversion_data <- raw_municipal_conversion_data %>%
    janitor::remove_empty(which = "cols") %>%
    janitor::clean_names() %>%
    mutate(township = str_replace_all(township, "_", " "))

  return(clean_municipal_conversion_data)
}
