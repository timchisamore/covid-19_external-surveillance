#' Reading Municipal Conversion Data
#'
#' This function reads in the municipal conversion data to map permanent and current
#' city at illness to their proper municipalities.
#'
#' @param path a pathway to the municipal conversion data .xlsx file
#'
#' @return a tbl_df of raw municipal conversion data
#' @export
#'
#' @examples
#' reading_municipal_conversion_data(here::here("data", "raw", "municipal_conversion_data.xlsx"))
reading_municipal_conversion_data <- function(path) {
  raw_municipal_conversion_data <- readxl::read_xlsx(path = path)

  return(raw_municipal_conversion_data)
}
