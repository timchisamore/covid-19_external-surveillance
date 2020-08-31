#' Reading ACES Data
#'
#' This function reads in the Acute Care Enhanced Surveillance (ACES) Emergency Department (ED)
#' and Admission (AD) data
#'
#' @param path a pathway to the ACES .xlsx file
#'
#' @return A list of tbl_df representing our ED and AD ACES data
#' @export
#'
#' @examples
#' reading_aces_data(here::here("data", "raw", "aces_data.xlsx"))
reading_aces_data <- function(path) {
  sheets <- readxl::excel_sheets(path = path)

  raw_aces_data <- map(sheets, readxl::read_xlsx, path = path) %>%
    set_names(janitor::make_clean_names(sheets))

  return(raw_aces_data)
}
