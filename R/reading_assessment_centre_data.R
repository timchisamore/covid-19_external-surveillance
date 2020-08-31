#' Reading Assessment Centre Data
#'
#' This function reads in the testing data provided by assessment centres in the region, note that
#' the data is in a wide format by date
#'
#' @param path a pathway to the assessment centre data .xlsx file
#'
#' @return A tbl_df of assessment centre testing numbers in a wide format by date
#' @export
#'
#' @examples
#' reading_assessment_centre_data(here::here("data", "raw", "assessment_centre_data.xlsx"))
reading_assessment_centre_data <- function(path) {
  raw_assessment_centre_data <- readxl::read_xlsx(path = path)

  return(raw_assessment_centre_data)
}
