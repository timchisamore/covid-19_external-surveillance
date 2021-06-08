#' Reading assessment centre data
#'
#' This function reads in the testing data provided by assessment centres in the
#' region.
#'
#' @param assessment_centre_path A character representing the pathway to
#' the assessment centre data .xlsx file.
#'
#' @return A tbl_df of assessment centre testing numbers by date.
#' @export
#'
#' @examples
#' `reading_assessment_centre_data(here::here("data", "raw", "assessment_centre_data.xlsx"))`
reading_assessment_centre_data <- function(assessment_centre_path) {
  raw_assessment_centre_data <- readxl::read_xlsx(path = assessment_centre_path)

  return(raw_assessment_centre_data)
}
