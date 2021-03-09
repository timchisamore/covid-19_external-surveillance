#' Reading CCM outbreaks data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 oubtreaks
#' data into our R project from a local folder. Note that the returned object
#' will be a `tbl_df`.
#'
#' @param ccm_outbreaks_path A `character` representing the pathway to the CCM
#' outbreaks data .csv file.
#'
#' @return A `tbl_df` of our CCM outbreaks data.
#' @export
#'
#' @examples
#' `reading_ccm_outbreaks_data(here::here("data", "raw", "ccm_outbreaks_data.csv"))`
reading_ccm_outbreaks_data <- function(ccm_outbreaks_path) {
  raw_ccm_outbreaks_data <- read_csv(
    file = ccm_outbreaks_path,
    col_names = TRUE,
    col_types = cols(
      `Outbreak Number` = col_character(),
      `IPHIS Outbreak Number` = col_character(),
      `Outbreak Name` = col_character(),
      `Location: Location Name` = col_character(),
      `Location: Location Type` = col_character(),
      `Outbreak Status` = col_factor(),
      `Disease Group` = col_factor(),
      `Disease` = col_factor(),
      `Aetiologic Agent` = col_factor(),
      `Outbreak Subtype` = col_factor(),
      `Onset Date/Time of Index Case` = col_date(format = "%Y-%m-%d"),
      `Reported Date` = col_date(format = "%Y-%m-%d"),
      `Outbreak Declared Date` = col_date(format = "%Y-%m-%d"),
      `Onset Date/Time of Last Case` = col_date(format = "%Y-%m-%d"),
      `Date Outbreak Declared Over` = col_date(format = "%Y-%m-%d")
    )
  )

  return(raw_ccm_outbreaks_data)
}
