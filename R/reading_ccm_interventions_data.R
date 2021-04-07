#' Reading CCM interventions data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19
#' Interventions data into our R project from a local folder. Note that the
#' returned object will be a `tbl_df`.
#'
#' @param ccm_interventions_path A `character` representing the pathway to the CCM Interventions
#' data .csv file.
#'
#' @return A `tbl_df` of our CCM Interventions data.
#' @export
#'
#' @examples
#' `reading_ccm_interventions_data(here::here("data", "raw", "ccm_interventions_data.csv"))`
reading_ccm_interventions_data <- function(ccm_interventions_path) {
  raw_ccm_interventions_data <- read_csv(
    file = ccm_interventions_path,
    col_names = TRUE,
    col_types = cols(
      `Investigation Number` = col_character(),
      `Intervention: Record Type` = col_factor(),
      `Intervention` = col_character(),
      `Intervention Information` = col_character(),
      `Intervention - other` = col_character(),
      `Start Date` = col_date(format = "%Y-%m-%d"),
      `End Date` = col_date(format = "%Y-%m-%d"),
      `Hospital Name` = col_character()
    )
  )

  return(raw_ccm_interventions_data)
}
