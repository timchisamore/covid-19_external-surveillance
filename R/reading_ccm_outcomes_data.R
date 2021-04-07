#' Reading CCM outcomes data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 outcomes
#' data into our R project from a local folder. Note that the returned object
#' will be a `tbl_df`.
#'
#' @param ccm_outcomes_path A `character` representing the pathway to the CCM
#' outcomes data .csv file.
#'
#' @return A `tbl_df` of our CCM outcomes data.
#' @export
#'
#' @examples
#' `reading_ccm_outcomes_data(here::here("data", "raw", "ccm_outcomes_data.csv"))`
reading_ccm_outcomes_data <- function(ccm_outcomes_path) {
  raw_ccm_outcomes_data <- read_csv(
    file = ccm_outcomes_path,
    col_names = TRUE,
    col_types = cols(
      `Investigation Number` = col_character(),
      `Outcome` = col_character(),
      `Outcome date` = col_date(format = "%Y-%m-%d"),
      `Type of Death` = col_factor(),
      `Date of Death` = col_date(format = "%Y-%m-%d")
    )
  )

  return(raw_ccm_outcomes_data)
}
