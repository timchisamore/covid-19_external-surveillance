#' Reading CCM risk factors data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19 risk
#' factors data into our R project from a local folder. Note that the returned
#' object will be a `tbl_df`.
#'
#' @param ccm_risk_factors_path A `character` representing the pathway to the
#' CCM risk factors data .csv file.
#'
#' @return A `tbl_df` of our CCM risk factors data.
#' @export
#'
#' @examples
#' `reading_ccm_risk_factors_data(here::here("data", "raw", "ccm_risk_factors_data.csv"))`
reading_ccm_risk_factors_data <- function(ccm_risk_factors_path) {
  raw_ccm_risk_factors_data <- read_csv(
    file = ccm_risk_factors_path,
    col_names = TRUE,
    col_types = cols(
      `Investigation Number` = col_character(),
      `Record Type` = col_factor(),
      `Risk Factor` = col_character(),
      `Additional Risk Factor Information` = col_character(),
      `Risk Factor Notes` = col_character(),
      `Start Date` = col_date(format = "%Y-%m-%d"),
      `End Date` = col_date(format = "%Y-%m-%d")
    )
  )

  return(raw_ccm_risk_factors_data)
}
