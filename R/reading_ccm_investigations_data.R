#' Reading CCM investigations data
#'
#' This function reads the Case and Contact Management (CCM) COVID-19
#' Investigations data into our R project from a local folder. Note that the
#' returned object will be a `tbl_df`.
#'
#' @param ccm_investigations_path A `character` representing the pathway to the
#' CCM Investigations data .csv file.
#'
#' @return A `tbl_df` of our CCM Investigations data.
#' @export
#'
#' @examples
#' `reading_ccm_investigations_data(here::here("data", "raw", "ccm_investigations_data.csv"))`
reading_ccm_investigations_data <-
  function(ccm_investigations_path) {
    raw_ccm_investigations_data <-
      read_csv(
        file = ccm_investigations_path,
        col_names = TRUE,
        col_types = cols(
          `Client ID` = col_character(),
          `IPHIS Client ID` = col_character(),
          `Investigation Number` = col_character(),
          `IPHIS Case ID` = col_character(),
          `Investigation Outbreak` = col_character(),
          `Disease` = col_character(),
          `Investigation Subtype` = col_character(),
          `Person Client: Gender` = col_factor(),
          `Person Client: Date of Birth` = col_date(format = "%Y-%m-%d"),
          `Permanent City at Illness` = col_character(),
          `Current City at Illness` = col_character(),
          `Investigation Record Type` = col_factor(),
          `Classification` = col_factor(),
          `Disposition` = col_factor(),
          `Closed Reason` = col_factor(),
          `Status` = col_character(),
          `Epidemiologic link Status` = col_character(),
          `Epidemiologic linkage` = col_character(),
          `Remote Positive` = col_factor(),
          `Re-Positive` = col_factor(),
          `Episode Date Type` = col_factor(),
          `Episode Date` = col_character(),
          `Earliest Symptom OnsetDate` = col_character(),
          `Earliest Positive Lab Collection Date` = col_character(),
          `Reported Date` = col_character(),
          `Opened Date` = col_date(format = "%Y-%m-%d"),
          `Closed Date` = col_date(format = "%Y-%m-%d"),
          `Investigation Start Date` = col_date(format = "%Y-%m-%d"),
          `Investigation Health Unit (RHU)` = col_factor()
        )
      )

    return(raw_ccm_investigations_data)
  }
