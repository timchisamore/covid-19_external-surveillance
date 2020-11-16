#' Getting Currently Hospitalized
#'
#' This function takes an investigation number, outcome, and the clean CCM interventions
#' data and determines whether the case is currently hospitalized.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param outcome a character vector of outcome values
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case is currently hospitalized
#' @export
#'
#' @examples
#' getting_currently_hospitalized(investigation_number, outcome, clean_ccm_interventions_data)
getting_currently_hospitalized <-
  function(investigation_number,
           outcome,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of cases currently hospitalized (only those with a
    # missing end date or whose end date exceeds the current date) and who have no outcome,
    # i.e., are active
    currently_hospitalized_investigation_numbers <-
      clean_ccm_interventions_data %>%
      filter(
        intervention == "Hospitalization",
        intervention_information == "YES",
        (is.na(end_date) |
           (
             lubridate::ymd(end_date) > lubridate::today()
           ))
      ) %>%
      pull(investigation_number) %>%
      unique()
    
    get_currently_hospitalized <-
      investigation_number %in% currently_hospitalized_investigation_numbers
    get_currently_hospitalized <-
      if_else(get_currently_hospitalized & outcome == "active", "Yes", "No")
    
    return(get_currently_hospitalized)
  }
