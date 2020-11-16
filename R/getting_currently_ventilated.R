#' Getting Currently Ventilated
#'
#' This function takes an investigation number, outcome, and the clean CCM interventions
#' data and determines whether the case is currently ventilated.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param outcome a character vector of outcome values
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case is currently ventilated
#' @export
#'
#' @examples
#' getting_currently_ventilated(investigation_number, outcome, clean_ccm_interventions_data)
getting_currently_ventilated <-
  function(investigation_number,
           outcome,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of cases currently ventilated (only those with a
    # missing end date or whose end date exceeds the current date) and who have no outcome,
    # i.e., are active
    currently_ventilated_investigation_numbers <-
      clean_ccm_interventions_data %>%
      filter(
        intervention == "Intubated with invasive ventilation",
        intervention_information == "YES",
        (is.na(end_date) |
           (
             lubridate::ymd(end_date) > lubridate::today()
           ))
      ) %>%
      pull(investigation_number) %>%
      unique()
    
    get_currently_ventilated <-
      investigation_number %in% currently_ventilated_investigation_numbers
    get_currently_ventilated <-
      if_else(get_currently_ventilated & outcome == "active", "Yes", "No")
    
    return(get_currently_ventilated)
  }
