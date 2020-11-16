#' Getting Ever Ventilated
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case was ever ventilated.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case was ever ventilated
#' @export
#'
#' @examples
#' getting_ever_ventilated(investigation_number, clean_ccm_interventions_data)
getting_ever_ventilated <-
  function(investigation_number,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who were ever ventilated
    ever_ventilated_investigation_numbers <- clean_ccm_interventions_data %>%
      filter(
        intervention == "Intubated with invasive ventilation",
        intervention_information == "YES"
      ) %>%
      pull(investigation_number) %>%
      unique()
    
    get_ever_ventilated <- investigation_number %in% ever_ventilated_investigation_numbers
    get_ever_ventilated <- if_else(get_ever_ventilated, "Yes", "No")
    
    return(get_ever_ventilated)
  }
