#' Getting Ever ICU
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case was ever admitted to the ICU.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#'
#' @return a character vector indicating if the case was ever admitted to the ICU
#' @export
#'
#' @examples
#' getting_ever_icu(investigation_number, clean_ccm_interventions_data)
getting_ever_icu <-
  function(investigation_number,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who were ever admitted to the ICU
    ever_icu_investigation_numbers <- clean_ccm_interventions_data %>%
      filter(
        intervention == "ICU",
        intervention_information == "YES"
      ) %>%
      pull(investigation_number) %>%
      unique()
    
    get_ever_icu <- investigation_number %in% ever_icu_investigation_numbers
    get_ever_icu <- if_else(get_ever_icu, "Yes", "No")
    
    return(get_ever_icu)
  }
