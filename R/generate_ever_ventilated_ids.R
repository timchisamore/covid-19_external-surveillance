#' Generate Ever Ventilated IDs
#'
#' This function takes the CCM interventions data and generates a vector of case IDs corresponding
#' to those records where the cases were ever ventilated, i.e., cases that are currently or were
#' ventilated
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_interventions_data a tbl_df of our cleaned CCM interventions data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_ever_ventilated_ids(clean_ccm_investigations_data, clean_ccm_interventions_data)
generate_ever_ventilated_ids <-
  function(clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting all investigation numbers
    investigation_numbers <- clean_ccm_investigations_data %>%
      pull(investigation_number)

    # extracting the investigation numbers of any cases who were ever ventilated
    ever_ventilated_ids <- clean_ccm_interventions_data %>%
      filter(
        fct_match(
          intervention,
          "Intubated with invasive ventilation"
        ) &
          fct_match(intervention_information, "YES")
      ) %>%
      pull(investigation_number)

    # ensuring all of our generated IDs are valid
    assert_generated_ids(investigation_numbers, ever_ventilated_ids)

    return(ever_ventilated_ids)
  }
