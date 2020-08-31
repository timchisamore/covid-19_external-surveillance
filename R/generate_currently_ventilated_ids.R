#' Generate Currently Ventilated IDs
#'
#' This function takes the CCM interventions and outcomes data and generates a vector of case IDs
#' corresponding to those records where the cases are currently ventilated. We use the ever ventilated
#' IDs and the active IDs to remove cases with known outcomes as well as keeping cases with outstanding
#' or unknown ventilation dates
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_interventions_data a tbl_df of our cleaned CCM interventions data
#' @param clean_ccm_outcomes_data a tbl_df of our cleaned CCM outcomes data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_currently_ventilated_ids(clean_ccm_investigations_data, clean_ccm_interventions_data, clean_ccm_outcomes_data)
generate_currently_ventilated_ids <-
  function(clean_ccm_investigations_data,
           clean_ccm_interventions_data,
           clean_ccm_outcomes_data) {
    # extracting all investigation numbers
    investigation_numbers <- clean_ccm_investigations_data %>%
      pull(investigation_number)

    # extracting the investigation numbers of cases currently ventilated (only those with a
    # missing end date or whose end date exceeds the current date)
    currently_ventilated_ids <- clean_ccm_interventions_data %>%
      filter(
        fct_match(
          intervention,
          "Intubated with invasive ventilation"
        ) &
          fct_match(intervention_information, "YES") &
          as.character(investigation_number) %in% generate_active_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data) &
          # wanted to use
          # fct_match, but not all records have intervention data so can't
          is.na(end_date) |
          (lubridate::ymd(end_date) > lubridate::today())
      ) %>%
      pull(investigation_number)

    # ensuring all of our generated IDs are valid
    assert_generated_ids(investigation_numbers, currently_ventilated_ids)

    return(currently_ventilated_ids)
  }
