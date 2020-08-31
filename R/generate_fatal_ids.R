#' Generate Fatal IDs
#'
#' This function takes the CCM outcomes data and generates a vector of case IDs corresponding to
#' those records where the case had a fatal value in the field outcome
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_outcomes_data a tbl_df of our cleaned CCM outcomes data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_fatal_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)
generate_fatal_ids <- function(clean_ccm_investigations_data, clean_ccm_outcomes_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  # extracting the investigation numbers of any cases who have died
  fatal_ids <- clean_ccm_outcomes_data %>%
    filter(fct_match(outcome, "FATAL")) %>%
    pull(investigation_number)

  # ensuring all of our generated IDs are valid
  assert_generated_ids(investigation_numbers, fatal_ids)

  return(fatal_ids)
}
