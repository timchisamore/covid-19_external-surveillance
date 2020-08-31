#' Generate Recovered IDs
#'
#' This function takes the CCM outcomes data and generates a vector of case IDs corresponding to
#' those records where the case had either a recovered or residual effects value in the
#' field outcome
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_outcomes_data a tbl_df of our cleaned CCM outcomes data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_recovered_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)
generate_recovered_ids <- function(clean_ccm_investigations_data, clean_ccm_outcomes_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  # extracing the investigation numbers of any cases who have recovered
  recovered_ids <- clean_ccm_outcomes_data %>%
    filter(fct_match(outcome, "RECOVERED")) %>%
    pull(investigation_number)

  # extracting the investigation numbers of any cases who have residual effects
  residual_effects_ids <- clean_ccm_outcomes_data %>%
    filter(fct_match(outcome, "RES. EFFECTS")) %>%
    pull(investigation_number)

  # taking the union of recovered and residual efects IDs
  recovered_ids <- union(recovered_ids, residual_effects_ids)

  # ensuring all of our generated IDs are valid
  assert_generated_ids(investigation_numbers, recovered_ids)

  return(recovered_ids)
}
