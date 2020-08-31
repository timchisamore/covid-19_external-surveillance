#' Generate Active IDs
#'
#' This function takes the CCM outcomes data and generates a vector of case IDs corresponding to
#' those records where the case had an NA value in the field outcome. This implies that the case
#' has had no outcome and is thus active
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_outcomes_data a tbl_df of our cleaned CCM outcomes data
#'
#' @return a numeric vector of 7 digit case IDs
#' @export
#'
#' @examples
#' generate_active_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)
generate_active_ids <- function(clean_ccm_investigations_data, clean_ccm_outcomes_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  # extracting the investigation numbers of any active cases
  active_ids <- clean_ccm_outcomes_data %>%
    filter(is.na(outcome)) %>%
    pull(investigation_number)

  # ensuring all of our generated IDs are valid
  assert_generated_ids(investigation_numbers, active_ids)

  return(active_ids)
}
