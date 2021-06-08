#' Getting Outbreak Related Investigation Numbers
#'
#' This function takes the clean CCM outbreakss data and returns the investigation
#' numbers of cases who were attached to an outbreak.
#'
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigations data
#' @param clean_ccm_outbreaks_data a tbl_df of clean CCM outbreak data
#'
#' @return a numeric vector of investigation numbers
#' @export
#'
#' @examples
#' getting_outbreak_related_investigation_numbers(clean_ccm_investigations_data, clean_ccm_outbreaks_dat)
getting_outbreak_related_investigation_numbers <- function(clean_ccm_investigations_data, clean_ccm_outbreaks_data) {
  # extracting all investigation numbers
  investigation_numbers <- clean_ccm_investigations_data %>%
    pull(investigation_number)

  # extracting all CCM investigation outbreak names
  outbreak_names <- clean_ccm_outbreaks_data %>%
    pull(outbreak_name) %>%
    unique()

  # with the migration of outbreaks from iPHIS into CCM, we no longer need to
  # specify between the two.
  get_outbreak_related_investigation_numbers <- clean_ccm_investigations_data %>%
    filter(investigation_outbreak %in% outbreak_names) %>%
    pull(investigation_number)

  # ensuring all of our generated investigation numbers are valid
  assert_investigation_numbers(investigation_numbers, get_outbreak_related_investigation_numbers)

  return(get_outbreak_related_investigation_numbers)
}
