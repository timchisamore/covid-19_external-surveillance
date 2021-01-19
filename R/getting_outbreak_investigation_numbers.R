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
  outbreak_numbers <- clean_ccm_outbreaks_data %>%
    pull(outbreak_name) %>%
    unique()

  # we need to account for there being both iPHIS outbreak numbers and CCM outbreak names in the investigation
  # outbreak field. This will remove the cases with a sporadic outbreak number from iPHIS, remove the cases with
  # a null value from CCM, and remove those with an invalid value from CCM.
  get_outbreak_related_investigation_numbers <- clean_ccm_investigations_data %>%
    mutate(iphis_outbreak = if_else(str_detect(investigation_outbreak, pattern = "^[0-9]{4}-[0-9]{4}-[0-9]{3}$"), "Yes", "No")) %>%
    filter((iphis_outbreak == "Yes" & investigation_outbreak != "0000-2020-001") | (iphis_outbreak == "No" & investigation_outbreak %in% outbreak_numbers)) %>%
    pull(investigation_number)

  # ensuring all of our generated investigation numbers are valid
  assert_investigation_numbers(investigation_numbers, get_outbreak_related_investigation_numbers)

  return(get_outbreak_related_investigation_numbers)
}
