#' Creating Tableau Outbreaks Data
#'
#' This function takes the clean CCM outbreaks and investigations data and creates
#' a dataset that will be used in our Tableau framework.
#'
#' @param clean_ccm_outbreaks_data A `tbl_df` of clean CCM outbreaks data.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#'
#' @return A `tbl_df` of Tableau outbreaks data
#' @export
#'
#' @examples
#' `creating_tableau_outbreaks_data(clean_ccm_outbreaks_data, clean_ccm_investigations_data)`
creating_tableau_outbreaks_data <- function(clean_ccm_outbreaks_data, clean_ccm_investigations_data) {
  create_tableau_outbreaks_data <- joining_outbreaks_and_investigations_data(
    clean_ccm_outbreaks_data = clean_ccm_outbreaks_data,
    clean_ccm_investigations_data = clean_ccm_investigations_data
  ) %>%
    # the CCM extract differentiates iPHIS and CCM outbreak numbers so we use the CCM outbreak number if non-null
    # and the iPHIS outbreak number otherwise.
    mutate(
      outbreak_number = coalesce(outbreak_number, iphis_outbreak_number),
      .keep = "unused"
    ) %>%
    select(outbreak_number,
      outbreak_name,
      location_location_name,
      location_location_type,
      outbreak_status,
      disease,
      aetiologic_agent,
      subtype,
      onset_date_time_of_index_case,
      reported_date,
      onset_date_time_of_last_case,
      date_outbreak_declared_over,
      number_of_cases = n
    )

  return(create_tableau_outbreaks_data)
}
