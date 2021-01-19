#' Combining Acquisition Type
#'
#' This function combines the acquisition types using the epidemiologic link status field to determine
#' whether we generate the value or use the epidemiologic linkage field. if we generate the data, episode date
#' is used to determine the logic. Prior to 2020/04/01, we use travel > outbreak related > close contact and
#' afterwards we use outbreak related > close contact > travel.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param episode_date A `date` vector of episode dates.
#' @param epidemiologic_link_status A `character` vector of epidemiologic link status.
#' @param epidemiologic_linkage A `character` vector of epidemiologic linkages.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#' @param clean_ccm_outbreaks_data A `tbl_df` of clean CCM outbreaks data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#'
#' @return A `character` vector of combined acquisition types.
#' @export
#'
#' @examples
#' `combining_acquisition_type(investigation_number, episode_date, epidemiologic_link_status, epidemiologic_linkage, clean_ccm_investigations_data, clean_ccm_outbreaks_data, clean_ccm_risk_factors_data)`
combining_acquisition_type <-
  function(investigation_number,
           episode_date,
           epidemiologic_link_status,
           epidemiologic_linkage,
           clean_ccm_investigations_data,
           clean_ccm_outbreaks_data,
           clean_ccm_risk_factors_data) {
    # lumping household contact into close contact
    epidemiologic_linkage <-
      str_replace_all(epidemiologic_linkage, "Household contact", "Close contact")

    # accounting for the new epidemiological link status field, if the record has a value, we use
    # the epidemiologic linkage, if the answer is no, we use no known epi-link, if the answer
    # is null, we use the getting_acquisition_type functions to determine it.
    combine_acquisition_type <- case_when(
      epidemiologic_link_status == "Yes" ~ epidemiologic_linkage,
      epidemiologic_link_status == "No" ~ "No known epi-link",
      is.na(epidemiologic_link_status) &
        episode_date < lubridate::ymd("2020-04-01") ~
      creating_early_acquisition_type(
        investigation_number,
        clean_ccm_investigations_data,
        clean_ccm_outbreaks_data,
        clean_ccm_risk_factors_data
      ),
      is.na(epidemiologic_link_status) &
        episode_date >= lubridate::ymd("2020-04-01") ~
      creating_late_acquisition_type(
        investigation_number,
        clean_ccm_investigations_data,
        clean_ccm_outbreaks_data,
        clean_ccm_risk_factors_data
      )
    )

    return(combine_acquisition_type)
  }
