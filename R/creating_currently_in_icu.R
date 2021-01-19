#' Creating Currently in ICU
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case is currentlyin ICU.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param adjusted_outcome A `character` vector of adjusted outcomes.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_interventions_data A `tbl_df` of clean CCM interventions data.
#'
#' @return A `character` vector indicating if the case is currently in ICU.
#' @export
#'
#' @examples
#' `creating_currently_in_icu(investigation_number, adjusted_outcome, clean_ccm_investigations_data, clean_ccm_interventions_data)`
creating_currently_in_icu <-
  function(investigation_number,
           adjusted_outcome,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who are currently in ICU
    currently_in_icu_investigation_numbers <- getting_currently_in_icu_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)

    create_currently_in_icu <- investigation_number %in% currently_in_icu_investigation_numbers
    create_currently_in_icu <- if_else(create_currently_in_icu & adjusted_outcome == "Active", "Yes", "No")

    return(create_currently_in_icu)
  }
