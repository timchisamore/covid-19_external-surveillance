#' Creating Ever in ICU
#'
#' This function takes a vector of investigation numbers and clean CCM interventions data and returns
#' a vector indicating whether the case was ever in ICU.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_interventions_data A `tbl_df` of clean CCM interventions data.
#'
#' @return A `character` vector indicating if the case was ever in ICU.
#' @export
#'
#' @examples
#' `creating_ever_in_icu(investigation_number, clean_ccm_investigations_data, clean_ccm_interventions_data)`
creating_ever_in_icu <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting the investigation numbers of any cases who were ever in ICU
    ever_in_icu_investigation_numbers <- getting_ever_in_icu_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)

    create_ever_in_icu <- investigation_number %in% ever_in_icu_investigation_numbers
    create_ever_in_icu <- if_else(create_ever_in_icu, "Yes", "No")

    return(create_ever_in_icu)
  }
