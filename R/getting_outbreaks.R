#' Getting Outbreaks
#'
#' This function takes a vector of investigation numbers and clean CCM outbreaks data and returns
#' a vector indicating whether the case was attached to an outbreak.
#'
#' @param investigation_number a numeric vector of investigation numbers
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigation data
#' @param clean_ccm_outbreaks_data a tbl_df of clean CCM outbreak data
#'
#' @return a character vector indicating if the case was linked to an outbreak
#' @export
#'
#' @examples
#' getting_outbreaks(investigation_number, clean_ccm_investigations_data, clean_ccm_outbreaks_data)
getting_outbreaks <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_outbreaks_data) {
    # extracting the investigation numbers of any cases who were linked to outbreaks
    outbreak_investigation_numbers <- getting_outbreak_investigation_numbers(clean_ccm_investigations_data, clean_ccm_outbreaks_data)

    get_outbreaks <- investigation_number %in% outbreak_investigation_numbers
    get_outbreaks <- if_else(get_outbreaks, "Yes", "No")

    return(get_outbreaks)
  }
