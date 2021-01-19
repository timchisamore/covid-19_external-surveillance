#' Creating Outbreak Related
#'
#' This function takes a vector of investigation numbers and clean CCM outbreaks data and returns
#' a vector indicating whether the case was attached to an outbreak.
#'
#' @param investigation_number A `numeric` vector of investigation numbers.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigation data.
#' @param clean_ccm_outbreaks_data A `tbl_df` of clean CCM outbreak data.
#'
#' @return A `character` vector indicating if the case was linked to an outbreak.
#' @export
#'
#' @examples
#' `creating_outbreak_related(investigation_number, clean_ccm_investigations_data, clean_ccm_outbreaks_data)`
creating_outbreak_related <-
  function(investigation_number,
           clean_ccm_investigations_data,
           clean_ccm_outbreaks_data) {
    # extracting the investigation numbers of any cases who were linked to outbreaks
    outbreak_related_investigation_numbers <- getting_outbreak_related_investigation_numbers(clean_ccm_investigations_data, clean_ccm_outbreaks_data)

    create_outbreak_related <- investigation_number %in% outbreak_related_investigation_numbers
    create_outbreak_related <- if_else(create_outbreak_related, "Yes", "No")

    return(create_outbreak_related)
  }
