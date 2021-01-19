#' Aggregating Investigations by Outbreak
#'
#' This function takes the clean CCM investigations outbreak data and aggregates it
#' by outbreak. Note, some of these will not be confirmed outbreaks due to a change
#' in practices in CCM versus iPHIS.
#'
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#'
#' @return A `tbl_df` of aggregate CCM investigations data by outbreak.
#' @export
#'
#' @examples
#' `aggregating_investigations_by_outbreak(clean_ccm_investigations_data`
aggregating_investigations_by_outbreak <- function(clean_ccm_investigations_data) {
  aggregate_investigations_by_outbreak <- clean_ccm_investigations_data %>%
    count(investigation_outbreak)

  return(aggregate_investigations_by_outbreak)
}
