#' Joining Outbreaks and Investigations Data
#'
#' This function takes the clean CCM outbreaks data and joins it to the aggregate
#' CCM investigations data by outbreak. We use a left join to remove any investigations
#' attached to outbreaks with an outbreak classification of suspect or does not meet as
#' the CCM outbreaks query only extracts outbreaks with an outbreak classifcation of
#' confirmed.
#'
#' @param clean_ccm_outbreaks_data A `tbl_df` of clean CCM outbreaks data.
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#'
#' @return A `tbl_df` of joined CCM outbreaks and investigations data.
#' @export
#'
#' @examples
#' `joining_outbreaks_and_investigations_data(clean_ccm_outbreaks_data, clean_ccm_investigations_data)`
joining_outbreaks_and_investigations_data <- function(clean_ccm_outbreaks_data, clean_ccm_investigations_data) {
  join_outbreaks_data_investigations_data <- clean_ccm_outbreaks_data %>%
    # a left join will remove investigations attached to outbreaks that have an outbreak classifcation of
    # suspect or does not meet.
    left_join(aggregating_investigations_by_outbreak(clean_ccm_investigations_data),
      by = c(outbreak_name = "investigation_outbreak")
    )

  return(join_outbreaks_data_investigations_data)
}
