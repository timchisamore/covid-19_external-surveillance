#' Cleaning CCM Investigations Data
#'
#' This function takes the raw CCM investigations data and cleans the field names, removes
#' empty fields, and coverts date objects to lubridate date objects. Note, we had to manually
#' trim the time out of the field reported_date
#'
#' @param raw_ccm_investigations_data A tbl_df of our CCM Investigations data
#'
#' @return A tbl_df of our cleaned CCM investigations data
#' @export
#'
#' @examples
#' cleaning_ccm_investigations_data(raw_ccm_investigations_data)
cleaning_ccm_investigations_data <- function(raw_ccm_investigations_data) {
  clean_ccm_investigations_data <- raw_ccm_investigations_data %>%
    janitor::remove_empty(which = "cols") %>%
    janitor::clean_names() %>%
    mutate(
      reported_date = str_extract(reported_date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}"),
      across(.cols = contains("date"), .fns = lubridate::ymd)
    )

  return(clean_ccm_investigations_data)
}
