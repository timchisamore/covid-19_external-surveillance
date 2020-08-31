#' Cleaning ACES Data
#'
#' This function cleans the Acute Care Enhanced Surveillance (ACES) data by using functions
#' from the janitor package and by formatting dates correctly. Further, it puts the data into a
#' tidy format.
#'
#' @param raw_aces_data A list of our raw ACES data where each sheet from the raw file is
#' an element of the list
#'
#' @return A list of tbl_df that has been cleaned and represents Emergency Department (ED) and
#' Admission (AD) data by date and syndrome
#' @export
#'
#' @examples
#' cleaning_aces_data(raw_aces_data)
cleaning_aces_data <- function(raw_aces_data) {
  clean_aces_data <- raw_aces_data %>%
    map(janitor::remove_empty, which = "cols") %>%
    map(janitor::clean_names) %>%
    map(
      ~ mutate(.data = .x, date = as.Date(date), date = lubridate::ymd(date))
    ) %>%
    map(
      ~ mutate(
        .data = .x, time = str_extract(time, "[0-9]{2}:[0-9]{2}:[0-9]{2}"),
        time = lubridate::hms(time)
      )
    )

  return(clean_aces_data)
}
