#' Aggregating ED Visits by Date
#'
#' This function aggregates the clean ACES ED visits data by date and computes a rolling 7-day
#' average to facilitate plotting.
#'
#' @param clean_aces_ed_visits_data a tbl_df of clean ACES ED visits data
#'
#' @return a tbl_df of aggregate ED visits by date
#' @export
#'
#' @examples
#' aggregating_ed_visits_by_date(clean_aces_ed_visits_data)
aggregating_ed_visits_by_date <- function(clean_aces_ed_visits_data) {
  aggregate_ed_visits_by_date <- clean_aces_ed_visits_data %>%
    count(date) %>%
    pad(
      interval = "day",
      by = "date"
    ) %>%
    fill_by_value(value = 0) %>%
    mutate(
      moving_average = slider::slide_dbl(
        .x = n,
        .f = mean,
        .before = 6,
        .after = 0,
        .step = 1,
        .complete = TRUE
      )
    ) %>%
    pivot_longer(
      cols = c(n, moving_average),
      names_to = "measure",
      values_to = "value"
    )

  return(aggregate_ed_visits_by_date)
}
