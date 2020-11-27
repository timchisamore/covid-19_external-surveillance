#' Aggregating ED Visits by Week and Syndrome
#'
#' This function uses the clean ACES ED visits data to produce aggregate
#' ED visits by week and syndrome. Note that we start from 2020/02/26
#' to match previous plots.
#'
#' @param clean_aces_ed_visits_data a tbl_df of clean ACES ED visits data
#'
#' @return a tbl_df of aggregate ED visits by week and syndrome
#' @export
#'
#' @examples
#' aggregating_ed_visits_by_week_and_syndrome(clean_aces_ed_visits_data)
aggregating_ed_visits_by_week_and_syndrome <- function(clean_aces_ed_visits_data) {
  aggregate_ed_visits_by_week_and_syndrome <- clean_aces_ed_visits_data %>%
    mutate(
      syndrome = fct_recode(
        syndrome,
        BRONCH = "BRONCH",
        CROUP = "CROUP",
        ILI = "ILI",
        PN = "PN",
        RESP = "Respiratory"
      )
    ) %>%
    thicken(
      interval = "week",
      start_val = lubridate::ymd("2020/02/26"),
      colname = "week",
      by = "date"
    ) %>%
    count(
      week,
      syndrome
    ) %>%
    pad(
      interval = "week",
      start_val = lubridate::ymd("2020/02/26"),
      by = "week",
      group = "syndrome"
    ) %>%
    fill_by_value(value = 0)

  return(aggregate_ed_visits_by_week_and_syndrome)
}
