#' Aggregating Admissions by Week and Syndrome
#'
#' This function takes the clean ACES admissions data and aggregates it by
#' week and syndrome to facilitate plotting. We use a start date of 2020/02/26
#' as we want to begin at this date.
#'
#' @param clean_aces_admissions_data a tbl_df of clean ACES admissions data
#'
#' @return a tbl_df of aggregates ACES admissions data
#' @export
#'
#' @examples
#' aggregating_admissions_by_week_and_syndrome(clean_aces_admissions_data)
aggregating_admissions_by_week_and_syndrome <- function(clean_aces_admissions_data) {
  aggregate_admissions_by_week_and_syndome <- clean_aces_admissions_data %>%
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

  return(aggregate_admissions_by_week_and_syndome)
}
