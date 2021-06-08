#' Cleaning assessment centre data
#'
#' This function takes the raw assessment centre data and cleans it by
#' formatting the field names, removing an uneccessary field, summarising by
#' date, and creating a field to indicate whether the date falls on a weekday or
#' weekend.
#'
#' @param raw_assessment_centre_data A tbl_df of the assessment centre data.
#'
#' @return A tbl_df of our cleaned and formatted assessment centre data.
#' @export
#'
#' @examples
#' `cleaning_assessment_centre_data(raw_assessment_centre_data)`
cleaning_assessment_centre_data <-
  function(raw_assessment_centre_data) {
    clean_assessment_centre_data <- raw_assessment_centre_data %>%
      janitor::clean_names() %>%
      select(-swabs_available) %>%
      mutate(date = lubridate::as_datetime(date)) %>%
      padr::thicken(by = "date",
                    interval = "day") %>%
      group_by(date_day) %>%
      summarise(swabs_used = sum(swabs_used, na.rm = TRUE),
                .groups = "drop") %>%
      padr::fill_by_value(swabs_used, value = 0) %>%
      rename(date = date_day) %>%
      mutate(
        weekday = case_when(
          between(lubridate::wday(date), 2, 6) ~ "Weekday",
          TRUE ~ "Weekend"
        )
      )

    return(clean_assessment_centre_data)
  }
