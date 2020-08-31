#' Cleaning Assessment Centre Data
#'
#' This function takes the assessment centre data and cleans it by removing empty columns,
#' formatting the field names, removing two unnecessary fields, creating a field to indicate
#' whether the date falls on a weekday or weekend, and pivoting too a long format.
#'
#' @param raw_assessment_centre_data A tbl_df of the assessment centre data that we need to clean
#'
#' @return A tbl_df of our cleaned and formatted assessment centre data
#' @export
#'
#' @examples
#' cleaning_assessment_centre_data(raw_assessment_centre_data)
cleaning_assessment_centre_data <-
  function(raw_assessment_centre_data) {
    clean_assessment_centre_data <- raw_assessment_centre_data %>%
      janitor::remove_empty(which = "cols") %>%
      janitor::clean_names() %>%
      select(-c(assessments_completed, test_completed)) %>%
      pivot_wider(
        names_from = community,
        values_from = number,
        values_fill = 0
      ) %>%
      rowwise() %>%
      transmute(date, total = sum(c_across(cols = c(`Smiths Falls`, Brockville, Almonte)))) %>%
      mutate(
        date = lubridate::ymd(date),
        weekday = case_when(
          lubridate::wday(date) %in% 2:6 ~ "Weekday",
          TRUE ~ "Weekend"
        )
      )

    return(clean_assessment_centre_data)
  }
