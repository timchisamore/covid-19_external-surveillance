#' Assessments by Day
#'
#' This function creates a plot indicating the daily number of tests provided by assessment centres
#' in the region with a fill indicating weekday versus weekend.
#'
#' @param assessment_centre_data_cleaned A tbl_df of our cleaned assessment centre data for the region
#'
#' @return None
#' @export
#'
#' @examples
#' assessments_by_day(assessment_centre_data_cleaned)
assessments_by_day <- function(assessment_centre_data_cleaned) {
  palette <- c(
    rgb(
      red = 255,
      green = 192,
      blue = 0,
      maxColorValue = 255
    ),
    rgb(
      red = 0,
      green = 176,
      blue = 80,
      maxColorValue = 255
    )
  )

  assessment_centre_data_cleaned %>%
    ggplot(aes(
      x = date,
      y = total,
      fill = weekday
    )) +
    geom_col(width = 1) +
    scale_x_date(
      date_breaks = "1 week",
      labels = scales::date_format()
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = scales::comma_format()
    ) +
    scale_fill_manual(values = palette) +
    labs(
      title = "Total Daily Count for Assessment Centre Tests Completed",
      x = "Date",
      y = "Number of Tests Completed",
      fill = NULL,
      caption = "Source: Ontario Health East, Ontario MOHLTC, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))
}
