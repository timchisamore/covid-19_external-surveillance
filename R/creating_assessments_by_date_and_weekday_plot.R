#' Creating assessments by date and weekday plot
#'
#' This function creates a plot indicating the daily number of tests provided by
#' assessment centres in the region with a fill indicating weekday versus
#' weekend.
#'
#' @param clean_assessment_centre_data A tbl_df of our cleaned assessment centre
#' data for the region.
#'
#' @return A ggplot of the number of assessments by day and weekday.
#' @export
#'
#' @examples
#' `creating_assessments_by_date_and_weekday_plot(clean_assessment_centre_data)`
creating_assessments_by_date_and_weekday_plot <- function(clean_assessment_centre_data) {
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

  create_assessments_by_date_and_weekday_plot <- clean_assessment_centre_data %>%
    ggplot(aes(
      x = date,
      y = swabs_used,
      fill = weekday
    )) +
    geom_col(width = 1) +
    scale_x_date(
      breaks = scales::date_breaks(),
      labels = scales::date_format()
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = scales::comma_format()
    ) +
    scale_fill_manual(values = palette) +
    labs(
      title = "Total Daily Count for Assessment Centre Tests Completed",
      x = "\nDate",
      y = "Number of Tests Completed\n",
      fill = NULL,
      caption = "Source: Ontario Health East, Ontario MOHLTC, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))

  return(create_assessments_by_date_and_weekday_plot)
}
