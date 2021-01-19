#' Creating ED Visits by Date Plot
#'
#' This function takes clean ACES ED visits data and plots the aggregate ED visits by
#' date and a rolling 7-day average. It makes use of the aggregating_ed_visits_by_date
#' function.
#'
#' @param clean_aces_ed_visits_data A `tbl_df` of clean ACES ED visits data.
#'
#' @return A `ggplot` of aggregate ED visits by date.
#' @export
#'
#' @examples
#' `creating_ed_visits_by_date_plot(clean_aces_ed_visits_data)`
creating_ed_visits_by_date_plot <- function(clean_aces_ed_visits_data) {
  create_ed_visits_by_date_plot <- aggregating_ed_visits_by_date(clean_aces_ed_visits_data) %>%
    ggplot(aes(
      x = date,
      y = value,
      colour = measure
    )) +
    geom_line(lwd = 1.5) +
    scale_x_date(
      breaks = scales::date_breaks(),
      labels = scales::date_format()
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = scales::comma_format()
    ) +
    scale_colour_manual(
      values = c(
        rgb(
          red = 255,
          green = 0,
          blue = 0,
          maxColorValue = 255
        ),
        rgb(
          red = 31,
          green = 73,
          blue = 125,
          maxColorValue = 255
        )
      ),
      labels = c("7-Day Moving Average", "Total Respiratory ED Visits")
    ) +
    labs(
      title = "Daily Respiratory-Related Emergency Department Visit Trend for LGLDHU Hospitals",
      x = "\nDate",
      y = "Count of Daily ED Visits\n",
      colour = NULL,
      caption = "Source: Acute Care Enhanced Surveillance (ACES), KFLA 2020, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))

  return(create_ed_visits_by_date_plot)
}
