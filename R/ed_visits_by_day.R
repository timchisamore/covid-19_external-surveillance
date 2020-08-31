#' ED Visits by Day
#'
#' This function creates a plot indicating the daily number of respiratory related ED visits with a 7-day
#' rolling average to provide smoothing.
#'
#' @param ed_visits A tbl_df of ACES aggregate data indicating the number of ED visits by
#' date and syndrome
#'
#' @return None
#' @export
#'
#' @examples
#' ed_visits_by_day(ed_visits)
ed_visits_by_day <- function(ed_visits) {
  figure_3_data <- ed_visits %>%
    count(date) %>%
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

  figure_3 <- figure_3_data %>%
    ggplot(aes(
      x = date,
      y = value,
      colour = measure
    )) +
    geom_line(lwd = 1.5) +
    scale_x_date(
      date_breaks = "1 week",
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
      x = "Date",
      y = "Count of Daily ED Visits",
      colour = NULL,
      caption = "Source: Acute Care Enhanced Surveillance (ACES), KFLA 2020, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))

  return(figure_3)
}
