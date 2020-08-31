#' Admissions by Week
#'
#' This function creates a plot indicating the weekly number of respiratory related hospital
#' admissions with a fill representing syndrome.
#'
#' @param admissions A tbl_df of ACES aggregate data indicating the number of hospital admissions by
#' date and syndrome
#'
#' @return None
#' @export
#'
#' @examples
#' admissions_by_week(admissions)
admissions_by_week <- function(admissions) {
  figure_5_data <- admissions %>%
    mutate(
      epi_week = lubridate::week(date),
      syndrome = fct_recode(
        syndrome,
        BRONCH = "BRONCH",
        CROUP = "CROUP",
        ILI = "ILI",
        PN = "PN",
        RESP = "Respiratory"
      )
    ) %>%
    count(
      epi_week,
      syndrome
    ) %>%
    complete(
      epi_week = unique(lubridate::week(admissions$date)),
      syndrome = c("BRONCH", "CROUP", "ILI", "PN", "RESP"),
      fill = list(n = 0)
    )

  date_labels <-
    tibble(date = seq(
      lubridate::ymd("2020-01-01"),
      lubridate::ymd("2020-12-31"),
      by = "day"
    )) %>%
    mutate(epi_week = lubridate::week(date)) %>%
    group_by(epi_week) %>%
    summarise(min_date = min(date), max_date = max(date)) %>%
    ungroup() %>%
    mutate(date_range = glue::glue("{format(min_date, '%b %d')} - {format(max_date, '%b %d')}")) %>%
    filter(epi_week %in% figure_5_data$epi_week)

  palette <- c(
    rgb(
      red = 149,
      green = 55,
      blue = 53,
      maxColorValue = 255
    ),
    rgb(
      red = 228,
      green = 108,
      blue = 10,
      maxColorValue = 255
    ),
    rgb(
      red = 0,
      green = 112,
      blue = 192,
      maxColorValue = 255
    ),
    rgb(
      red = 0,
      green = 176,
      blue = 80,
      maxColorValue = 255
    ),
    rgb(
      red = 255,
      green = 192,
      blue = 0,
      maxColorValue = 255
    )
  )

  figure_5 <- figure_5_data %>%
    ggplot(aes(
      x = epi_week,
      y = n,
      fill = syndrome,
      label = if_else(n > 0, n, NULL)
    )) +
    geom_col(position = position_stack()) +
    geom_text(position = position_stack(vjust = 0.5)) +
    scale_x_continuous(
      breaks = min(date_labels$epi_week):max(date_labels$epi_week),
      labels = unique(date_labels$date_range)
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = scales::comma_format()
    ) +
    scale_fill_manual(
      values = palette,
      labels = c(
        "BRONCHITIS",
        "CROUP",
        "INFLUENZA LIKE ILLNESS",
        "PNEUMONIA",
        "RESPIRATORY"
      )
    ) +
    labs(
      title = "Weekly Respiratory-Related Emergent Hospital Admissions Trend for LGLDHU Hospitals by Syndrome",
      x = "Week",
      y = "Count of Weekly Admissions by Syndrome",
      fill = NULL,
      caption = "Source: Acute Care Enhanced Surveillance (ACES), KFLA 2020, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))

  return(figure_5)
}
