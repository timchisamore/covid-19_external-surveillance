#' ED Visits by Week
#'
#' This function creates a plot indicating the weekly number of respiratory related ED visits
#' with a fill representing syndrome.
#'
#' @param ed_visits A tbl_df of ACES aggregate data indicating the number of ED visits by
#' date and syndrome
#'
#' @return None
#' @export
#'
#' @examples
#' ed_visits_by_week(ed_visits)
ed_visits_by_week <- function(ed_visits) {
  figure_4_data <- ed_visits %>%
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
      epi_week = unique(lubridate::week(ed_visits$date)),
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
    filter(epi_week %in% figure_4_data$epi_week)

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

  figure_4 <- figure_4_data %>%
    ggplot(aes(
      x = epi_week,
      y = n,
      fill = syndrome,
      label = if_else(n > 0, n, NULL)
    )) +
    geom_col() +
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
        "BRONCH" = "BRONCHITIS",
        "CROUP" = "CROUP",
        "ILI" = "INFLUENZA LIKE ILLNESS",
        "PN" = "PNEUMONIA",
        "RESP" = "RESPIRATORY"
      )
    ) +
    labs(
      title = "Weekly Respiratory-Related Emergency Department Visit Trend for LGLDHU Hospitals by Syndrome",
      x = "Week",
      y = "Count of Weekly ED Visits by Syndrome",
      fill = NULL,
      caption = "Source: Acute Care Enhanced Surveillance (ACES), KFLA 2020, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))

  return(figure_4)
}
