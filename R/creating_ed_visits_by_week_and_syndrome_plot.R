#' Creating ED Visits by Week and Syndrome Plot
#'
#' This function takes the clean ACES ED visits data and produces a plot of
#' aggregate ED visits by week and syndrome. It makes use of the
#' aggregating_ed_visits_by_week_and_syndrome function.
#'
#' @param clean_aces_ed_visits_data A `tbl_df` of clean ACES ED visits data.
#'
#' @return A `ggplot` of aggregate ED visits by week and syndrome.
#' @export
#'
#' @examples
#' `creating_ed_visits_by_week_and_syndrome_plot(clean_aces_ed_visits_data)`
creating_ed_visits_by_week_and_syndrome_plot <-
  function(clean_aces_ed_visits_data) {
    # this palette was developed internally by staff
    palette <- getting_palette()

    create_ed_visits_by_week_and_syndrome_plot <-
      aggregating_ed_visits_by_week_and_syndrome(clean_aces_ed_visits_data) %>%
      ggplot(aes(
        x = week,
        y = n,
        fill = syndrome,
        label = if_else(n > 0, n, NULL)
      )) +
      geom_col(position = position_stack()) +
      geom_text(position = position_stack(vjust = 0.5)) +
      scale_x_date(
        breaks = scales::date_breaks(),
        labels = scales::date_format()
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
        x = "\nWeek",
        y = "Count of Weekly ED Visits by Syndrome\n",
        fill = NULL,
        caption = "Source: Acute Care Enhanced Surveillance (ACES), KFLA 2020, LGLDHU Data"
      ) +
      ggthemes::theme_economist_white() +
      theme(axis.text.x = element_text(angle = 90))

    return(create_ed_visits_by_week_and_syndrome_plot)
  }
