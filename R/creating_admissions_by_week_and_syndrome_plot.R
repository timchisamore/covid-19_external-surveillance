#' Creating Admissions by Week and Syndrome Plot
#'
#' This function takes the clean ACES admissions data and produces a plot
#' of the weekly admissions by week and syndrome. We use the
#' aggregating_admissions_by_week_and_sydrome function to.
#'
#' @param clean_aces_admissions_data A `tbl_df` of clean ACES admissions data.
#'
#' @return A `ggplot` of admissions by week and syndrome.
#' @export
#'
#' @examples
#' `creating_admissions_by_week_and_syndrome_plot(clean_aces_admissions_data)`
creating_admissions_by_week_and_syndrome_plot <-
  function(clean_aces_admissions_data) {
    # this palette was developed internally by staff
    palette <- getting_palette()

    create_admissions_by_week_and_syndrome_plot <-
      aggregating_admissions_by_week_and_syndrome(clean_aces_admissions_data) %>%
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
        title = "Weekly Respiratory-Related Emergent Hospital Admissions Trend for LGLDHU Hospitals by Syndrome",
        x = "\nWeek",
        y = "Count of Weekly Admissions by Syndrome\n",
        fill = NULL,
        caption = "Source: Acute Care Enhanced Surveillance (ACES), KFLA 2020, LGLDHU Data"
      ) +
      ggthemes::theme_economist_white() +
      theme(axis.text.x = element_text(angle = 90))

    return(create_admissions_by_week_and_syndrome_plot)
  }
