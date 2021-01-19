#' Creating Cases by Reported Date and Case Type Plot
#'
#' This function takes the get_tableau_linelist data and produces a plot of the
#' number of COVID-19 cases by reported date and case ttype. It uses the
#' aggregating_cases_by_reported_date_and_case_type function to generate
#' the data.
#'
#' @param create_external_tableau_linelist_data A `tbl_df` of external Tableau
#' linelist data.
#'
#' @return A `ggplot` of cases by reported date and case type.
#' @export
#'
#' @examples
#' creating_cases_by_reported_date_and_case_type_plot(create_external_tableau_linelist_data)
creating_cases_by_reported_date_and_case_type_plot <- function(create_external_tableau_linelist_data) {
  palette <- c(
    rgb(
      red = 255,
      green = 192,
      blue = 0,
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
    )
  )

  create_cases_by_reported_date_and_case_type_plot <- aggregating_cases_by_reported_date_and_case_type(create_external_tableau_linelist_data) %>%
    ggplot(aes(
      x = reported_date,
      y = n,
      fill = case_type
    )) +
    geom_col(width = 1) +
    # annotate(
    #   "rect",
    #   xmin = max(figure_1_data$reported_date) - lubridate::days(14),
    #   xmax = max(figure_1_data$reported_date),
    #   ymin = -Inf,
    #   ymax = Inf,
    #   alpha = 0.2
    # ) +
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
      title = "Daily Count for COVID-19 Cases by Case Type",
      x = "\nDate Reported",
      y = "Number of Cases\n",
      fill = NULL,
      caption = "Source: Case and Contact Management (CCM), Ontario MOHLTC, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))

  return(create_cases_by_reported_date_and_case_type_plot)
}
