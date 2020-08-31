#' COVID-19 Cases by Day
#'
#' This function creates a plot indicating the number of COVID-19 cases by reported date, using
#' CCM investigations data, with a fill indicating whether the case was a healthcare worker,
#' community case, or long-term care resident, again, from CCM risk factors data
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_risk_factors_data a tbl_df of our cleaned CCM risk factors data
#'
#' @return a plot indicating the number of cases by reported date and case type
#' @export
#'
#' @examples
#' covid_19_cases_by_day(clean_ccm_investigations_data, clean_ccm_risk_factors_data)
covid_19_cases_by_day <- function(clean_ccm_investigations_data, clean_ccm_risk_factors_data) {
  # extracting the IDs of any cases who are healthcare providers
  healthcare_provider_ids <-
    generate_healthcare_provider_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

  # extracting the IDs of any cases who were residents of retirement homes or long-term care homes
  long_term_care_ids <-
    generate_long_term_care_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

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

  figure_1_data <- clean_ccm_investigations_data %>%
    mutate(
      case_type = case_when(
        fct_match(as.character(investigation_number), healthcare_provider_ids) ~ "Healthcare Worker",
        fct_match(as.character(investigation_number), long_term_care_ids) ~ "Long Term Care Resident",
        TRUE ~ "Community"
      )
    ) %>%
    count(
      reported_date,
      case_type
    ) %>%
    complete(
      reported_date = seq(min(reported_date), lubridate::today(), by = "day"),
      case_type = unique(case_type),
      fill = list(n = 0)
    )

  figure_1 <- figure_1_data %>%
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
      date_breaks = "1 weeks",
      labels = scales::date_format()
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = scales::comma_format()
    ) +
    scale_fill_manual(values = palette) +
    labs(
      title = "Daily Count for COVID-19 Cases by Case Type",
      x = "Date Reported",
      y = "Number of Cases",
      fill = NULL,
      caption = "Source: Case and Contact Management (CCM), Ontario MOHLTC, LGLDHU Data"
    ) +
    ggthemes::theme_economist_white() +
    theme(axis.text.x = element_text(angle = 90))

  return(figure_1)
}
