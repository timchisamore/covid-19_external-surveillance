#' Creating Case Outcomes Table
#'
#' This function takes the COVID-19 linelist data and produces a {gt} table
#' of the number and proportion of cases by outcome. Note, we use an adjusted
#' outcome that accounts for missing or pending information, see
#' getting_adjusted_outcome for more information.
#'
#' @param create_external_tableau_linelist_data A `tbl_df` of COVID-19 external Tableau linelist data.
#'
#' @return A `gt` table of COVID-19 case outcomes.
#' @export
#'
#' @examples
#' `creating_case_outcomes_table(create_external_tableau_linelist_data)`
creating_case_outcomes_table <- function(create_external_tableau_linelist_data) {
  create_case_outcomes_table <- create_external_tableau_linelist_data %>%
    tbl_summary(
      include = "adjusted_outcome",
      label = list(adjusted_outcome ~ "Outcome"),
      type = list(adjusted_outcome ~ "categorical"),
      statistic = list(
        all_continuous() ~ "{median} ({min}, {max})",
        all_categorical() ~ "{n} ({p}%)"
      )
    ) %>%
    add_stat_label(label = list(all_continuous() ~ "median (min, max)")) %>%
    bold_labels() %>%
    modify_header(
      label = "**Table 1: Case Outcomes**",
      update = list(stat_0 ~ "**Total = {N}**")
    ) %>%
    as_gt() %>%
    tab_style(
      style = cell_fill(color = "#EBEBEB"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_options(table.width = "80%")

  return(create_case_outcomes_table)
}
