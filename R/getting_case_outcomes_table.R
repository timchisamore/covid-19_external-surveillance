#' Getting Case Outcomes Table
#'
#' This function takes the COVID-19 linelist data and produces a {gt} table
#' of the number and proportion of cases by outcome. Note, we use an adjusted
#' outcome that accounts for missing or pending information, see
#' getting_adjusted_outcome for more information.
#'
#' @param get_tableau_linelist a tbl_df of COVID-19 linelist data
#'
#' @return a {gt} table of COVID-19 case outcomes
#' @export
#'
#' @examples
#' getting_case_outcomes_table(get_tableau_linelist)
getting_case_outcomes_table <- function(get_tableau_linelist) {
  get_case_outcomes_table <- get_tableau_linelist %>%
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

  return(get_case_outcomes_table)
}
