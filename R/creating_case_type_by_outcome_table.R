#' Creating Case Type by Outcome Table
#'
#' This functions takes the COVID-19 linelist data and produces a `gt` table
#' with the number and proportion of cases by outcome and case type.
#'
#' @param create_external_tableau_linelist_data A `tbl_df` of COVID-19 external Tableau linelist data.
#'
#' @return A `gt`}table of cases by outcome and case type.
#' @export
#'
#' @examples
#' `creating_case_type_by_outcome_table(create_external_tableau_linelist_data)`
creating_case_type_by_outcome_table <-
  function(create_external_tableau_linelist_data) {
    create_case_type_by_outcome_table <- create_external_tableau_linelist_data %>%
      tbl_summary(
        include = c("adjusted_outcome", "case_type"),
        by = adjusted_outcome,
        label = case_type ~ "Case Type",
        type = case_type ~ "categorical",
        statistic = list(
          all_continuous() ~ "{median} ({min}, {max})",
          all_categorical() ~ "{n} ({p}%)"
        )
      ) %>%
      add_overall(last = TRUE) %>%
      add_stat_label(label = list(all_continuous() ~ "median (min, max)")) %>%
      bold_labels() %>%
      modify_header(
        label = "**Table 2: Case Types by Case Outcomes**",
        update = list(stat_0 ~ "**Overall**"),
        stat_by = "**{level}**"
      ) %>%
      as_gt() %>%
      tab_spanner(
        label = "Case Outcomes",
        columns = starts_with("stat_")
      ) %>%
      tab_style(
        style = cell_fill(color = "#EBEBEB"),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = cell_fill(color = "#EBEBEB"),
        locations = cells_column_spanners(spanners = "Case Outcomes")
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_spanners(spanners = "Case Outcomes")
      ) %>%
      tab_options(table.width = "80%")

    return(create_case_type_by_outcome_table)
  }
