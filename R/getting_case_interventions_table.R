#' Getting Case Interventions Table
#'
#' This function takes the COVID-19 linelist data and returns a {gt} table
#' indicating the number and proportion of cases ever or currently recieving
#' interventions.
#'
#' @param get_tableau_linelist a tbl_df of COVID-19 linelist data
#'
#' @return a {gt} table of COVID-19 case interventions
#' @export
#'
#' @examples
#' getting_case_interventions_table(get_tableau_linelist)
getting_case_interventions_table <- function(get_tableau_linelist) {
  get_case_interventions_table <- get_tableau_linelist %>%
    tbl_summary(
      include = c("ever_hospitalized", "ever_in_icu", "ever_ventilated", "currently_hospitalized", "currently_in_icu", "currently_ventilated"),
      label = list(
        ever_hospitalized ~ "Ever Hospitalized",
        ever_in_icu ~ "Ever in ICU",
        ever_ventilated ~ "Ever Ventilated",
        currently_hospitalized ~ "Currently Hospitalized",
        currently_in_icu ~ "Currently in ICU",
        currently_ventilated ~ "Currently Ventilated"
      ),
      type = list(
        c(
          ever_hospitalized,
          ever_in_icu,
          ever_ventilated,
          currently_hospitalized,
          currently_in_icu,
          currently_ventilated
        ) ~ "dichotomous"
      ),
      statistic = list(
        all_continuous() ~ "{median} ({min}, {max})",
        all_categorical() ~ "{n} ({p}%)"
      )
    ) %>%
    add_stat_label(label = list(all_continuous() ~ "median (min, max)")) %>%
    bold_labels() %>%
    modify_header(
      label = "**Table 3: Case Interventions**",
      update = list(stat_0 ~ "**Total = {N}**")
    ) %>%
    as_gt() %>%
    tab_style(
      style = cell_fill(color = "#EBEBEB"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_footnote(
      footnote = "The hospital interventions we report on represent a gradient of severity, with hospitalization being the least severe and ventilation the most severe. To be ventilated, one must also be admitted to the ICU and thus also be hospitalized. However, someone can be hospitalized without being admitted to the ICU or ventilated. Therefore cases reported as being hospitalized may also appear as being in the ICU or ventilated at the same time.",
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_options(table.width = "80%")

  return(get_case_interventions_table)
}
