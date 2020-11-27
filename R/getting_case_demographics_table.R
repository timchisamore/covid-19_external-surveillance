#' Getting Case Demographics Table
#'
#' This function takes the COVID-19 tableau linelist and produces a table of the case demographics, including
#' median and range of numeric age, number and proportion of age groups, and number and proportion of gender.
#' Note that we use larger age groupings than in the COVID-19 tableau linelist data.
#'
#' @param get_tableau_linelist a tbl_df of COVID-19 linelist data
#'
#' @return a {gt} object of COVID-19 case demographics
#' @export
#'
#' @examples
#' getting_case_demographics_table(get_tableau_linelist)
getting_case_demographics_table <- function(get_tableau_linelist) {
  get_case_demographics_table <- get_tableau_linelist %>%
    mutate(age_group_at_illness = fct_collapse(age_group_at_illness,
      `<20` = c("00-04", "05-09", "10-14", "15-19"),
      `20-44` = c("20-24", "25-29", "30-34", "35-39", "40-44"),
      `45-64` = c("45-49", "50-54", "55-59", "60-64"),
      other_level = "65+"
    )) %>%
    tbl_summary(
      include = c("age_at_illness", "age_group_at_illness", "person_client_gender"),
      label = list(
        age_at_illness ~ "Age",
        age_group_at_illness ~ "Age Group",
        person_client_gender ~ "Gender"
      ),
      type = list(
        c(
          age_group_at_illness,
          person_client_gender
        ) ~ "categorical",
        age_at_illness ~ "continuous"
      ),
      statistic = list(
        all_continuous() ~ "{median} ({min}, {max})",
        all_categorical() ~ "{n} ({p}%)"
      )
    ) %>%
    add_stat_label(label = list(all_continuous() ~ "median (min, max)")) %>%
    bold_labels() %>%
    modify_header(
      label = "**Table 4: Case Demographics**",
      update = list(stat_0 ~ "**Total = {N}**")
    ) %>%
    as_gt() %>%
    tab_style(
      style = cell_fill(color = "#EBEBEB"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_options(table.width = "80%")

  return(get_case_demographics_table)
}
