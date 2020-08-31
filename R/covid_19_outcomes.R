#' COVID-19 Outcomes
#'
#' This function creates a summary table using the {gtsummary} package indicating the number
#' and proportion of COVID-19 cases by outcome.
#'
#' @param clean_ccm_investigations_data A tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_outcomes_data A tbl_df of our cleaned CCM outcomes data
#'
#' @return A table indicating the number and proportion of COVID-19 cases by outcome
#' @export
#'
#' @examples
#' covid_19_outcomes(clean_ccm_investigations_data, clean_ccm_outcomes_data)
covid_19_outcomes <-
  function(clean_ccm_investigations_data,
           clean_ccm_outcomes_data) {
    # extracting the IDs of any recovered or residual effects cases
    recovered_ids <- generate_recovered_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)

    # extracting the IDs of any fatal cases
    fatal_ids <- generate_fatal_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)

    # extracting the IDs of any active cases
    active_ids <- generate_active_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)

    table_1_data <- clean_ccm_investigations_data %>%
      mutate(
        outcome = case_when(
          fct_match(
            as.character(investigation_number),
            recovered_ids
          ) ~ "Recovered",
          fct_match(as.character(investigation_number), fatal_ids) ~ "Fatal",
          fct_match(as.character(investigation_number), active_ids) ~ "Active",
          TRUE ~ "Unknown"
        )
      ) %>%
      select(outcome)

    table_1 <- table_1_data %>%
      tbl_summary(
        label = list(outcome ~ "Outcome"),
        type = list(outcome ~ "categorical"),
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

    return(table_1)
  }
