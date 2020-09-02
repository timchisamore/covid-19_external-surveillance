#' COVID-19 Outcomes by Type
#'
#' This function creates a summary table using the {gtsummary} package indicating the number
#' and proportion of COVID-19 cases by outcome and case type.
#'
#' @param clean_ccm_investigations_data A tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_risk_factors_data A tbl_df of our cleaned CCM risk factors data
#' @param clean_ccm_outcomes_data A tbl_df of our cleaned CCM outcomes data
#'
#' @return A table indicating the number and proportion of COVID-19 cases by outcome and case type
#' @export
#'
#' @examples
#' covid_19_outcomes_by_type(clean_ccm_investigations_data, clean_ccm_risk_factors_data, clean_ccm_outcomes_data)
covid_19_outcomes_by_type <-
  function(clean_ccm_investigations_data,
           clean_ccm_risk_factors_data,
           clean_ccm_outcomes_data) {
    # extracting the IDs of any cases who are healthcare providers
    healthcare_provider_ids <-
      generate_healthcare_provider_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    # extracting the IDs of any cases who reported close contact with a case
    close_contact_ids <-
      generate_close_contact_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    # extracting the IDs of any cases who reported travel preceeding symptoms
    travel_associated_ids <-
      generate_travel_associated_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    # extracting the IDs of any cases who were residents of retirement homes or long-term care homes
    long_term_care_ids <-
      generate_long_term_care_ids(clean_ccm_investigations_data, clean_ccm_risk_factors_data)

    # extracing the IDs of any recovered or residual effects cases
    recovered_ids <- generate_recovered_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)

    # extracting the IDs of any fatal cases
    fatal_ids <- generate_fatal_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)

    # extracting the IDs of any active cases
    active_ids <- generate_active_ids(clean_ccm_investigations_data, clean_ccm_outcomes_data)

    table_2_data <- clean_ccm_investigations_data %>%
      mutate(
        case_type = case_when(
          fct_match(as.character(investigation_number), healthcare_provider_ids) ~ "Healthcare Worker",
          fct_match(as.character(investigation_number), long_term_care_ids) ~ "Long Term Care or Retirement Home Resident",
          TRUE ~ "Community"
        ),
        outcome = case_when(
          fct_match(as.character(investigation_number), recovered_ids) ~ "Recovered",
          fct_match(as.character(investigation_number), fatal_ids) ~ "Fatal",
          fct_match(as.character(investigation_number), active_ids) ~ "Active",
          TRUE ~ "Unknown"
        )
      ) %>%
      select(
        case_type,
        outcome
      )

    table_2 <- table_2_data %>%
      tbl_summary(
        by = outcome,
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

    return(table_2)
  }
