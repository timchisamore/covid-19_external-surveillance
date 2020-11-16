#' COVID-19 Interventions
#'
#' This function creates a summary table using the {gtsummary} package indicating the number and
#' proportion of cases ever or currently hospitalized, admitted to the ICU, or on a ventilator
#'
#' @param clean_ccm_investigations_data A tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_interventions_data A tbl_df of our cleaned CCM interventions data
#' @param clean_ccm_outcomes_data A tbl_df of our cleaned CCM outcomes data
#'
#' @return A table indicating the number and proportion of COVID-19 cases by intervention
#' @export
#'
#' @examples
#' covid_19_interventions(clean_ccm_investigations_data, clean_ccm_interventions_data, clean_ccm_outcomes_data)
covid_19_interventions <-
  function(clean_ccm_investigations_data,
           clean_ccm_interventions_data,
           clean_ccm_outcomes_data) {
    # extracting the IDs of any cases who were ever hospitalized
    ever_hospitalized_ids <-
      generate_ever_hospitalized_ids(clean_ccm_investigations_data, clean_ccm_interventions_data)

    # extracting the IDs of any cases who were ever ventilated
    ever_ventilated_ids <-
      generate_ever_ventilated_ids(clean_ccm_investigations_data, clean_ccm_interventions_data)

    # extracting the IDs of any cases who were ever admitted to the ICU
    ever_icu_ids <- generate_ever_icu_ids(clean_ccm_investigations_data, clean_ccm_interventions_data)

    # extracting the IDs of cases currently hospitalized (only those with a missing end date or whose end
    # date exceeds the current date)
    currently_hospitalized_ids <-
      generate_currently_hospitalized_ids(clean_ccm_investigations_data, clean_ccm_interventions_data, clean_ccm_outcomes_data)

    # extracting the IDs of cases currently ventilated (only those with a missing end date or whose end
    # date exceeds the current date)
    currently_ventilated_ids <-
      generate_currently_ventilated_ids(clean_ccm_investigations_data, clean_ccm_interventions_data, clean_ccm_outcomes_data)

    # extracting the IDs of cases currently in the ICU (only those with a missing end date or whose end
    # date exceeds the current date)
    currently_icu_ids <-
      generate_currently_icu_ids(clean_ccm_investigations_data, clean_ccm_interventions_data, clean_ccm_outcomes_data)

    table_3_data <- clean_ccm_investigations_data %>%
      mutate(
        ever_hospitalized = case_when(
          fct_match(as.character(investigation_number), ever_hospitalized_ids) ~ "Yes",
          TRUE ~ "No"
        ),
        ever_icu = case_when(
          fct_match(as.character(investigation_number), ever_icu_ids) ~ "Yes",
          TRUE ~ "No"
        ),
        ever_ventilated = case_when(
          fct_match(as.character(investigation_number), ever_ventilated_ids) ~ "Yes",
          TRUE ~ "No"
        ),
        currently_hospitalized = case_when(
          fct_match(as.character(investigation_number), currently_hospitalized_ids) ~ "Yes",
          TRUE ~ "No"
        ),
        currently_icu = case_when(
          fct_match(as.character(investigation_number), currently_icu_ids) ~ "Yes",
          TRUE ~ "No"
        ),
        currently_ventilated = case_when(
          fct_match(as.character(investigation_number), currently_ventilated_ids) ~ "Yes",
          TRUE ~ "No"
        )
      ) %>%
      select(
        ever_hospitalized,
        ever_icu,
        ever_ventilated,
        currently_hospitalized,
        currently_icu,
        currently_ventilated
      )

    table_3 <- table_3_data %>%
      tbl_summary(
        label = list(
          ever_hospitalized ~ "Ever Hospitalized",
          ever_icu ~ "Ever in ICU",
          ever_ventilated ~ "Ever Ventilated",
          currently_hospitalized ~ "Currently Hospitalized",
          currently_icu ~ "Currently in ICU",
          currently_ventilated ~ "Currently Ventilated"
        ),
        type = list(
          c(
            ever_hospitalized,
            ever_icu,
            ever_ventilated,
            currently_hospitalized,
            currently_icu,
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
      tab_footnote(footnote = "The hospital interventions we report on represent a gradient of severity, with hospitalization being the least severe and ventilation the most severe. To be ventilated, one must also be admitted to the ICU and thus also be hospitalized. However, someone can be hospitalized without being admitted to the ICU or ventilated. Therefore cases reported as being hospitalized may also appear as being in the ICU or ventilated at the same time.",
                   locations = cells_column_labels(columns = everything())) %>%
      tab_options(table.width = "80%")

    return(table_3)
  }
