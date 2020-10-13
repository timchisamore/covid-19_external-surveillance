#' COVID-19 Demographics
#'
#' This function creates a summary table using the {gtsummary} package indicating the number and
#' proportion of cases by age group and gender, as well as the median and range of continuous age
#'
#' @param ccm_investigations_data A tbl_df of our cleaned CCM investigations data
#'
#' @return A table indicating the number and proportion of COVID-19 cases by demographic
#' @export
#'
#' @examples
#' covid_19_demographics(ccm_investigations_data)
covid_19_demographics <-
  function(clean_ccm_investigations_data) {
    table_4_data <- clean_ccm_investigations_data %>%
      mutate(
        age_at_illness = floor(lubridate::time_length(lubridate::interval(person_client_date_of_birth, episode_date), "years")),
        age_group_at_illness = santoku::chop(age_at_illness, breaks = c(20, 45, 65, Inf), labels = c("<20", "20-44", "45-64", "65+")),
        client_gender = str_to_title(person_client_gender)
      ) %>%
      select(
        age_at_illness,
        age_group_at_illness,
        client_gender
      )

    table_4 <- table_4_data %>%
      tbl_summary(
        label = list(
          age_at_illness ~ "Age",
          age_group_at_illness ~ "Age Group",
          client_gender ~ "Gender"
        ),
        type = list(
          c(
            age_group_at_illness,
            client_gender
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

    return(table_4)
  }
