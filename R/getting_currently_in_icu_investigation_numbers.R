#' Getting Currently in ICU Investigation Numbers
#'
#' This function takes the CCM risk factors data and generates a vector of investigation numbers corresponding
#' to those records where the cases had an intervention of ICU currently.
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_interventions_data a tbl_df of our cleaned CCM interventions data
#'
#' @return a numeric vector of investigation numbers
#' @export
#'
#' @examples
#' getting_currently_in_icu_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)
getting_currently_in_icu_investigation_numbers <-
  function(clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting all investigation numbers
    investigation_numbers <- clean_ccm_investigations_data %>%
      pull(investigation_number)

    # extracting the investigation numbers of any cases who are currently in ICU
    get_currently_in_icu_investigation_numbers <- clean_ccm_interventions_data %>%
      filter(
        intervention == "ICU",
        intervention_information == "YES",
        (is.na(end_date) |
          (
            lubridate::ymd(end_date) > lubridate::today()
          ))
      ) %>%
      pull(investigation_number) %>%
      unique()

    # ensuring all of our generated investigation numbers are valid
    assert_investigation_numbers(investigation_numbers, get_currently_in_icu_investigation_numbers)

    return(get_currently_in_icu_investigation_numbers)
  }
