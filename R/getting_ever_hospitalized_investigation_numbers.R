#' Getting Ever Hospitalized Investigation Numbers
#'
#' This function takes the CCM risk factors data and generates a vector of investigation numbers corresponding
#' to those records where the cases had an intervention of hospitalization at any time.
#'
#' @param clean_ccm_investigations_data a tbl_df of our cleaned CCM investigations data
#' @param clean_ccm_interventions_data a tbl_df of our cleaned CCM interventions data
#'
#' @return a numeric vector of investigation numbers
#' @export
#'
#' @examples
#' getting_ever_hospitalized_investigation_numbers(clean_ccm_investigations_data, clean_ccm_interventions_data)
getting_ever_hospitalized_investigation_numbers <-
  function(clean_ccm_investigations_data,
           clean_ccm_interventions_data) {
    # extracting all investigation numbers
    investigation_numbers <- clean_ccm_investigations_data %>%
      pull(investigation_number)

    # extracting the investigation numbers of any cases who were ever hospitalized
    get_ever_hospitalized_investigation_numbers <- clean_ccm_interventions_data %>%
      filter(
        intervention == "Hospitalization",
        intervention_information == "YES"
      ) %>%
      pull(investigation_number) %>%
      unique()

    # ensuring all of our generated investigation numbers are valid
    assert_investigation_numbers(investigation_numbers, get_ever_hospitalized_investigation_numbers)

    return(get_ever_hospitalized_investigation_numbers)
  }
