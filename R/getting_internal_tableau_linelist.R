#' Getting Internal Tableau Linelist
#'
#' This function takes the clean CCM investigations, outcomes, interventions, and risk factors
#' data and produces a linelist intended for Tableau.This will be the internal report
#' that includes cases to the time the report was pulled.
#'
#' @param clean_ccm_investigations_data a tbl_df of clean CCM investigations data
#' @param clean_ccm_outcomes_data a tbl_df of clean CCM outcomes data
#' @param clean_ccm_interventions_data a tbl_df of clean CCM interventions data
#' @param clean_ccm_risk_factors_data a tbl_df of clean CCM risk factors data
#' @param clean_ccm_outbreaks_data a tbl_df of clean CCM outbreaks data
#' @param clean_municipal_conversion_data a tbl_df of clean municipal conversion data
#'
#' @return a tbl_df of linelist data for Tableau
#' @export
#'
#' @examples
#' getting_internal_tableau_linelist(clean_ccm_investigations_data, clean_ccm_outcomes_data, clean_ccm_interventions_data, clean_ccm_risk_factors_data, clean_ccm_outbreaks_data, clean_municipal_conversion_data)
getting_internal_tableau_linelist <-
  function(clean_ccm_investigations_data,
           clean_ccm_outcomes_data,
           clean_ccm_interventions_data,
           clean_ccm_risk_factors_data,
           clean_ccm_outbreaks_data,
           clean_municipal_conversion_data) {
    get_internal_tableau_linelist <- clean_ccm_investigations_data %>%
      left_join(clean_ccm_outcomes_data,
        by = "investigation_number"
      ) %>%
      left_join(clean_municipal_conversion_data,
        by = c(permanent_city_at_illness = "township")
      ) %>%
      left_join(clean_municipal_conversion_data,
        by = c(current_city_at_illness = "township")
      ) %>%
      rename(
        permanent_municipality_at_illness = "municipality.x",
        current_municipality_at_illness = "municipality.y",
        permanent_map_region_at_illness = "mapregion.x",
        current_map_region_at_illness = "mapregion.y"
      ) %>%
      mutate(
        permanent_municipality_at_illness = fct_explicit_na(permanent_municipality_at_illness, na_level = "Unknown"),
        current_municipality_at_illness = fct_explicit_na(current_municipality_at_illness, na_level = "Unknown"),
        permanent_map_region_at_illness = fct_explicit_na(permanent_map_region_at_illness, na_level = "Unknown"),
        current_map_region_at_illness = fct_explicit_na(current_map_region_at_illness, na_level = "Unknown"),
        age_at_illness = getting_age_at_illness(person_client_date_of_birth, episode_date),
        age_group_at_illness = getting_age_group_at_illness(age_at_illness),
        adjusted_outcome = getting_adjusted_outcome(outcome, status),
        person_client_gender = fct_explicit_na(person_client_gender, na_level = "Unknown"),
        health_care_workers = getting_health_care_workers(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        ltch_or_rh_residents = getting_ltch_or_rh_residents(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        case_type = getting_case_type(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        ever_hospitalized = getting_ever_hospitalized(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        ever_in_icu = getting_ever_in_icu(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        ever_ventilated = getting_ever_ventilated(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        ever_interventions = getting_ever_interventions(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_hospitalized = getting_currently_hospitalized(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_in_icu = getting_currently_in_icu(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_ventilated = getting_currently_ventilated(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_interventions = getting_currently_interventions(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        acquisition_type = combining_acquisition_type(
          investigation_number,
          episode_date,
          epidemiologic_link_status,
          epidemiologic_linkage,
          clean_ccm_investigations_data,
          clean_ccm_outbreaks_data,
          clean_ccm_risk_factors_data
        )
      ) %>%
      select(investigation_number,
             permanent_city_at_illness,
             current_city_at_illness,
             person_client_gender,
             episode_date,
             reported_date,
             investigation_start_date,
             permanent_municipality_at_illness,
             permanent_map_region_at_illness,
             current_municipality_at_illness,
             current_map_region_at_illness,
             age_at_illness,
             age_group_at_illness,
             adjusted_outcome,
             health_care_workers,
             ltch_or_rh_residents,
             case_type,
             ever_hospitalized,
             ever_in_icu,
             ever_ventilated,
             ever_interventions,
             currently_hospitalized,
             currently_in_icu,
             currently_ventilated,
             currently_interventions,
             acquisition_type)

    return(get_internal_tableau_linelist)
  }
