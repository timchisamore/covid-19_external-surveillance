#' Creating Internal Tableau Linelist Data
#'
#' This function takes the clean CCM investigations, outcomes, interventions, and risk factors
#' data and creates a linelist intended for Tableau. This will be the internal report
#' that includes cases to the time the report was pulled.
#'
#' @param clean_ccm_investigations_data A `tbl_df` of clean CCM investigations data.
#' @param clean_ccm_outcomes_data A `tbl_df` of clean CCM outcomes data.
#' @param clean_ccm_interventions_data A `tbl_df` of clean CCM interventions data.
#' @param clean_ccm_risk_factors_data A `tbl_df` of clean CCM risk factors data.
#' @param clean_ccm_outbreaks_data A `tbl_df` of clean CCM outbreaks data.
#' @param clean_municipal_conversion_data A `tbl_df` of clean municipal conversion data.
#'
#' @return A `tbl_df` of internal linelist data for Tableau.
#' @export
#'
#' @examples
#' `creating_internal_tableau_linelist_data(clean_ccm_investigations_data, clean_ccm_outcomes_data, clean_ccm_interventions_data, clean_ccm_risk_factors_data, clean_ccm_outbreaks_data, clean_municipal_conversion_data)`
creating_internal_tableau_linelist_data <-
  function(clean_ccm_investigations_data,
           clean_ccm_outcomes_data,
           clean_ccm_interventions_data,
           clean_ccm_risk_factors_data,
           clean_ccm_outbreaks_data,
           clean_municipal_conversion_data) {
    create_internal_tableau_linelist_data <- clean_ccm_investigations_data %>%
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
        age_at_illness = creating_age_at_illness(person_client_date_of_birth, episode_date),
        age_group_at_illness = creating_age_group_at_illness(age_at_illness),
        adjusted_outcome = creating_adjusted_outcome(outcome, status),
        person_client_gender = fct_explicit_na(person_client_gender, na_level = "Unknown"),
        health_care_workers = creating_health_care_workers(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        ltch_or_rh_residents = creating_ltch_or_rh_residents(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        case_type = creating_case_type(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        ever_hospitalized = creating_ever_hospitalized(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        ever_in_icu = creating_ever_in_icu(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        ever_ventilated = creating_ever_ventilated(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        ever_interventions = creating_ever_interventions(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_hospitalized = creating_currently_hospitalized(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_in_icu = creating_currently_in_icu(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_ventilated = creating_currently_ventilated(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        currently_interventions = creating_currently_interventions(
          investigation_number,
          adjusted_outcome,
          clean_ccm_investigations_data,
          clean_ccm_interventions_data
        ),
        outbreak_related = creating_outbreak_related(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_outbreaks_data
        ),
        close_contact_related = creating_close_contact(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        household_contact_related = creating_household_contact(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        travel_related = creating_travel(
          investigation_number,
          clean_ccm_investigations_data,
          clean_ccm_risk_factors_data
        ),
        adjusted_epidemiologic_linkage = creating_adjusted_epidemiologic_linkage(
          epidemiologic_link_status,
          epidemiologic_linkage
        ),
        acquisition_type = combining_acquisition_type(
          epidemiologic_link_status,
          adjusted_epidemiologic_linkage,
          episode_date,
          outbreak_related,
          close_contact_related,
          household_contact_related,
          travel_related
        )
      ) %>%
      select(
        investigation_number,
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
        acquisition_type,
        investigation_subtype
      )

    return(create_internal_tableau_linelist_data)
  }
