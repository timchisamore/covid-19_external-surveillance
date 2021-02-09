plan <- drake_plan(
  current_date = target(lubridate::today(), trigger = trigger(change = digest::digest(lubridate::today(), algo = "md5"))),
  raw_ccm_investigations_data = reading_ccm_investigations_data(file_in(!!here::here("data", "raw", "ccm_investigations_data.csv"))),
  raw_ccm_outcomes_data = reading_ccm_outcomes_data(file_in(!!here::here("data", "raw", "ccm_outcomes_data.csv"))),
  raw_ccm_interventions_data = reading_ccm_interventions_data(file_in(!!here::here("data", "raw", "ccm_interventions_data.csv"))),
  raw_ccm_risk_factors_data = reading_ccm_risk_factors_data(file_in(!!here::here("data", "raw", "ccm_risk_factors_data.csv"))),
  raw_ccm_outbreaks_data = reading_ccm_outbreaks_data(file_in(!!here::here("data", "raw", "ccm_outbreaks_data.csv"))),
  raw_municipal_conversion_data = reading_municipal_conversion_data(file_in(!!here::here("data", "raw", "municipal_conversion_data.xlsx"))),
  clean_ccm_investigations_data = cleaning_ccm_investigations_data(raw_ccm_investigations_data),
  clean_ccm_outcomes_data = cleaning_ccm_outcomes_data(raw_ccm_outcomes_data),
  clean_ccm_interventions_data = cleaning_ccm_interventions_data(raw_ccm_interventions_data),
  clean_ccm_risk_factors_data = cleaning_ccm_risk_factors_data(raw_ccm_risk_factors_data),
  clean_ccm_outbreaks_data = cleaning_ccm_outbreaks_data(raw_ccm_outbreaks_data),
  clean_municipal_conversion_data = cleaning_municipal_conversion_data(raw_municipal_conversion_data),
  create_internal_tableau_linelist_data = creating_internal_tableau_linelist_data(clean_ccm_investigations_data, clean_ccm_outcomes_data, clean_ccm_interventions_data, clean_ccm_risk_factors_data, clean_ccm_outbreaks_data, clean_municipal_conversion_data),
  create_external_tableau_linelist_data = creating_external_tableau_linelist_data(create_internal_tableau_linelist_data, current_date),
  internal_tableau_linelist_data = openxlsx::write.xlsx(create_internal_tableau_linelist_data, file_out(!!here::here("data", "clean", "internal_tableau_linelist.xlsx"))),
  external_tableau_linelist_data = openxlsx::write.xlsx(create_external_tableau_linelist_data, file_out(!!here::here("data", "clean", "tableau_linelist.xlsx"))),
  create_tableau_outbreaks_data = creating_tableau_outbreaks_data(clean_ccm_outbreaks_data, clean_ccm_investigations_data),
  tableau_outbreaks_data = openxlsx::write.xlsx(create_tableau_outbreaks_data, file_out(!!here::here("data", "clean", "tableau_outbreaks_data.xlsx"))),
  raw_aces_data = reading_aces_data(file_in(!!here::here("data", "raw", "aces_data.xlsx"))),
  clean_aces_data = cleaning_aces_data(raw_aces_data),
  get_aces_sheets = target(
    getting_aces_sheet(clean_aces_data, sheet = sheets),
    transform = map(
      sheets = c("ed_visits", "admissions"),
      .names = c("clean_aces_ed_visits_data", "clean_aces_admissions_data")
    )
  ),
  raw_assessment_centre_data = reading_assessment_centre_data(file_in(!!here::here("data", "raw", "assessment_centre_data.xlsx"))),
  clean_assessment_centre_data = cleaning_assessment_centre_data(raw_assessment_centre_data),
  # create_cases_by_reported_date_and_case_type_plot = creating_cases_by_reported_date_and_case_type_plot(create_external_tableau_linelist_data),
  # create_case_outcomes_table = creating_case_outcomes_table(create_external_tableau_linelist_data),
  # create_case_type_by_outcome_table = creating_case_type_by_outcome_table(create_external_tableau_linelist_data),
  # create_case_interventions_table = creating_case_interventions_table(create_external_tableau_linelist_data),
  # create_case_demographics_table = creating_case_demographics_table(create_external_tableau_linelist_data),
  # create_assessments_by_date_and_weekday_plot = creating_assessments_by_date_and_weekday_plot(clean_assessment_centre_data),
  # create_ed_visits_by_date_plot = creating_ed_visits_by_date_plot(clean_aces_ed_visits_data),
  # create_ed_visits_by_week_and_syndrome_plot = creating_ed_visits_by_week_and_syndrome_plot(clean_aces_ed_visits_data),
  # create_admissions_by_week_and_syndrome_plot = creating_admissions_by_week_and_syndrome_plot(clean_aces_admissions_data),
  # report = rmarkdown::render(
  #   input = knitr_in(!!here::here("documents", "covid_19_external_surveillance.Rmd")),
  #   output_file = file_out(!!here::here("documents", "covid_19_external_surveillance.html"))
  # )
)
