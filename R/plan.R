plan <- drake_plan(
  raw_ccm_investigations_data = reading_ccm_investigations_data(file_in(!!here("data", "raw", "ccm_investigations_data.csv"))),
  raw_ccm_outcomes_data = reading_ccm_outcomes_data(file_in(!!here("data", "raw", "ccm_outcomes_data.csv"))),
  raw_ccm_interventions_data = reading_ccm_interventions_data(file_in(!!here("data", "raw", "ccm_interventions_data.csv"))),
  raw_ccm_risk_factors_data = reading_ccm_risk_factors_data(file_in(!!here("data", "raw", "ccm_risk_factors_data.csv"))),
  raw_ccm_outbreaks_data = reading_ccm_outbreaks_data(file_in(!!here("data", "raw", "ccm_outbreaks_data.csv"))),
  raw_municipal_conversion_data = reading_municipal_conversion_data(file_in(!!here("data", "raw", "municipal_conversion_data.xlsx"))),
  clean_ccm_investigations_data = cleaning_ccm_investigations_data(raw_ccm_investigations_data),
  clean_ccm_outcomes_data = cleaning_ccm_outcomes_data(raw_ccm_outcomes_data),
  clean_ccm_interventions_data = cleaning_ccm_interventions_data(raw_ccm_interventions_data),
  clean_ccm_risk_factors_data = cleaning_ccm_risk_factors_data(raw_ccm_risk_factors_data),
  clean_ccm_outbreaks_data = cleaning_ccm_outbreaks_data(raw_ccm_outbreaks_data),
  clean_municipal_conversion_data = cleaning_municipal_conversion_data(raw_municipal_conversion_data),
  get_internal_tableau_linelist = getting_internal_tableau_linelist(clean_ccm_investigations_data, clean_ccm_outcomes_data, clean_ccm_interventions_data, clean_ccm_risk_factors_data, clean_ccm_outbreaks_data, clean_municipal_conversion_data),
  get_external_tableau_linelist = getting_external_tableau_linelist(get_internal_tableau_linelist),
  internal_tableau_linelist = openxlsx::write.xlsx(get_internal_tableau_linelist, file_out(!!here("data", "clean", "internal_tableau_linelist.xlsx"))),
  external_tableau_linelist = openxlsx::write.xlsx(get_external_tableau_linelist, file_out(!!here("data", "clean", "external_tableau_linelist.xlsx"))),
  raw_aces_data = reading_aces_data(file_in(!!here("data", "raw", "aces_data.xlsx"))),
  clean_aces_data = cleaning_aces_data(raw_aces_data),
  get_aces_sheets = target(
    getting_aces_sheet(clean_aces_data, sheet = sheets),
    transform = map(
      sheets = c("ed_visits", "admissions"),
      .names = c("clean_aces_ed_visits_data", "clean_aces_admissions_data")
    )
  ),
  raw_assessment_centre_data = reading_assessment_centre_data(file_in(!!here("data", "raw", "assessment_centre_data.xlsx"))),
  clean_assessment_centre_data = cleaning_assessment_centre_data(raw_assessment_centre_data),
  get_cases_by_reported_date_and_case_type_plot = getting_cases_by_reported_date_and_case_type_plot(get_external_tableau_linelist),
  get_case_outcomes_table = getting_case_outcomes_table(get_external_tableau_linelist),
  get_case_type_by_outcome_table = getting_case_type_by_outcome_table(get_external_tableau_linelist),
  get_case_interventions_table = getting_case_interventions_table(get_external_tableau_linelist),
  get_case_demographics_table = getting_case_demographics_table(get_external_tableau_linelist),
  get_assessments_by_date_and_weekday_plot = getting_assessments_by_date_and_weekday_plot(clean_assessment_centre_data),
  get_ed_visits_by_date_plot = getting_ed_visits_by_date_plot(clean_aces_ed_visits_data),
  get_ed_visits_by_week_and_syndrome_plot = getting_ed_visits_by_week_and_syndrome_plot(clean_aces_ed_visits_data),
  get_admissions_by_week_and_syndrome_plot = getting_admissions_by_week_and_syndrome_plot(clean_aces_admissions_data),
  report = rmarkdown::render(
    input = knitr_in(!!here("documents", "covid_19_external_surveillance.Rmd")),
    output_file = file_out(!!here("documents", "covid_19_external_surveillance.html"))
  )
)
