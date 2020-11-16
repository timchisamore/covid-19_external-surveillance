plan <- drake_plan(
  raw_ccm_investigations_data = reading_ccm_investigations_data(file_in(!!here::here("data", "raw", "ccm_investigations_data.csv"))),
  raw_ccm_outcomes_data = reading_ccm_outcomes_data(file_in(!!here::here("data", "raw", "ccm_outcomes_data.csv"))),
  raw_ccm_interventions_data = reading_ccm_interventions_data(file_in(!!here::here("data", "raw", "ccm_interventions_data.csv"))),
  raw_ccm_risk_factors_data = reading_ccm_risk_factors_data(file_in(!!here::here("data", "raw", "ccm_risk_factors_data.csv"))),
  raw_ccm_outbreaks_data = reading_ccm_outbreaks_data(file_in(!!here::here("data", "raw", "ccm_outbreaks_data.csv"))),
  clean_ccm_investigations_data = cleaning_ccm_investigations_data(raw_ccm_investigations_data),
  clean_ccm_outcomes_data = cleaning_ccm_outcomes_data(raw_ccm_outcomes_data),
  clean_ccm_interventions_data = cleaning_ccm_interventions_data(raw_ccm_interventions_data),
  clean_ccm_risk_factors_data = cleaning_ccm_risk_factors_data(raw_ccm_risk_factors_data),
  clean_ccm_outbreaks_data = cleaning_ccm_outbreaks_data(raw_ccm_outbreaks_data),
  raw_aces_data = reading_aces_data(file_in(!!here::here("data", "raw", "aces_data.xlsx"))),
  clean_aces_data = cleaning_aces_data(raw_aces_data),
  get_aces_sheets = target(
    get_sheet(clean_aces_data, sheet = sheets),
    transform = map(
      sheets = c("ed_visits", "admissions"),
      .names = c("ed_visits", "admissions")
    )
  ),
  raw_assessment_centre_data = reading_assessment_centre_data(file_in(!!here::here("data", "raw", "assessment_centre_data.xlsx"))),
  clean_assessment_centre_data = cleaning_assessment_centre_data(raw_assessment_centre_data),
  figure_1 = covid_19_cases_by_day(clean_ccm_investigations_data, clean_ccm_risk_factors_data),
  table_1 = covid_19_outcomes(clean_ccm_investigations_data, clean_ccm_outcomes_data),
  table_2 = covid_19_outcomes_by_type(clean_ccm_investigations_data, clean_ccm_risk_factors_data, clean_ccm_outcomes_data),
  table_3 = covid_19_interventions(clean_ccm_investigations_data, clean_ccm_interventions_data, clean_ccm_outcomes_data),
  table_4 = covid_19_demographics(clean_ccm_investigations_data),
  figure_2 = assessments_by_day(clean_assessment_centre_data),
  figure_3 = ed_visits_by_day(ed_visits),
  figure_4 = ed_visits_by_week(ed_visits),
  figure_5 = admissions_by_week(admissions),
  report = rmarkdown::render(
    input = knitr_in(!!here::here("documents", "covid_19_external_surveillance.Rmd")),
    output_file = file_out(!!here::here("documents", "covid_19_external_surveillance.html"))
  )
)
