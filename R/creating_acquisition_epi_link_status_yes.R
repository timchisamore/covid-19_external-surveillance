#' Creating acquisition type for cases with an epidemiologic link status of "Yes"
#'
#' This function returns the acquisition status for cases with an epidemiologic
#' link status of "Yes". It tests whether the epidemiologic linkage is null or
#' equal to "None" and defines the hierarchy according to the episode date of
#' the case. Specifically, if the episode date is before 2020/04/01, then
#' the hierarrchy is travel related > outbreak related >
#' close contact/household related. If the episode date is on or after
#' 2020/04/01, then the hierarrchy is outbreak related >
#' close contact/household related > travel related.
#'
#' @param adjusted_epidemiologic_linkage A `character` indicating which of the
#' epidemiologic links the case had.
#' @param episode_date A `POSIXct` which is a proxy for the onset of COVID-19.
#' @param outbreak_related A `character` indicating whether the case is related
#' to an outbreak.
#' @param close_contact_related A `character` indicating whether the case had
#' close contact with another case.
#' @param household_contact_related A `character` indicating whether the case
#' had household contact with another case.
#' @param travel_related A `character` indicating whether the case travelled
#' outside of the province.
#'
#' @return A `character` indicating which acquisition type the case had.
#' @export
#'
#' @examples
#' `creating_acquisition_epi_link_status_yes(adjusted_epidemiologic_linkage, episode_date, outbreak_related, close_contact_related, household_contact_related, travel_related)`
creating_acquisition_epi_link_status_yes <-
  function(adjusted_epidemiologic_linkage,
           episode_date,
           outbreak_related,
           close_contact_related,
           household_contact_related,
           travel_related) {
    create_acquisition_epi_link_status_yes <- case_when(
      (
        !is.na(adjusted_epidemiologic_linkage) &
          adjusted_epidemiologic_linkage != "None"
      ) ~ adjusted_epidemiologic_linkage,
      ((
        is.na(adjusted_epidemiologic_linkage) |
          adjusted_epidemiologic_linkage == "None"
      ) &
        episode_date < lubridate::ymd("2020-04-01")
      ) ~ creating_acqusition_early(
        "Yes",
        travel_related,
        outbreak_related,
        close_contact_related,
        household_contact_related
      ),
      ((
        is.na(adjusted_epidemiologic_linkage) |
          adjusted_epidemiologic_linkage == "None"
      ) &
        episode_date >= lubridate::ymd("2020-04-01")
      ) ~ creating_acqusition_late(
        "Yes",
        outbreak_related,
        close_contact_related,
        household_contact_related,
        travel_related
      ),
      TRUE ~ getting_other_acquisition("Yes")
    )
    
    return(create_acquisition_epi_link_status_yes)
  }
