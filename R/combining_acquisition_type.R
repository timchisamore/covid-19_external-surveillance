#' Combining acquisition types for all epidemiologic link statuses
#'
#' This function takes the epidemiologic link status of a case and determines
#' what it's acquisition type is based on adjusted epidemiologic linkage, episode
#' date, etc.
#'
#' @param epidemiologic_link_status A `character` indicating whether the case
#' was epidemiologically linked to an outbreak, another case, or travel.
#' @param adjusted_epidemiologic_linkage A `character` indicating which of the
#' epidemiologic links the case had.
#' @param episode_date A `POSIXct` which is a proxy for the onset of COVID-19
#' @param outbreak_related A `character` indicating whether the case is related
#' to an outbreak.
#' @param close_contact_related A `character` indicating whether the case had
#' close contact with another case.
#' @param household_contact_related A `character` indicating whether the case
#' had household contact with another case.
#' @param travel_related A `character` indicating whether the case travelled
#' outside of the province.
#'
#' @return A `character` indicating the acquisition type of the case.
#' @export
#'
#' @examples
#' `combining_acquisition_type(epidemiologic_link_status, adjusted_epidemiologic_linkage, episode_date, outbreak_related, close_contact_related, household_contact_related, travel_related)`
combining_acquisition_type <-
  function(epidemiologic_link_status,
           adjusted_epidemiologic_linkage,
           episode_date,
           outbreak_related,
           close_contact_related,
           household_contact_related,
           travel_related) {
    combined_acquisition_type <-
      case_when(
        epidemiologic_link_status == "Yes" ~ creating_acquisition_epi_link_status_yes(
          adjusted_epidemiologic_linkage,
          episode_date,
          outbreak_related,
          close_contact_related,
          household_contact_related,
          travel_related
        ),
        epidemiologic_link_status == "No" ~ creating_acquisition_epi_link_status_no(
          episode_date,
          outbreak_related,
          close_contact_related,
          household_contact_related,
          travel_related
        ),
        (is.na(epidemiologic_link_status) |
          epidemiologic_link_status == "Missing Information") ~ creating_acquisition_epi_link_status_missing(
          episode_date,
          outbreak_related,
          close_contact_related,
          household_contact_related,
          travel_related
        )
      )

    return(combined_acquisition_type)
  }
