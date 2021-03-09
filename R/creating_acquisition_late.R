#' Creating acquisition late
#'
#' This function takes the epidemiologic link status, travel rated, outbreak
#' related, close contact related, and household contact related fields and
#' determines the acquisition for cases with an episode date on or after
#' 2020/04/01. Note, the other argument in the `case_when` function is
#' determined by the epidemiologic link status.
#'
#' @param epidemiologic_link_status A `character` indicating whether the case
#' was epidemiologically linked to an outbreak, another case, or travel.
#' @param outbreak_related A `character` indicating whether the case is related
#' to an outbreak.
#' @param close_contact_related A `character` indicating whether the case had
#' close contact with another case.
#' @param household_contact_related A `character` indicating whether the case
#' had household contact with another case.
#' @param travel_related A `character` indicating whether the case travelled
#' outside of the province.
#'
#' @return A `character` representing the acquisition for cases occuring on or
#' after 2020/04/01.
#' @export
#'
#' @examples
#' `creating_acqusition_late(epidemiologic_link_status, outbreak_related, close_contact_related, household_contact_related, travel_related)`
creating_acqusition_late <- function(epidemiologic_link_status,
                                     outbreak_related,
                                     close_contact_related,
                                     household_contact_related,
                                     travel_related) {
  create_acquisition_late <- case_when(
    outbreak_related == "Yes" ~ "Outbreak Related",
    (
      close_contact_related == "Yes" |
        household_contact_related == "Yes"
    ) ~ "Close Contact/Household Contact Related",
    travel_related == "Yes" ~ "Travel Related",
    TRUE ~ getting_other_acquisition(epidemiologic_link_status)
  )
  
  return(create_acquisition_late)
}
