#' Creating acquisition early
#'
#' This function takes the epidemiologic link status, travel rated, outbreak
#' related, close contact related, and household contact related fields and
#' determines the acquisition for cases with an episode date before 2020/04/01.
#' Note, the other argument in the `case_when` function is determined by the
#' epidemiologic link status.
#'
#' @param epidemiologic_link_status A `character` indicating whether the case
#' was epidemiologically linked to an outbreak, another case, or travel.
#' @param travel_related A `character` indicating whether the case travelled
#' outside of the province.
#' @param outbreak_related A `character` indicating whether the case is related
#' to an outbreak.
#' @param close_contact_related A `character` indicating whether the case had
#' close contact with another case.
#' @param household_contact_related A `character` indicating whether the case
#' had household contact with another case.
#'
#' @return A `character` representing the acquisition for cases occuring before
#' 2020/04/01.
#' @export
#'
#' @examples
#' `creating_acqusition_early(epidemiologic_link_status, travel_related, outbreak_related, close_contact_related, household_contact_related)`
creating_acqusition_early <- function(epidemiologic_link_status,
                                     travel_related,
                                     outbreak_related,
                                     close_contact_related,
                                     household_contact_related) {
  create_acquisition_early <- case_when(
    travel_related == "Yes" ~ "Travel Related",
    outbreak_related == "Yes" ~ "Outbreak Related",
    (
      close_contact_related == "Yes" |
        household_contact_related == "Yes"
    ) ~ "Close Contact/Household Contact Related",
    TRUE ~ getting_other_acquisition(epidemiologic_link_status)
  )
  
  return(create_acquisition_early)
}
