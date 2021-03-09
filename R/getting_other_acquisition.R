#' Getting other acquisition
#' 
#' This function takes the epidemiologic link status and determines what the
#' other acquisition type should be for our early and late acquisition types.
#' For example, if the epidemiologic link status is "Yes" then the other
#' acqusition will be "Epidemiological Linkage – Type Unspecified".
#'
#' @param epidemiologic_link_status A `character` indicating whether the case
#' has an epidemiologic link.
#'
#' @return A `character` determining what the other acquisition will be be for
#' our early and later acquisition functions.
#' @export
#'
#' @examples
#' `getting_other_acquisition(epidemiologic_link_status)`
getting_other_acquisition <- function(epidemiologic_link_status) {
  get_other_acquisition <-
    case_when(
      epidemiologic_link_status == "Yes" ~ "Epidemiological Linkage – Type Unspecified",
      epidemiologic_link_status == "No" ~ "No Known Epi-Link",
      (
        is.na(epidemiologic_link_status) |
          epidemiologic_link_status == "None"
      ) ~ "No Information",
      TRUE ~ "Unknown"
    )
  
  return(get_other_acquisition)
}
