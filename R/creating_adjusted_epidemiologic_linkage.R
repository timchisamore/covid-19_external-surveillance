#' Creating adjusted epidemiologic linkage
#'
#' This function creates an adjusted epidemiologic linkage for cases where their
#' epidemiologic link status equals "Yes". Specifically, close contact and
#' household contact are collapsed into a single value and outbreak related and
#' travel are renamed to provide a consistent naming.
#'
#' @param epidemiologic_link_status A `character` indicating whether the case
#' was linked epidemiologically to an outbreak, another case, or travel.
#' @param epidemiologic_linkage A `character` indicating which of the
#' epidemiologic links the case had.
#'
#' @return A `character` that has been adjused to either combine values or
#' provide consistent naming.
#' @export
#'
#' @examples
#' `creating_adjusted_epidemiologic_linkage(epidemiologic_link_status, epidemiologic_linkage)`
creating_adjusted_epidemiologic_linkage <-
  function(epidemiologic_link_status,
           epidemiologic_linkage) {
    create_adjusted_epidemiologic_linkage <-
      if_else(
        epidemiologic_link_status == "Yes",
        case_when(
          epidemiologic_linkage == "Outbreak related" ~ "Outbreak Related",
          epidemiologic_linkage == "Close contact" |
            epidemiologic_linkage == "Household contact" ~ "Close Contact/Household Contact Related",
          epidemiologic_linkage == "Travel" ~ "Travel Related"
        ),
        epidemiologic_linkage
      )

    return(create_adjusted_epidemiologic_linkage)
  }
