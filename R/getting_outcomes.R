#' Getting Outcomes
#' 
#' This function takes the outcome and status values for CCM COVID-19
#' investigations and returns a value of active, fatal, or resolved.
#'
#' @param outcome a character vector of outcome values from clean CCM outcomes data
#' @param status a character vector of status values from clean CCM investigations data
#'
#' @return a character vector of outcome values
#' @export
#'
#' @examples
#' getting_outcomes(clean_ccm_outcomes_data$outcome, clean_ccm_investigations_data$status)
getting_outcomes <- function(outcome, status) {
  # missing outcomes or outcomes of pending or unknown and a status of not closed will
  # be considered active. Outcomes of recovered, res. effects or missing outcomes or
  # outcomes of pending or unknown with a status of closed will be considered
  # resolved. Fatal outcomes will stay as is.
  get_outcomes <- case_when(
    ((
      is.na(outcome) |
        outcome %in% c("PENDING", "UNKNOWN")
    ) & status != "Closed") ~ "active",
    (
      outcome %in% c("RECOVERED", "RES. EFFECTS") |
        ((is.na(outcome) | outcome %in% c("PENDING", "UNKNOWN")) &
           status == "Closed")
    ) ~ "resolved",
    TRUE ~ outcome
  )
  
  get_outcomes <- str_to_lower(get_outcomes)
  
  return(get_outcomes)
}
