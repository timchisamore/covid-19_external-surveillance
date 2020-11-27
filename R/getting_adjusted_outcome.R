#' Getting Adjusted Outcome
#'
#' This function takes the outcome and status of a case and produces
#' an adjusted outcome of active, resolved, or fatal.
#'
#' @param outcome a character representing the outcome of the case
#' @param status a character representing the status of the case
#'
#' @return a character representing the adjusted outcome of the case
#' @export
#'
#' @examples
#' getting_adjusted_outcome(outcome, status)
getting_adjusted_outcome <- function(outcome, status) {
  # we are considering any cases with no outcome or with a ending or unknown outcome and a status that
  # isn't closed as active. Cases with an outcome of recovered, res. effects or ill or with no outcome,
  # and outcome of pending or unknown and status of closed are considered resolved. Fetal cases are
  # considered fatal.
  get_adjusted_outcome <- case_when(
    ((
      is.na(outcome) |
        outcome %in% c("PENDING", "UNKNOWN")
    ) & status != "Closed") ~ "Active",
    (
      outcome %in% c("RECOVERED", "RES. EFFECTS", "ILL") |
        ((
          is.na(outcome) | outcome %in% c("PENDING", "UNKNOWN")
        ) &
          status == "Closed")
    ) ~ "Resolved",
    TRUE ~ outcome
  )
  get_adjusted_outcome <- as_factor(str_to_title(get_adjusted_outcome))

  return(get_adjusted_outcome)
}
