#' Assert Investigation Numbers
#'
#' This function takes our investigation numbers from the CCM Investigations data
#' and checks whether any of our generated investigation numbers for long-term care home,
#' reitement home, etc. are not contained within. This would indicate issues
#' with our CCM queries and will stop the report.
#'
#' @param investigation_numbers The investigation numbers from our clean CCM investigations data
#' @param generated_investigation_numbers Various sets of generated investigation numbers from clean CCM data
#'
#' @return None
#' @export
#'
#' @examples
#' assert_investigation_numbers(clean_ccm_investigations_data$investigation_number, generate_active_ids(clean_ccm_outcomes_data))
assert_investigation_numbers <-
  function(investigation_numbers, generated_investigation_numbers) {
    return(stopifnot("One of our generated investigation numbers is invalid!" = is_empty(
      setdiff(generated_investigation_numbers, investigation_numbers)
    )))
  }
