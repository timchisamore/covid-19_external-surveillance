#' Assert Generated IDs
#'
#' This function takes our investigation numbers from the CCM Investigations data
#' and checks whether any of our generated IDs for active cases, long-term care
#' associated cases, etc. are not contained within. This would indicate issues
#' with our CCM queries and will stop the report.
#'
#' @param investigation_numbers The investigation numbers from our CCM Investigations data
#' @param generated_ids Various sets of generated IDs from CCM objects
#'
#' @return None
#' @export
#'
#' @examples
#' assert_generated_ids(clean_ccm_investigations_data$investigation_number, generate_active_ids(clean_ccm_outcomes_data))
assert_generated_ids <-
  function(investigation_numbers, generated_ids) {
    return(stopifnot("One of our generated IDs is invalid!" = is_empty(
      setdiff(generated_ids, investigation_numbers)
    )))
  }
