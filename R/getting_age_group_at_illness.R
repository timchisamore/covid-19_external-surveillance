#' Getting Age Group at Illness
#' 
#' This function takes the age at illness of a case and returns the age group at illness it belongs to.
#'
#' @param age_at_illness a numeric vector of ages at illness for cases
#'
#' @return a character vector of age groups at illness for cases
#' @export
#'
#' @examples
#' getting_age_group_at_illness(age_at_illness)
getting_age_group_at_illness <- function(age_at_illness) {
  get_age_group_at_illness <- santoku::chop(age_at_illness, breaks = c(20, 45, 65, Inf), labels = c("<20", "20-44", "45-64", "65+"))
  
  return(get_age_group_at_illness)
}
