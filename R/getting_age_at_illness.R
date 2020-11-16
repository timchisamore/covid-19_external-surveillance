#' Getting Age at Illness
#' 
#' This function takes the cases date of birth and episode date and calculates their age at illness.
#'
#' @param person_client_date_of_birth a Date vector of case date of births
#' @param episode_date a Date vector of case episode dates
#'
#' @return a numeric vector of case ages at illness
#' @export
#'
#' @examples
#' getting_age_at_illness(person_client_date_of_birth, episode_date)
getting_age_at_illness <- function(person_client_date_of_birth, episode_date) {
  get_age_at_illness <- floor(lubridate::time_length(lubridate::interval(person_client_date_of_birth, episode_date), "years"))
  
  return(get_age_at_illness)
}
