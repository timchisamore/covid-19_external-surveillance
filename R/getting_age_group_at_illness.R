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
  get_age_group_at_illness <- santoku::chop(
    x = age_at_illness,
    breaks = c(seq(0, 85, 5), Inf),
    labels = c(
      "00-04",
      "05-09",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85+"
    )
  )

  return(get_age_group_at_illness)
}
