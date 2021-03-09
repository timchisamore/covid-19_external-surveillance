#' Getting Palette
#'
#' This function returns a palette specific for the organizaton developed by staff.
#'
#' @return A `character` vector of five RGB values.
#' @export
#'
#' @examples
#' `getting_palette()`
getting_palette <- function() {
  get_palette <- c(
    rgb(
      red = 149,
      green = 55,
      blue = 53,
      maxColorValue = 255
    ),
    rgb(
      red = 228,
      green = 108,
      blue = 10,
      maxColorValue = 255
    ),
    rgb(
      red = 0,
      green = 112,
      blue = 192,
      maxColorValue = 255
    ),
    rgb(
      red = 0,
      green = 176,
      blue = 80,
      maxColorValue = 255
    ),
    rgb(
      red = 255,
      green = 192,
      blue = 0,
      maxColorValue = 255
    )
  )

  return(get_palette)
}
