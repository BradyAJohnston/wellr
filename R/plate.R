#' Create a Tibble of Plate Information
#'
#' Generates a [tibble][tibble::tibble-package] that contains row, col, and well
#' ID for the size of the plate specified in nrow and ncol. If vectors of length
#' > 1 are supplied to either nrow or ncol, the contents of the vectors are used
#' instead of their numeric value.
#'
#' @param nrow Number of rows to have in the generated plate.
#' @param ncol NUmber of columns to have in the generated plate.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
well_plate <- function(nrow = 8, ncol = 12) {
  # nrow or ncol are of length 1, generate a sequence of numbers equal to their
  # values instead
  if (length(nrow) == 1) nrow <- seq(nrow)
  if (length(ncol) == 1) ncol <- seq(ncol)

  # generate the rows base of rows and columns
  plate <- expand.grid(
    col = ncol,
    row = nrow
  )[, c("row", "col")]
  plate$well <- well_join(plate$row, plate$col)

  # return the plate as a tibble
  tibble::as_tibble(plate)
}
