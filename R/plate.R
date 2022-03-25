#' Title
#'
#' @param nrow
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
well_plate <- function(nrow = 8, ncol = 12) {
  if (length(nrow) == 1) nrow <- seq(nrow)
  if (length(ncol) == 1) ncol <- seq(ncol)

  plate <- expand.grid(
    col = ncol,
    row = nrow
  )[, c("row", "col")]
  plate$well <- well_join(plate$row, plate$col)
  plate
}
