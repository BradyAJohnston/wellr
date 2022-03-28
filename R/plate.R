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

#' Title
#'
#' @param df A dattaframe containing at least a column called "well" that
#'   contains well IDs.
#' @param plate Size of the plate, to override the auto-detected plate size.
#'
#' @return
#' @export
#'
#' @examples
well_df_to_matrix <- function(df, data, plate = NULL) {
  # if (!is.null(plate)) {}
  stopifnot(
    "well" %in% colnames(df)
  )

  df$row <- well_to_rownum(df$well)
  df$col <- well_to_rownum(df$well)

  max_cols <- max(df$col)
  max_rows <- max(df$row)

  if (max_cols <= 3) {
    plate <- 6
    ncols <- 3
    nrows <- 2
  } else if (max_cols <= 6) {
    plate <- 24
    ncols <- 6
    nrows <- 4
  } else if (max_cols <= 12) {
    plate <- 96
    ncols <- 12
    nrows <- 8
  } else if (max_cols <= 24) {
    plate <- 384
    ncols <- 24
    nrows <- 16
  } else if (max_cols <= 48) {
    plate <- 1536
    ncols <- 48
    nrows <- 32
  } else {
    stop(
      paste0(
        "Detected Number of cols '",
        max_cols,
        "', no within acceptable plate parameters."
      )
    )
  }

  empty_plate <- well_plate(nrow = ncows, ncol = ncols)

  missing_wells <- empty_plate[!(df$well %in% empty_plate$well)]



  matrix(NA, nrow = nrows, ncol = ncols)
}
