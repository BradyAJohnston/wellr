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
#' @param data Data frame to reorder, than contains a row column and a col column.
#'
#' @return
#'
#' @examples
well_reorder_df <- function(data) {
  wells <- well_join(data$row, data$col)
  data$col <- well_to_colnum(wells)
  data$row <- well_to_rownum(wells)
  data <- data[order(data$col), ]
  data <- data[order(data$row), ]
  data
}

#' Convert a data.frame to a Matrix in Plate Format
#'
#' @param df A dataframe containing at least a column called "well" that
#'   contains well IDs.
#' @param plate Size of the plate, to override the auto-detected plate size.
#' @param values_from  Column that contains values to populate matrix.
#'
#' @return
#' @export
#'
#' @examples
#' library(wellr)
#'
#' # pivot a long data frame into a matrix based on the row and col columns,
#' # with values from a given column
#' plate <- well_plate(8, 12)
#' plate$value <- seq(nrow(plate))
#' well_df_to_matrix(plate, values_from = "value")
#'
#' # the function also handles missing values from a plate, filling with with NA
#' # if certain wells are missing
#' plate <- well_plate(7, 11)
#' plate$value <- seq(nrow(plate))
#' well_df_to_matrix(plate, values_from = "value", plate = 384)
well_df_to_matrix <- function(df, values_from, plate = NULL) {
  # if (!is.null(plate)) {}
  stopifnot(
    "well" %in% colnames(df)
  )
  df <- as.data.frame(df)
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
  if (!is.null(plate)) {
    plate <- plate
    ncols <- n_cols_from_wells(plate)
    nrows <- n_rows_from_wells(plate)
  }

  empty_plate <- well_plate(nrow = nrows, ncol = ncols)

  missing_wells <- sapply(empty_plate$well, function(x) !(x %in% df$well))

  missing_wells <- empty_plate[missing_wells, ]
  missing_wells[, values_from] <- NA

  df_only_relevant <- df[, c("well", "row", "col", values_from)]

  df_combined <- rbind(df_only_relevant, missing_wells)
  df_combined <- well_reorder_df(df_combined)

  matrix(as.numeric(df_combined[, values_from]), nrow = nrows, ncol = ncols, byrow = TRUE)
}

