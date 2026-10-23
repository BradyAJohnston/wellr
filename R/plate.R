#' Create a Tibble of Plate Information
#'
#' Generates a [tibble][tibble::tibble-package] that contains row, col, and well
#' ID for the size of the plate specified in nrow and ncol. If vectors of length
#' `> 1` are supplied to either nrow or ncol, the contents of the vectors are used
#' instead of their numeric value.
#'
#' @param nrow Number of rows to have in the generated plate.
#' @param ncol NUmber of columns to have in the generated plate.
#'
#' @return a [tibble][tibble::tibble-package]. The `well` column holds
#'   [well()] IDs on a plate with this many rows and columns.
#' @export
#'
#' @examples
#' well_plate(nrow = 8, ncol = 12)
#' well_plate(nrow = 4, ncol = 6)
well_plate <- function(nrow = 8, ncol = 12) {
  # nrow or ncol are of length 1, generate a sequence of numbers equal to their
  # values instead
  if (length(nrow) == 1) {
    nrow <- seq(nrow)
  }
  if (length(ncol) == 1) {
    ncol <- seq(ncol)
  }

  # generate the rows base of rows and columns
  plate <- expand.grid(
    col = ncol,
    row = nrow
  )[, c("row", "col")]

  wells <- well_join(plate$row, plate$col)
  dims <- c(max(well_to_row_num(wells)), max(well_to_col_num(wells)))
  plate$well <- new_well(wells, dims)

  # return the plate as a tibble
  tibble::as_tibble(plate)
}

#' Reorder a Plate-Based DataFrame
#'
#' @param data Data frame to reorder, than contains a row column and a col column.
#' @param well_col Column containing the well IDs.
#' @param row_col Column containing the row letters or numbers.
#' @param col_col Column containing the column numbers.
#'
#' @return A data.frame that has been reordered according to the `row` & `col`
#'   columns.
#' @export
#'
#' @examples
#' df <- well_plate(nrow = 8, ncol = 12)
#' df <- df[sample(1:96, 96), ]
#' head(df)
#'
#' df <- well_reorder_df(df)
#' head(df)
#'
well_reorder_df <- function(
  data,
  well_col = "well",
  row_col = NULL,
  col_col = NULL
) {
  if (well_col %in% colnames(data)) {
    wells <- data[, well_col]
  } else {
    wells <- well_join(
      row = data[, row_col],
      col = data[, col_col]
    )
  }

  data$col <- well_to_col_num(wells)
  data$row <- well_to_row_num(wells)
  data <- data[order(data$col), ]
  data <- data[order(data$row), ]
  data
}

#' Convert a data.frame to a Matrix in Plate Format
#'
#' Extracts row and column information from the column specified by `well_col`
#' (defaults to 'well') and uses this to create a matrix that contains values
#' from the `values_from` column. Wells missing from `df` are `NA`.
#'
#' @param df A dataframe containing at least a column called "well" that
#'   contains well IDs, with one row per well.
#' @param plate Size of the plate, such as 96 or 384, or its number of rows and
#'   columns. If `NULL`, the plate of [well()] IDs is used, or else the smallest
#'   standard plate that holds all the wells.
#' @param values_from  Column that contains values to populate matrix.
#' @param well_col Column containing the well IDs to extract row and column
#'   information from.
#'
#' @return A matrix with a row for each row of the plate and a column for each
#'   column, containing the values that were present in the data frame in the
#'   `values_from` column.
#' @export
#'
#' @examples
#' library(wellr)
#'
#' # pivot a long data frame into a matrix based on the row and col columns,
#' # with values from a given column
#' plate <- well_plate(8, 12)
#' plate$value <- seq(nrow(plate))
#' well_df_to_mat(plate, values_from = "value")
#'
#' # the function also handles missing values from a plate, filling with with NA
#' # if certain wells are missing
#' plate <- well_plate(7, 11)
#' plate$value <- seq(nrow(plate))
#' well_df_to_mat(plate, values_from = "value", plate = 384)
well_df_to_mat <- function(df, values_from, well_col = "well", plate = NULL) {
  check_data_frame(df)
  check_column(df, well_col)
  check_column(df, values_from)
  df <- as.data.frame(df)
  wells <- df[[well_col]]
  dims <- plate_dims_for(wells, plate)

  rows <- well_to_row_num(wells)
  cols <- well_to_col_num(wells)
  duplicated <- duplicated(cbind(rows, cols))
  if (any(duplicated)) {
    cli::cli_abort(c(
      "{.arg df} must have one row per well.",
      i = "{.val {unique(as.character(wells[duplicated]))[1:min(3, sum(duplicated))]}} appear{?s/} more than once."
    ))
  }

  mat <- matrix(NA_real_, nrow = dims[[1]], ncol = dims[[2]])
  mat[cbind(rows, cols)] <- as.numeric(df[[values_from]])
  mat
}


#' Convert a DataFrame to a Multi-Frame Matrix
#'
#' Converts a DataFrame to a matrix where each row is a single time point, and
#' each column is a single well. The wells are in index order, indexing across
#' the rows so the first values are all from row A.
#'
#' @param data Data frame or tibble to convert to a multi-frame matrix.
#' @param value Column with value data that will be used to populate the matrix.
#' @param frame Column with the frame or time data that will be used to create the rows of the matrix.
#' @param well Column with the well IDs which will be used to create the columns of the matrix.
#'
#' @importFrom rlang .data
#' @return a matrix where each column is a well and each row is a time point.
#' @export
#'
#' @examples
# Create a data frame with multiple complete sets of plate data from multiple
# time points.
#' df_list <- lapply(1:10, function(x) {
#'   df <- wellr::well_plate()
#'   df$value <- rnorm(96)
#'   df$frame <- x
#'   df
#' })
#'
#' df_frames <- do.call(rbind, df_list)
#'
#' # convert the data frame to multi-frame matrix
#' mat <- well_df_to_mat_frames(
#'   data = df_frames,
#'   value = value,
#'   frame = frame,
#'   well = well
#' )
#'
#' head(mat[, 1:14])
#'
well_df_to_mat_frames <- function(data, value, frame, well) {
  df_frames <- dplyr::arrange(
    .data = data,
    {{ frame }},
    well_to_row_num({{ well }}),
    well_to_col_num({{ well }})
  )

  n_row <- max(well_to_row_num(dplyr::pull(df_frames, {{ well }})))
  n_col <- max(well_to_col_num(dplyr::pull(df_frames, {{ well }})))
  # n_row = 16
  # n_col = 50
  frames <- unique(dplyr::pull(data, {{ frame }}))
  n_wells <- n_row * n_col

  mat <- matrix(
    data = dplyr::pull(data, {{ value }}),
    ncol = n_wells,
    nrow = length(frames)
  )

  rownames(mat) <- frames
  colnames(mat) <- well_from_index(seq(n_wells), plate = c(n_row, n_col))

  mat
}

#' Turn a Multi-Frame Matrix into a DataFrame
#'
#' Takes a matrix where each row is a time point and each column is a well and
#' converts it to a DataFrame containing a column for the well, a column for the
#' row, a column for the column, a column for the time and a column for the
#' values.
#'
#' @param mat A matrix where each column is a well and each row is a frame
#'   from a multi time point experiment.
#' @param value Name for the column containing the values in the resulting data
#'   frame.
#'
#' @importFrom rlang .data
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
# Create a data frame with multple complete sets of plate data from multiple
# time points.
#' df_list <- lapply(1:10, function(x) {
#'   df <- wellr::well_plate()
#'   df$value <- rnorm(96)
#'   df$frame <- x
#'   df
#' })
#'
#' df_frames <- do.call(rbind, df_list)
#'
#' # convert the data frame to multi-frame matrix
#' mat <- well_df_to_mat_frames(
#'   data = df_frames,
#'   value = value,
#'   frame = frame,
#'   well = well
#' )
#'
#' head(mat[, 1:14])
#'
#' # convert the matrix back to a dataframe
#' well_mat_frames_to_df(mat)
#'
well_mat_frames_to_df <- function(mat, value = "value") {
  dat <- dplyr::as_tibble(mat)

  dat <- dplyr::mutate(dat, frame = rownames(mat))

  dat <- tidyr::pivot_longer(
    data = dat,
    cols = -"frame",
    names_to = "well",
    values_to = value
  )

  dat <- dplyr::mutate(
    .data = dat,
    well = as_well(.data$well),
    row = well_to_row_num(.data$well),
    col = well_to_col_num(.data$well)
  )

  dat
}

#' Convert a Plate Matrix to a DataFrame
#'
#' Takes a matrix that is formatted as a plate, and converts it to a long-format
#' 'tidy' data frame with a column for the well ID, the row, the column and the
#' values that were in the matrix.
#'
#' @param matrix A matrix object to be converted.
#' @param value_col The name of the value column in the final DataFrame.
#'
#' @return a [tibble][tibble::tibble-package]
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(96), ncol = 12)
#' well_mat_to_df(mat, "random_values")
well_mat_to_df <- function(matrix, value_col = "value") {
  values <- c(t(matrix))
  index <- seq_along(values)

  df <- tibble::tibble(
    well = well_from_index(index, plate = dim(matrix)),
    row = well_to_row_num(.data$well),
    col = well_to_col_num(.data$well),
    value = values
  )

  colnames(df)[4] <- value_col
  df
}


#' Relative Distance Between Given Well and a Reference Row
#'
#' @param row Numeric vector of row numbers. Must be equal in length to col.
#' @param col Numeric vector of col numbers. Must be equal in length to row.
#' @param ref_row Single reference row number.
#' @param ref_col Single reference column number.
#'
#' @return a numeric vector.
#' @export
#'
#' @examples
#' well_dis(3, 4, ref_row = 5, ref_col = 5)
#' well_dis(1:8, 1:8, ref_row = well_to_row_num("C4"), ref_col = well_to_col_num("C4"))
well_dis <- function(row, col, ref_row, ref_col) {
  sqrt((row - ref_row)^2 + (col - ref_col)^2)
}

#' Relative Distance for an Entire Plate
#'
#' @param ref_row Row number to calculate distance from.
#' @param ref_col Column number to calculate distance from.
#' @param plate Plate size to calculate distances for.
#'
#' @return a matrix of relative distances.
#' @export
#'
#' @examples
#'
#' well_dis_plate(5, 5)
#'
#' image(well_dis_plate(5, 5))
well_dis_plate <- function(ref_row, ref_col, plate = 96) {
  n_cols <- n_cols_from_wells(plate)
  n_rows <- n_rows_from_wells(plate)

  outer(
    X = seq(n_rows),
    Y = seq(n_cols),
    FUN = well_dis,
    ref_row = ref_row,
    ref_col = ref_col
  )
}
