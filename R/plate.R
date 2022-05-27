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
  plate <- expand.grid(col = ncol,
                       row = nrow)[, c("row", "col")]
  plate$well <- well_join(plate$row, plate$col)

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

well_reorder_df <- function(data,
                            well_col = "well",
                            row_col = NULL,
                            col_col = NULL) {
  if (well_col %in% colnames(data)) {
    wells <- data[, well_col]
  } else {
    wells <- well_join(row = data[, row_col],
                       col = data[, col_col])
  }

  data$col <- well_to_colnum(wells)
  data$row <- well_to_rownum(wells)
  data <- data[order(data$col),]
  data <- data[order(data$row),]
  data
}

#' Convert a data.frame to a Matrix in Plate Format
#'
#' Extracts row and column information from the column specified by `well_col`
#' (defaults to 'well') and uses this to create a matrix that contains values
#' from the `values_from` column.
#'
#' @param df A dataframe containing at least a column called "well" that
#'   contains well IDs.
#' @param plate Size of the plate, to override the auto-detected plate size.
#' @param values_from  Column that contains values to populate matrix.
#' @param well_col Column containing the well IDs to extract row and column
#'   information from.
#'
#' @return A matrix containing the values that were present in the data frame in
#'   the `values_from` column
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
well_df_to_mat <-
  function(df,
           values_from,
           well_col = "well",
           plate = NULL) {
    # if (!is.null(plate)) {}
    stopifnot(well_col %in% colnames(df))
    df <- as.data.frame(df)
    df$row <- well_to_rownum(df[, well_col])
    df$col <- well_to_rownum(df[, well_col])

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

    missing_wells <-
      sapply(empty_plate$well, function(x) {
        !(x %in% df[, well_col])
      })

    missing_wells <- empty_plate[missing_wells,]
    missing_wells[, values_from] <- NA

    df_only_relevant <- df[, c("well", "row", "col", values_from)]

    df_combined <- rbind(df_only_relevant, missing_wells)
    df_combined <- well_reorder_df(df_combined)

    matrix(
      as.numeric(df_combined[, values_from]),
      nrow = nrows,
      ncol = ncols,
      byrow = TRUE
    )
  }


#' Convert a DataFrame to a Multi-Frame Matrix
#'
#' Converts a DataFrame to a matrix where each row is a single time point, and
#' each column is a single well. The wells are in index order, indexing across
#' the rows so the first values are all from row A.
#'
#' @param data A DataFrame to be converted.
#' @param values_col The name of the column containing the values.
#' @param time_col The name of the column containing the time values.
#' @param well_col The name of the column with the well IDs.
#'
#' @return a matrix
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
#'   values_col = "value",
#'   time_col = "frame"
#' )
#'
#' head(mat[, 1:14])
well_df_to_mat_frames <- function(data,
                                  values_col,
                                  time_col,
                                  well_col = "well") {
  data <- as.data.frame(data)
  # order first by time
  data <- data[order(data[, time_col]),]
  # then order by the rows and columns
  data <- well_reorder_df(data, well_col = well_col)

  values <- unlist(data[, values_col])
  n_rows <- max(wellr::well_to_rownum(unlist(data[, well_col])))
  n_cols <- max(wellr::well_to_colnum(unlist(data[, well_col])))
  n_wells <- n_rows * n_cols
  frames <- unique(unlist(data[, time_col]))
  n_frames <- length(frames)

  mat <- matrix(values, ncol = n_wells, nrow = n_frames)
  rownames(mat) <- frames
  colnames(mat) <- wellr::well_from_index(seq(n_wells), plate = n_wells)
  mat
}

#' Turn a Multi-Frame Matrix into a DataFrame
#'
#' Takes a matrix where each row is a time point and each column is a well and
#' converts it to a DataFrame containing a column for the well, a column for the
#' row, a column for the column, a column for the time and a column for the
#' values.
#'
#' @param matrix multi-frame matrix to be converted.
#' @param value_col Name to be assigned to the values column.
#'
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
#'   values_col = "value",
#'   time_col = "frame"
#' )
#'
#' head(mat[, 1:14])
#'
#' # convert the matrix back to a dataframe
#' well_mat_frames_to_df(mat)
well_mat_frames_to_df <- function(matrix, value_col = "value") {
  df_list <- lapply(seq(nrow(matrix)), function(x) {
    row <- unlist(matrix[x,])
    plate_mat <- matrix(row, ncol = n_cols_from_wells(length(row)))
    plate_df <-
      wellr::well_mat_to_df(plate_mat, value_col = value_col)
    plate_df$frames <- x
    plate_df
  })

  combined <- do.call(rbind, df_list)
  rownames <- rownames(matrix)
  if (!is.null(rownames)) {
    combined$frames <- rownames[combined$frames]
  }

  tibble::as_tibble(combined)
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
    well = wellr::well_from_index(index),
    row = wellr::well_to_rownum(.data$well),
    col = wellr::well_to_colnum(.data$well),
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
#' well_dis(1:8, 1:8, ref_row = well_to_rownum("C4"), ref_col = well_to_colnum("C4"))
well_dis <- function(row, col, ref_row, ref_col) {
  sqrt((row - ref_row) ^ 2 + (col - ref_col) ^ 2)
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
