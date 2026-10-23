#' Checks if a well ID contains a letter and a number in the correct format.
#'
#' @param x well ID to check for proper structure.
#'
#' @return A vector the same length as x with a TRUE of FALSE for a a well that
#'   can be read by wellr
well_check <- function(x) {
  row_letter <- stringr::str_detect(
    as.character(x),
    "^[:alpha:](?=\\d)"
  )

  col_numer <- stringr::str_detect(
    as.character(x),
    "\\d{1,3}$"
  )

  row_letter & col_numer
}

#' Logical test for well ID format.
#'
#' @param x A string vector.
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#' is_well_id(c("a12", "a2", "a02", "foo1"))
is_well_id <- function(x) {
  well_id_regex <- "^(A|a)?[:alpha:]\\d{1,3}"

  stringr::str_detect(
    stringr::str_trim(x),
    well_id_regex
  )
}

# given a column number and a row (either number or letter) return the
# 3-character well that is joined from the two

#' Join a row and column into a well ID.
#'
#' Joins a column number and a row letter or number into a well ID. i.e. joins
#' "A" and "1" to "A01" and joins "3 and "10" to "C10".
#'
#' @param row either a row letter or number for to be coerced into a letter.
#' @param col a column number.
#' @param num_width the number of digits to pad out the column number with 0.
#'
#' @return a vector of well IDs as strings.
#' @export
#'
#' @examples
#'
#' well_join(1:3, 4)
#' well_join(c("A", "B", "H"), 9)
#' well_join("C", 1:10)
well_join <- function(row, col, num_width = 2) {
  check_number_whole(num_width, min = 1)
  if (is.factor(row)) {
    row <- as.integer(row)
  }

  rowlet <- if (is.character(row)) {
    # Rows given as numbers in strings, such as "3", are converted to letters.
    numeric_row <- !is.na(row) & stringr::str_detect(row, "\\d")
    out <- row
    out[numeric_row] <- row_letters(row[numeric_row], arg = "row")
    out
  } else if (is.numeric(row)) {
    row_letters(row, arg = "row")
  } else {
    stop_input_type(row, c("a character vector", "a numeric vector"))
  }

  # pad out the column number for format "A01" properly.
  colnum <- stringr::str_pad(col, width = num_width, side = "left", pad = "0")

  joined <- vctrs::vec_recycle_common(row = rowlet, col = colnum)
  well <- paste0(joined$row, joined$col)
  well[is.na(joined$row) | is.na(joined$col)] <- NA
  well
}

# Converts row numbers to letters, erroring informatively for anything that
# isn't a whole number from 1 to 26.
row_letters <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  number <- suppressWarnings(as.numeric(x))
  bad <- (!is.na(x) & is.na(number)) |
    (!is.na(number) & (number < 1 | number > 26 | number != round(number)))
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be row letters or whole numbers from 1 to 26.",
        x = "Can't use {.val {x[bad][1:min(3, sum(bad))]}}."
      ),
      call = call
    )
  }
  LETTERS[number]
}

#' Extract number from well ID.
#'
#' @param x Well ID as string in format "A10"
#'
#' @return Numeric verctor of column numbers extracted from well ID.
#' @export
#'
#' @examples
#' well_to_col_num(c("A10", "c3", "h12"))
#' well_to_col_num("H08")
#' well_to_col_num("h8")
well_to_col_num <- function(x) {
  x <- stringr::str_trim(x)

  x <- stringr::str_extract(x, "\\d+$")

  as.numeric(x)
}

#' Extracts letter from well ID.
#'
#' @param x Well ID as string.
#'
#' @return a string which is the letter extracted from a well ID.
#' @export
#'
#' @examples
#' well_to_row_let(c("A10", "c3", "h12"))
#' well_to_row_let("H08")
#' well_to_row_let("h8")
#' well_to_row_let("C20")
well_to_row_let <- function(x) {
  x <- stringr::str_trim(x)

  x <- stringr::str_to_upper(x)
  let <- stringr::str_extract(x, "^[^\\d]+")
  let
}

#' Convert Well ID to Row Number
#'
#' Calculates the row, but returns it as a number.
#'
#' @param x Well ID as a string.
#'
#' @return Numeric row number.
#' @export
#'
#' @examples
#' well_to_row_num(c("A10", "c3", "h12"))
#' well_to_row_num("H08")
#' well_to_row_num("h8")
#' well_to_row_num("C20")
well_to_row_num <- function(x) {
  x <- stringr::str_to_upper(x)
  row_let <- well_to_row_let(x)
  row_num <- factor(row_let, levels = LETTERS)
  as.numeric(row_num)
}

#' Converts Well ID to a Numeric Index
#'
#' @description  Indexes along rows first, so A12 is index 12 and B01 is index
#'   13 for a 96 well plate.
#'
#' @param x string well ID
#' @param plate size of the plate. One of c(6, 12, 24, 48, 96, 384), or the
#'   number of rows and columns. If `NULL`, the plate of [well()] IDs is used,
#'   or else a 96 well plate.
#' @param colwise if TRUE, index instead down the columns, so H01 is index 8,
#'   A12 is index 89 and B01 is index 2 for a 96 well plate.
#'
#' @return numeric well index.
#' @export
#'
#' @examples
#'
#' # indexing along the row first
#' well_to_index(c("A10", "c3", "h12"))
#' well_to_index("H08")
#' well_to_index("h8")
#' well_to_index("C20")
#'
#' # indexing instead down the column first
#' well_to_index(c("A10", "c3", "h12"), colwise = TRUE)
#' well_to_index("H08", colwise = TRUE)
#' well_to_index("h8", colwise = TRUE)
#' well_to_index("C20", colwise = TRUE)
well_to_index <- function(x, plate = NULL, colwise = FALSE) {
  check_character(x)
  check_bool(colwise)
  dims <- resolve_plate_dims(x, plate)

  colnum <- well_to_col_num(x)
  rownum <- well_to_row_num(x)

  if (colwise) {
    id <- (colnum - 1) * dims[[1]] + rownum
  } else {
    id <- (rownum - 1) * dims[[2]] + colnum
  }

  id
}

#' Convert Numeric Index to Well ID.
#'
#' @param x numeric index to convert to a well ID
#' @param plate number of wells in the plate. One of c(6, 12, 24, 48, 96, 384),
#'   or the number of rows and columns. If `NULL`, a 96 well plate.
#' @param num_width number of zeros to pad the column number with to the left.
#' @param colwise if TRUE, index instead down the columns, so H01 is index 8,
#'   A12 is index 89 and B01 is index 2 for a 96 well plate.
#'
#' @return [well()] IDs on the plate.
#' @export
#'
#' @examples
#' # indexing first along the rows
#' well_from_index(1:20)
#'
#' # indexing first down the columns
#' well_from_index(1:20, colwise = TRUE)
well_from_index <- function(x, plate = NULL, num_width = 2, colwise = FALSE) {
  if (!is.numeric(x)) {
    stop_input_type(x, "a numeric vector")
  }
  check_bool(colwise)
  dims <- resolve_plate_dims(x, plate)
  n_rows <- dims[[1]]
  n_cols <- dims[[2]]

  outside <- !is.na(x) & (x < 1 | x > n_rows * n_cols | x != round(x))
  if (any(outside)) {
    cli::cli_abort(
      c(
        "{.arg x} must be whole numbers from 1 to {n_rows * n_cols}, the wells of the plate.",
        x = "Can't use {.val {x[outside][1:min(3, sum(outside))]}}."
      )
    )
  }

  if (colwise) {
    id_row <- .count_row_down(x, n_rows)
    id_col <- .count_col_down(x, n_rows)
  } else {
    id_row <- .count_row_across(x, n_cols)
    id_col <- .count_col_across(x, n_cols)
  }

  well <- well_join(id_row, id_col)

  new_well(well, dims)
}

#' Format a Well to Uppercase and Padded Numbers
#'
#' @param x Vector of well IDs to be formatted.
#' @param num_width Width to pad out number component with 0's.
#' @param uppercase Logical, whether the letter should be upercase.
#'
#' @return Vector of strings as formatted well IDs. [well()] IDs keep their
#'   plate.
#' @export
#'
#' @examples
#' well_format(c("A9", "c3", "h12"))
well_format <- function(x, num_width = 2, uppercase = TRUE) {
  dims <- if (is_well(x)) well_plate_dims(x)

  x <- wellr::well_join(
    row = wellr::well_to_row_num(x),
    col = wellr::well_to_col_num(x),
    num_width = num_width
  )

  if (!uppercase) {
    x <- stringr::str_to_lower(x)
  }
  if (!is.null(dims)) {
    x <- new_well(x, dims)
  }
  x
}

#' Calculate number of columns from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 96 or 384
#'
#' @return integer
#' @keywords internal
n_cols_from_wells <- function(x) {
  plate_dims(x)[[2]]
}

#' Calculate number of rows from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 384 or 96.
#'
#' @return integer
#' @keywords internal
n_rows_from_wells <- function(x) {
  plate_dims(x)[[1]]
}

#' Rows and columns of a plate
#'
#' `well_row()` gives the row letter of each well and `well_col()` its column
#' number, as factors whose levels are all the rows or all the columns of the
#' plate, in order. Unlike [well_to_row_let()] and [well_to_col_num()], they
#' keep rows and columns with no wells in the data, so sorting, counting,
#' faceting and plotting show the whole plate.
#'
#' @param x Well IDs, such as `"A01"`.
#' @param plate The size of the plate, such as 96 or 384, or its number of rows
#'   and columns. If `NULL`, the plate of [well()] IDs is used, or else the
#'   smallest standard plate that holds all the wells.
#'
#' @returns A factor the same length as `x`.
#' @export
#'
#' @examples
#' x <- well(c("A01", "C05"), plate = 96)
#' well_row(x)
#' well_col(x)
#'
#' # Every row of the plate is counted, even those with no wells
#' table(well_row(x))
#'
#' well_row(c("A01", "C05"), plate = 384)
well_row <- function(x, plate = NULL) {
  check_character(x)
  dims <- plate_dims_for(x, plate)
  factor(well_to_row_let(x), levels = LETTERS[seq_len(dims[[1]])])
}

#' @rdname well_row
#' @export
well_col <- function(x, plate = NULL) {
  check_character(x)
  dims <- plate_dims_for(x, plate)
  factor(well_to_col_num(x), levels = seq_len(dims[[2]]))
}
