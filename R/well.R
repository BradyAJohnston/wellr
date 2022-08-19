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

  # rowlet <- ifelse(
  #   is.numeric(row),
  #   LETTERS[as.numeric(row)],
  #   stringr::str_trim(row)
  #   )

  # if row is character, coerce first to a numeric. Stop if unsuccessful
  rowlet <- sapply(row, function(x) {
    if (is.character(x)) {
      if (stringr::str_detect(x, "\\d+")) {
        y <- as.numeric(x)

        if (is.na(x)) {
          stop(paste("Cannot coerce supplied row:", x))
        }

        if (y > 26 | y < 1) {
          stop(paste("Row number", y, "cannot exceed the number of available letters (1:26)."))
        }
        # return corresponding capital letter
        LETTERS[y]
      } else {
        # print(x)
        x
      }
    } else {
      LETTERS[x]
    }
  })


  # pad out the column number for format "A01" properly.
  colnum <- stringr::str_pad(col, width = num_width, side = "left", pad = "0")

  # join the final well
  well <- as.character(paste0(rowlet, colnum))
  # quality control the well

  well
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

  stringr::str_extract(x, "\\d+$") %>%
    as.numeric()
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
  let <- well_to_row_let(x)
  row_num <- as.numeric(factor(let, levels = LETTERS))
  row_num
}

#' Converts Well ID to a Numeric Index
#'
#' @description  Indexes along rows first, so A12 is index 12 and B01 is index
#'   13 for a 96 well plate.
#'
#' @param x string well ID
#' @param plate size of the plate. One of c(6, 12, 24, 96, 384)
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
well_to_index <- function(x, plate = 96, colwise = FALSE) {
  stopifnot(is.character(x))


  colnum <- well_to_col_num(x)
  rownum <- well_to_row_num(x)

  if (colwise) {
    n_rows <- n_rows_from_wells(plate)
    id <- (colnum - 1) * n_rows + rownum
  } else {
    n_cols <- n_cols_from_wells(plate)
    id <- (rownum - 1) * n_cols + colnum
  }

  id
}

#' Convert Numeric Index to Well ID.
#'
#' @param x numeric index to convert to a well ID
#' @param plate number of wells in the plate. One of c(6, 12, 24, 96, 384)
#' @param num_width number of zeros to pad the column number with to the left.
#' @param colwise if TRUE, index instead down the columns, so H01 is index 8,
#'   A12 is index 89 and B01 is index 2 for a 96 well plate.
#'
#' @return a column ID as a vector of strings.
#' @export
#'
#' @examples
#' # indexing first along the rows
#' well_from_index(1:20)
#'
#' # indexing first down the columns
#' well_from_index(1:20, colwise = TRUE)
well_from_index <- function(x, plate = 96, num_width = 2, colwise = FALSE) {
  stopifnot(is.numeric(x))
    n_rows <- n_rows_from_wells(plate)
    n_cols <- n_cols_from_wells(plate)

  if (colwise) {
    id_row <- .count_row_down(x, n_rows)
    id_col <- .count_col_down(x, n_rows)

  } else {
    id_row <- .count_row_across(x, n_cols)
    id_col <- .count_col_across(x, n_cols)
  }

  well <- well_join(id_row, id_col)

  well
}

#' Format a Well to Uppercase and Padded Numbers
#'
#' @param x Vector of well IDs to be formatted.
#' @param num_width Width to pad out number component with 0's.
#'
#' @return Vector of strings as formatted well IDs.
#' @export
#'
#' @examples
#' well_format(c("A9", "c3", "h12"))
well_format <- function(x, num_width = 2) {
  wellr::well_join(
    row = wellr::well_to_row_num(x),
    col = wellr::well_to_col_num(x),
    num_width = num_width
  )
}

#' Calculate number of columns from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 96 or 384
#'
#' @return integer
n_cols_from_wells <- function(x) {
  stopifnot(is.numeric(x))
  switch(as.character(x),
    "6" = 3,
    "12" = 4,
    "24" = 6,
    "96" = 12,
    "384" = 24
  )
}

#' Calculate number of rows from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 384 or 96.
#'
#' @return integer
n_rows_from_wells <- function(x) {
  stopifnot(is.numeric(x))
  switch(as.character(x),
    "6" = 2,
    "12" = 3,
    "24" = 4,
    "96" = 8,
    "384" = 16
  )
}
