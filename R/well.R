#' Checks if a well ID contains a letter and a number in the correct format.
#'
#' @param x well ID to check for proper structure.
#'
#' @return
well_check <- function(x) {
  lapply(x, function(x) {
     if (is.na(stringr::str_extract(as.character(x), "^[:alpha:](?=\\d)"))) {
      stop(paste0("Well ID '", x, "' has a misformed row letter."))
     }
     if (is.na(stringr::str_extract(as.character(x), "\\d{1,3}$"))) {
       stop(paste0("Well ID '", x, "' has a misformed col number."))
     }
  })
  return(TRUE)

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

  # if row is character, coerce first to a numeric. Stop if unsuccessful
  rowlet <- sapply(row, function(x) {
    if(is.character(x)) {
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
        print(x)
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
  well_check(well)

  well
}

#' Extract number from well ID.
#'
#' @param x Well ID as string in format "A10"
#'
#' @return
#' @export
#'
#' @examples
#' well_to_colnum("A10")
#' well_to_colnum("H08")
#' well_to_colnum("h8")
well_to_colnum <- function(x) {
  x <- stringr::str_trim(x)
  well_check(x)
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
#' well_to_colnum("A10")
#' well_to_colnum("H08")
#' well_to_colnum("h8")
#' well_to_colnum("C20")
well_to_rowlet <- function(x) {
  x <- stringr::str_trim(x)
  well_check(x)
  stringr::str_extract(x, "^\\w")
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
#' well_to_colnum("A10")
#' well_to_colnum("H08")
#' well_to_colnum("h8")
#' well_to_colnum("C20")
well_to_rownum <- function(x) {
  x <- stringr::str_to_upper(x)
  let <- well_to_rowlet(x)
  rownum <- as.numeric(factor(let, levels = LETTERS))
  rownum
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
well_to_index <- function(x, plate = 96, colwise = FALSE) {
  stopifnot(is.character(x))
  well_check(x)
  colnum <- well_to_colnum(x)
  rownum <- well_to_rownum(x)

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
well_from_index <- function(x, plate = 96, num_width = 2, colwise = FALSE) {
  stopifnot(is.numeric(x))

  if (colwise) {
    plate <- expand.grid(
      row = seq(n_rows_from_wells(plate)),
      col = seq(n_cols_from_wells(plate))
      )
    plate$well <- well_join(plate$row, plate$col)
  } else {
    plate <- wellr::well_plate(
      n_rows_from_wells(plate),
      n_cols_from_wells(plate)
    )
  }



  well <- plate$well[x]
  well_check(well)
  well
}

#' Calculate number of columns from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 96 or 384
#'
#' @return
n_cols_from_wells <- function(x) {
  stopifnot(is.numeric(x))
  switch(as.character(x),
         "6" = 3,
         "12" = 4,
         "24" = 6,
         "96" = 12,
         "384" = 24)
}

#' Calculate number of rows from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 384 or 96.
#'
#' @return
n_rows_from_wells <- function(x) {
  stopifnot(is.numeric(x))
  switch(as.character(x),
         "6" = 2,
         "12" = 3,
         "24" = 4,
         "96" = 8,
         "384" = 16)
}
