well_check <- function(x) {
 if (is.na(stringr::str_extract(as.character(x), "^[:alpha:]"))) {
  stop("Well ID is missing required row letter.")
 }
 if (is.na(stringr::str_extract(as.character(x), "\\d{1,3}"))) {
   stop("Well ID is missing required column number.")
 }
}

#' Extract number from well ID.
#'
#' @param x Well ID as string in format "A10"
#' @param plate
#'
#' @return
#' @export
#'
#' @examples
well_to_colnum <- function(x, plate = 96) {
  stringr::str_extract(as.character(x), "\\d+$") %>%
    as.numeric()
}

#' Extracts letter from well ID.
#'
#' @param x Well ID as string.
#' @param plate
#'
#' @return
#' @export
#'
#' @examples
well_to_rowlet <- function(x, plate = 96) {
  stringr::str_extract(as.character(x), "^\\w")
}

#' Convert Well ID to Row Number
#'
#' @param x Well ID as a string.
#' @param plate
#'
#' @return Numeric row number.
#' @export
#'
#' @examples
well_to_rownum <- function(x, plate = 96) {
  well_to_rowlet(x) %>%
    let_to_num()
}

well_to_index <- function(x, plate = 96) {

}

well_from_index <- function(x, plate = 96) {

}
