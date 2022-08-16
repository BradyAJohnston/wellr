#' @noRd
.count_row_down <- function(x, n_rows) {
  .count <- x %% n_rows
  .count[.count == 0] <- n_rows
  .count
}

#' @noRd
.count_col_down <- function(x, n_rows) {
  .count <- x %/% n_rows + 1
  .count[x %% n_rows == 0] <- .count[x %% n_rows == 0] - 1
  .count
}

#' @noRd
.count_row_across <- function(x, n_col) {
  .count <- x %/% n_col + 1
  .count[x %% n_col == 0] <- .count[x %% n_col == 0] - 1
  .count
}

#' @noRd
.count_col_across <- function(x, n_col) {
  .count <- x %% n_col
  .count[.count == 0] <- n_col
  .count
}
