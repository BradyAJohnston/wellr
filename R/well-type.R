#' Well IDs that know their plate
#'
#' @description
#' A `wellr_well` vector holds well IDs such as `"A01"`, along with the size of
#' the plate they come from. It is a character vector underneath, so it prints,
#' compares, joins and works with string functions exactly as well IDs always
#' have. Because it knows the plate size, functions such as [well_to_index()]
#' don't need to be told it.
#'
#' * `well()` creates well IDs, formatting them as `"A01"`.
#' * `as_well()` converts a vector to well IDs, keeping their formatting.
#' * `is_well()` tests whether an object is a vector of well IDs.
#' * `well_plate_size()` and `well_plate_dims()` give the size of the plate,
#'   as the number of wells or as the number of rows and columns.
#'
#' Combining well IDs from plates of the same size gives well IDs; combining
#' them with plain character vectors, or with well IDs from a plate of a
#' different size, gives a character vector. Use `as.character()` to get the
#' plain character vector.
#'
#' @param x For `well()` and `as_well()`, a character vector of well IDs. For
#'   the other functions, an object to test or a vector of well IDs.
#' @param plate The plate the wells come from: either the number of wells in a
#'   standard plate (6, 12, 24, 48, 96 or 384), or the number of rows and
#'   columns. If `NULL`, `as_well()` keeps the plate of a `wellr_well` and
#'   otherwise uses the smallest standard plate that contains all the wells.
#' @returns `well()` and `as_well()` return a `wellr_well` vector.
#'   `is_well()` returns a single logical. `well_plate_size()` returns the
#'   number of wells and `well_plate_dims()` the number of rows and columns.
#' @export
#' @examples
#' x <- well(c("a1", "B2", "h12"), plate = 96)
#' x
#'
#' x == "B02"
#' well_to_index(x)
#' well_plate_size(x)
#'
#' as_well(c("A01", "P24"))
well <- function(x = character(), plate = 96) {
  x <- vctrs::vec_cast(x, character())
  dims <- plate_dims(plate)
  check_wells(x, dims)
  ids <- x
  ok <- !is.na(x)
  ids[ok] <- well_format(x[ok])
  new_well(ids, dims)
}

#' @rdname well
#' @export
as_well <- function(x, plate = NULL) {
  as_well_impl(x, plate)
}

as_well_impl <- function(x, plate = NULL, call = rlang::caller_env()) {
  if (is_well(x) && is.null(plate)) {
    return(x)
  }
  x <- vctrs::vec_cast(x, character(), call = call)
  dims <- if (is.null(plate)) {
    infer_plate_dims(x)
  } else {
    plate_dims(plate, arg = "plate", call = call)
  }
  check_wells(x, dims, call = call)
  new_well(x, dims)
}

#' @rdname well
#' @export
is_well <- function(x) {
  inherits(x, "wellr_well")
}

#' @rdname well
#' @export
well_plate_size <- function(x) {
  check_is_well(x)
  prod(well_plate_dims(x))
}

#' @rdname well
#' @export
well_plate_dims <- function(x) {
  check_is_well(x)
  attr(x, "dims")
}

new_well <- function(x = character(), dims = c(8L, 12L)) {
  vctrs::new_vctr(
    unclass(x),
    dims = as.integer(dims),
    class = "wellr_well"
  )
}

# Standard plates, as rows and columns.
plate_sizes <- list(
  "6" = c(2L, 3L),
  "12" = c(3L, 4L),
  "24" = c(4L, 6L),
  "48" = c(6L, 8L),
  "96" = c(8L, 12L),
  "384" = c(16L, 24L)
)

plate_dims <- function(
  plate,
  arg = rlang::caller_arg(plate),
  call = rlang::caller_env()
) {
  if (is.numeric(plate) && length(plate) == 2 && all(plate >= 1)) {
    return(as.integer(plate))
  }
  if (
    is.numeric(plate) &&
      length(plate) == 1 &&
      as.character(plate) %in% names(plate_sizes)
  ) {
    return(plate_sizes[[as.character(plate)]])
  }
  cli::cli_abort(
    c(
      "{.arg {arg}} must be a standard plate size or the number of rows and columns.",
      i = "Standard plate sizes are {.val {names(plate_sizes)}}."
    ),
    call = call
  )
}

# The smallest standard plate that contains all the wells.
infer_plate_dims <- function(x, call = rlang::caller_env()) {
  rows <- suppressWarnings(max(well_to_row_num(x), na.rm = TRUE))
  cols <- suppressWarnings(max(well_to_col_num(x), na.rm = TRUE))
  if (!is.finite(rows) || !is.finite(cols)) {
    return(plate_sizes[["96"]])
  }
  for (dims in plate_sizes) {
    if (rows <= dims[[1]] && cols <= dims[[2]]) {
      return(dims)
    }
  }
  c(rows, cols)
}

check_wells <- function(x, dims, call = rlang::caller_env()) {
  ok <- is.na(x) | is_well_id(x)
  if (!all(ok)) {
    cli::cli_abort(
      "{.val {x[!ok][1:min(3, sum(!ok))]}} {?is/are} not {?a/} well ID{?s}.",
      call = call
    )
  }
  rows <- well_to_row_num(x)
  cols <- well_to_col_num(x)
  outside <- !is.na(x) & (rows > dims[[1]] | cols > dims[[2]])
  if (any(outside)) {
    cli::cli_abort(
      "{.val {x[outside][1:min(3, sum(outside))]}} {?is/are} outside a plate of {dims[[1]]} rows and {dims[[2]]} columns.",
      call = call
    )
  }
}

check_is_well <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_well(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be well IDs from {.fn well}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}

# Plate dimensions to use for `x`: from `plate` if given, from `x` if it knows
# its plate, otherwise `default`.
resolve_plate_dims <- function(x, plate, default = 96) {
  if (!is.null(plate)) {
    plate_dims(plate)
  } else if (is_well(x)) {
    well_plate_dims(x)
  } else {
    plate_dims(default)
  }
}

# The plate size recorded in a plate reader file, e.g. "Greiner 96 Flat Bottom".
plate_size_from_text <- function(text) {
  sizes <- names(plate_sizes)
  pattern <- paste0(
    "(?<![0-9])(",
    paste(rev(sizes), collapse = "|"),
    ")(?![0-9])"
  )
  match <- stringr::str_extract(text, pattern)
  match <- match[!is.na(match)]
  if (length(match) == 0) {
    return(NULL)
  }
  as.numeric(match[[1]])
}

# Converts the `well` column of a reader's output to well IDs: on the plate
# given by `plate`, else the plate recorded in the file, else the smallest
# standard plate that holds all the wells.
as_plate_wells <- function(
  data,
  plate = NULL,
  recorded = NULL,
  call = rlang::caller_env()
) {
  if (!"well" %in% names(data)) {
    return(data)
  }
  if (is.null(plate)) {
    plate <- recorded
  }
  data$well <- as_well_impl(as.character(data$well), plate, call = call)
  data
}

# The plate size recorded on the line of `lines` starting with `label`.
recorded_plate_size <- function(lines, label) {
  line <- lines[startsWith(lines, label)]
  if (length(line) == 0) {
    return(NULL)
  }
  plate_size_from_text(line[[1]])
}

# vctrs methods -----------------------------------------------------------------

#' @export
format.wellr_well <- function(x, ...) {
  vctrs::vec_data(x)
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.wellr_well <- function(x, ...) {
  paste0("well:", plate_label(well_plate_dims(x)))
}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.wellr_well <- function(x, ...) {
  paste0("well<", plate_label(well_plate_dims(x)), ">")
}

plate_label <- function(dims) {
  size <- prod(dims)
  standard <- as.character(size) %in%
    names(plate_sizes) &&
    identical(plate_sizes[[as.character(size)]], as.integer(dims))
  if (standard) as.character(size) else paste0(dims[[1]], "x", dims[[2]])
}

# Sort in plate order: by row, then by column.
#' @exportS3Method vctrs::vec_proxy_order
vec_proxy_order.wellr_well <- function(x, ...) {
  data.frame(row = well_to_row_num(x), col = well_to_col_num(x))
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.wellr_well.wellr_well <- function(x, y, ...) {
  if (identical(well_plate_dims(x), well_plate_dims(y))) {
    x
  } else {
    character()
  }
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.wellr_well.character <- function(x, y, ...) {
  character()
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.character.wellr_well <- function(x, y, ...) {
  character()
}

#' @exportS3Method vctrs::vec_cast
vec_cast.wellr_well.wellr_well <- function(x, to, ...) {
  if (!identical(well_plate_dims(x), well_plate_dims(to))) {
    vctrs::stop_incompatible_cast(
      x,
      to,
      ...,
      details = "The wells come from plates of different sizes."
    )
  }
  x
}

#' @exportS3Method vctrs::vec_cast
vec_cast.wellr_well.character <- function(x, to, ...) {
  dims <- well_plate_dims(to)
  check_wells(x, dims)
  new_well(x, dims)
}

#' @exportS3Method vctrs::vec_cast
vec_cast.character.wellr_well <- function(x, to, ...) {
  vctrs::vec_data(x)
}

# Answer the checks ggplot2 and others make of any vector, as for a character
# vector, but don't allow arithmetic.
#' @exportS3Method vctrs::vec_math
vec_math.wellr_well <- function(.fn, .x, ...) {
  if (.fn %in% c("is.finite", "is.infinite", "is.nan")) {
    return(get(.fn, envir = baseenv())(vctrs::vec_data(.x)))
  }
  cli::cli_abort("Can't use {.fn {(.fn)}} on well IDs.")
}

#' @exportS3Method ggplot2::scale_type
scale_type.wellr_well <- function(x) {
  "discrete"
}
