#' @noRd
.is_blank_line <- function(x) {
  pattern <- "^(,|\t|NA)"
  grepl(pattern, x)
}

#' @noRd
.mask_blank_lines <- function(lines) {
  lines[!.is_blank_line(lines)]
}

#' @noRd
.lines_to_list <- function(file) {
  lines <- readr::read_lines(file)
  block_ends <- which(.is_blank_line(lines))
  block_length <- block_ends[1] - 1

  blocks <- purrr::map(c(1, block_ends + 1), \(x) {
    i <- seq(x, length.out = block_length)
    .mask_blank_lines(lines[i])
  })
  blocks
}

#' @noRd
.read_block <- function(block) {
  block <- readr::read_csv(
    paste0(block, collapse = "\n"),
    col_types = readr::cols()
    )
}

#' @noRd
.pivot_plate <- function(plate) {
  col_names <- colnames(plate)
  meta_name <- col_names[1]
  col_numbers <- col_names[-1]

  col_pairs <- lapply(col_numbers, \(x) {
    dat <- plate[, c(meta_name, x)]
    colnames(dat) <- c('row', meta_name)
    dat$col <- x
    dat$well <- well_join(dat$row, dat$col)

    dat[, c("well", meta_name)]
  })

  purrr::reduce(col_pairs, rbind)
}

#' @noRd
.guess_plate_size <- function(x) {
  if (is.numeric(x)) {
    n_well <- x
  } else if (is.data.frame(x)) {
    n_well <- nrow(x)
  } else {
    stop("Must be numeric or dataframe")
  }
  if (n_well > 1536) {
    stop("Too many wells!")
  }

  sizes <- c(6, 24, 48, 96, 384, 1536)
  lower <- c(0, sizes[-6])
  sizes[n_well <= sizes & n_well > lower]
}

#' Read a `{plater}` formatted metadata file
#'
#' @param file to read metadata from.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' file <- system.file("extdata", "plate_layout_96.csv", package = "wellr")
#' read_meta(file)

read_meta <- function(file) {
  lines <- .lines_to_list(file)
  blocks <- purrr::map(lines, .read_block)
  cols <- purrr::map(blocks, .pivot_plate)
  dat <- purrr::reduce(cols, dplyr::inner_join, by = "well")
  dat
}
