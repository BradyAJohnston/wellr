.read_block <- function(block_lines) {

  single_string <- stringr::str_c(block_lines, sep = "\n")

  block <- readr::read_csv(
    I(single_string),
    col_types = readr::cols()
  )

  name <- colnames(block)[1]
  block <- block |>
    tidyr::pivot_longer(
      cols = c(-1),
      names_transform = as.numeric,
      names_to = "col"
    )
  colnames(block)[colnames(block) == name] = "row"
  colnames(block)[colnames(block) == "value"] = name

  dplyr::mutate(block, well = well_join(row, col)) |>
    dplyr::select(-c(row, col))
}

.read_blocks <- function(lines, blocks) {
  purrr::map(unique(blocks), \(x) {

    line_block <- lines[blocks == x]
    .read_block(line_block)

  })
}

.read_meta <- function(file) {
  lines <- readr::read_lines(file)
  # find the gaps
  is_blank <- stringr::str_starts(lines, ",")
  blocks <- cumsum(is_blank)

  # remove lines where it is just blanks
  blocks <- blocks[!is_blank]
  lines <- lines[!is_blank]

  # read each block of values as a list of dataframes
  blocks_list <- .read_blocks(lines, blocks)
  # squish down to a single tibble
  dat <- purrr::reduce(blocks_list, dplyr::left_join, by = dplyr::join_by(well))

  # return with `well` as first column
  dplyr::select(dat, well, dplyr::everything())
}

read_meta <- function(file, id = "id") {
  if (length(file) > 1) {
    purrr::map(file, .read_meta) |>
      dplyr::bind_rows(.id = id)
  } else {
    .read_meta(file)
  }
}
