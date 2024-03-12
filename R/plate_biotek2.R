

is_block_start <- function(lines) {
  stringr::str_detect(lines, "^,(Time|Wavelength),[^,]+")
}

is_block_line <- function(lines) {
  stringr::str_detect(lines, "^,[^,]+,[^,]+,[^,]+,")
}

is_meta_lines <- function(lines) {
  cumsum(is_block_start(lines)) == 0
}

is_data_lines <- function(lines) {
  !is_meta_lines(lines)
}

read_data_block <- function(lines, drop_temp = TRUE) {
  lines |>
    stringr::str_remove("^,") |>
    stringr::str_remove(",$") |>
    stringr::str_c(collapse = "\n") |>
    readr::read_csv(col_types = readr::cols()) -> dat


  dat <- tidyr::pivot_longer(
    dat,
    cols = which(is_well_id(colnames(dat))),
    values_transform = as.numeric,
    names_to = "well"
  ) |>

    janitor::clean_names() |>

    # change any time formatted columns into seconds / numeric
    dplyr::mutate(dplyr::across(dplyr::matches("time"), as.numeric))

  if (drop_temp) {
    dat <- dplyr::select(dat, -dplyr::starts_with('t_'))
  }

  dat
}

get_all_blocks <- function(lines, drop_temp = TRUE) {
  data_block_starts <- which(is_block_start(lines))
  intervals <- c(diff(data_block_starts), NA)

  purrr::map2(data_block_starts, intervals, \(start, interval) {
    if (is.na(interval)) {
      end = length(lines)
    } else {
      end = start + interval - 4
    }

    read_data_block(lines[start:end], drop_temp = drop_temp)
  })
}

# all_blocks <- get_all_blocks(lines)
drop_results <- function(lines) {
  is_results <- cumsum(stringr::str_detect(lines, '^Results')) > 0
  lines[!is_results]
}

get_blocks_wl <- function(blocks) {
  wl <- list()
  counter <- 1

  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    if ("wavelength" %in% colnames(block)) {
      wl[[counter]] <- block
      counter = counter + 1
    }
  }

  wl
}

read_biotek_wl <- function(file) {
  # get the lines and drop irrelevant ones
  lines <- readr::read_lines(file) |>
    drop_results()

  blocks <- get_all_blocks(lines, drop_temp = TRUE)

  blocks_wl <- get_blocks_wl(blocks)

  dplyr::bind_rows(blocks_wl, .id = 'id')
}



