#' Test for start of a data block.
#'
#' @noRd
is_block_start <- function(lines) {
  stringr::str_detect(lines, "^,(Time|Wavelength),[^,]+")
}

#' Test if a line is part of a data block
#' @noRd
is_block_line <- function(lines) {
  stringr::str_detect(lines, paste0(c("^,?", rep("[^,]+,", 10)), collapse = ""))
}

#' Test if a line is a metadata line
#' @noRd
is_meta_lines <- function(lines) {
  cumsum(is_block_start(lines)) == 0
}

#' Test if a line is part of a data block
#' @noRd
is_data_lines <- function(lines) {
  !is_meta_lines(lines)
}

#' Read a data block from a vector of strings that are the lines of a .csv
#'
#' @param lines The vector of lines for the csv
#' @param temp Whether to include the temp column or not.
#' @param format Whether to format the well column names.
#'
#'
#' @noRd
read_data_block <- function(lines, temp = FALSE, format = TRUE, rownum = FALSE) {
  lines |>
    stringr::str_remove("^,") |>
    stringr::str_remove(",$") |>
    stringr::str_subset("^[,]+,[,]+,[,]+", negate = TRUE) |>
    stringr::str_c(collapse = "\n") |>
    readr::read_csv(col_types = readr::cols()) -> dat

  if (rownum) {
    dat <- dplyr::mutate(dat, rownum = dplyr::row_number())
  }


  dat <- tidyr::pivot_longer(
    dat,
    cols = which(is_well_id(colnames(dat))),
    values_transform = as.numeric,
    names_to = "well"

  ) |>
    janitor::clean_names() |>
    # change any time formatted columns into seconds / numeric
    dplyr::mutate(dplyr::across(dplyr::matches("time"), as.numeric))

  if (format) {
    dat <- dplyr::mutate(dat, well = well_format(.data$well))
  }

  # remove temp column if not wanted
  if (!temp) {
    dat <- dplyr::select(dat, -dplyr::starts_with("t_"))
  }

  dat
}

#' Read all data blocks from a file, with each dataframe being an entry in a list
#'
#' @param lines Vector of strings that are lines from a .csv file.
#' @param temp Whether to include the temp column of a file.
#' @param format Whether to format the well ID column of a file.
#'
#' @noRd
get_all_blocks <- function(lines, temp = FALSE, format = TRUE, include_id = FALSE, rownum = FALSE) {
  data_block_starts <- which(is_block_start(lines))
  intervals <- c(diff(data_block_starts), NA)

  purrr::map2(data_block_starts, intervals, \(start, interval) {
    if (is.na(interval)) {
      end <- length(lines)
    } else {
      end <- start + interval - 4
    }

    id <- 'value'
    if (include_id) {
      id <- stringr::str_split(lines[start - 2], ",")[[1]][1]
    }

    dat <- read_data_block(
      lines = lines[start:end],
      temp = temp,
      format = format,
      rownum = rownum
    )

    if (include_id) {
      id_clean <- janitor::make_clean_names(id)
      id_split <- stringr::str_split(id_clean, "_")[[1]]
      id_name <- paste0(id_split[c(1, 3)], collapse = "_")
      id_int <- id_split[2]

      dat <- dat |>
        dplyr::mutate(
          idx = as.integer(id_int),
          type = id_name
          )

    }
    dat
  })
}

#' Drop the lines that are "results" from a biotek file
#' @noRd
drop_results <- function(lines) {
  is_results <- cumsum(stringr::str_detect(lines, "^Results")) > 0
  lines[!is_results]
}

#' Get 'wavelength' dataframes
#'
#' Get all of the datafromes from a list of datablocks that include 'wavelength'
#' as a column name.
#'
#' @param blocks List of blocks from `get_all_blocks()`
#'
#' @noRd
get_blocks_wl <- function(blocks) {
  wl <- list()
  counter <- 1

  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    if ("wavelength" %in% colnames(block)) {
      wl[[counter]] <- block
      counter <- counter + 1
    }
  }

  wl
}

#' Title
#'
#' @param time
#' @param idx
#' @param interval
#'
#' @return
#' @export
#'
#' @examples
accumulate_time <- function(time, idx, interval = 60 * 20) {
  is_new <- c(FALSE, diff(idx) > 0)
  offset <- cumsum(ifelse(is_new, dplyr::lag(time) + interval, 0))
  time + offset
}

#' Title
#'
#' @param blocks
#' @param interval
#' @param average_time
#'
#' @return
#' @export
#'
#' @examples
get_blocks_time <- function(blocks, interval = 60 * 20, average_time = TRUE) {
  dat <- purrr::keep(blocks, \(x) "time" %in% colnames(x)) |>
    dplyr::bind_rows()

  if (average_time) {
    dat <- dplyr::mutate(dat, time = mean(time), .by = c(.data$rownum, .data$well))
  }

  dat |>
    dplyr::mutate(
      time = accumulate_time(time, idx),
      .by = c(well, type)
    )

}

#' Read the `wavelength` data blocks from a _biotek_ `.csv` file.
#'
#' @param file File path to the `.csv` file.
#' @param format Whether to format the `well` column of the returned dataframes.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' file_data <- system.file(
#'   "extdata",
#'   "2024-02-29_vio_GFP_main.csv",
#'   package = "wellr"
#' )
#'
#' plate_read_biotek_wl(file_data)
#' plate_read_biotek_wl(file_data, format = FALSE)
plate_read_biotek_wl <- function(file, format = TRUE) {
  # get the lines and drop irrelevant ones
  lines <- readr::read_lines(file) |>
    drop_results()

  blocks <- get_all_blocks(lines, temp = FALSE, format = format)

  blocks_wl <- get_blocks_wl(blocks)

  dplyr::bind_rows(blocks_wl, .id = "id")
}
