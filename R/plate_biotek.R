#' The pre prefix onto some number of repeating .csv entris
#'
#' Useful for constructing regex for csv line identification
#' @noRd
.multi_entry <- function(pre = "^,?", reps = 8) {
  paste0(c(pre, rep("[^,]+,", reps)), collapse = "")
}

#' Test for start of a data block.
#'
#' @noRd
is_block_start <- function(lines) {
  stringr::str_detect(lines, .multi_entry("^,?(Time|Wavelength),")) |
    stringr::str_detect(lines, '^,?Cycle Nr\\.,')
}

#' Test if a line is part of a data block
#' @noRd
is_block_line <- function(lines) {
  stringr::str_detect(lines, .multi_entry("^,?"))
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

#' Remove blank read lines
#' @noRd
.drop_empty_reads <- function(lines) {
  stringr::str_subset(lines, "0:00:00,,,", negate = TRUE)
}


#' Read a data block from a vector of strings that are the lines of a .csv
#'
#' @param lines The vector of lines for the csv
#' @param temp Whether to include the temp column or not.
#' @param format Whether to format the well column names.
#'
#'
#' @keywords internal
read_data_block <-
  function(lines,
           temp = FALSE,
           format_well = TRUE,
           rownum = FALSE) {
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

    if (format_well) {
      dat <- dplyr::mutate(dat, well = well_format(.data$well))
    }

    # remove temp column if not wanted
    if (!temp) {
      dat <- dplyr::select(dat, -dplyr::starts_with("t_"))
    }

    dat
  }

#' Split label lines into manageable chunks
#'
#' @keywords internal
.split_labels <- function(label_lines) {
  label_lines |>
    stringr::str_remove_all("(^,|,,|\")") |>
    stringr::str_split(":") |>
    purrr::map(\(x) {
      stringr::str_replace(x, ",", "_") |>
        stringr::str_remove_all('/\\d{1,2}') |>
        stringr::str_to_lower()
    })
}


#' Read all data blocks from a file, with each dataframe being an entry in a
#'list
#'
#' @param lines Vector of strings that are lines from a .csv file.
#' @param temp Whether to include the temp column of a file.
#' @param format_well Whether to format the well ID column of a file.
#' @param include_id Whether to include the label / ID information for the data
#'  block as a column in the resulting data frame.
#' @param second_wl Whether to include the second possible wavelength in column
#'  names when reading in.
#' @param rownum Whether to include row numbers from pre-pivoted blocks when
#'  reading in.
#'
#' @keywords internal
get_all_blocks <-
  function(lines,
           temp = FALSE,
           format_well = TRUE,
           include_id = FALSE,
           second_wl = TRUE,
           rownum = FALSE) {
    data_block_starts <- which(is_block_start(lines))
    intervals <- c(diff(data_block_starts), NA)

    purrr::map2(data_block_starts, intervals, \(start, interval) {
      if (is.na(interval)) {
        end <- length(lines)
      } else {
        end <- start + interval - 4
      }


      dat <- read_data_block(
        lines = lines[start:end],
        temp = temp,
        format_well = format_well,
        rownum = rownum
      )


      id <- "value"
      if (include_id) {
        label_line <- lines[start - 2]
        split_label <- .split_labels(label_line)[[1]]


        name <- stringr::str_split(split_label[1], '_')[[1]][1]
        id_int <- as.integer(stringr::str_split(split_label[1], '_')[[1]][2])
        extra_info <- split_label[2]

        if (!second_wl) {
          extra_info <- stringr::str_remove_all(extra_info, '_\\d+')
        }

        new_label <- paste0(c(name, extra_info), collapse = '_')

        dat <- dplyr::mutate(dat, idx = id_int, type = new_label)
      }

      dat
    })
  }

#' Drop the lines that are "results" from a biotek file
#'
#' @param lines Character vector of lines. Drops all lines after the 'Results'
#'  headings in biotek .csv files
#'
#' @keywords internal
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

#' @noRd
accumulate_time <- function(time, idx, interval = 0) {
  is_new <- c(FALSE, diff(idx) > 0)
  offset <- cumsum(ifelse(is_new, dplyr::lag(time) + interval, 0))
  time + offset
}

#' @noRd
get_blocks <- function(blocks, colname) {
  purrr::keep(blocks, \(x) colname %in% colnames(x))
}

#' @noRd
get_blocks_time <-
  function(blocks,
           interval = 0,
           accumulate_time = TRUE) {
    dat <- get_blocks(blocks, "time") |>
      dplyr::bind_rows()

    average_time = TRUE
    if (average_time) {
      dat <- dplyr::mutate(dat,
                           time = mean(.data$time),
                           .by = dplyr::matches("rownum|well"))
    }

    if (accumulate_time) {
      dat <- dplyr::mutate(
        dat,
        time = accumulate_time(.data$time, .data$idx, interval = interval),
        .by = c("well", "type")
      ) |>
        dplyr::select(-dplyr::matches("^rownum$"))
    }

    dat |>
      dplyr::select(-dplyr::matches("^idx$"))
  }

#' @noRd
.plate_read_lines <- function(file) {
  readr::read_lines(file) |>
    drop_results() |>
    .drop_empty_reads()
}

#' drop-in replacement for plate_read_biotek
#' @keywords internal
plate_read_biotek2 <- function(file,
                               average_time = TRUE,
                               accumulate_time = TRUE,
                               interval = 0,
                               second_wl = TRUE,
                               rename = TRUE) {
  lines <- .plate_read_lines(file)

  blocks <-
    get_all_blocks(lines,
                   temp = FALSE,
                   include_id = TRUE,
                   second_wl = second_wl,
                   rownum = TRUE)

  blocks_time <- get_blocks_time(blocks,
                                 interval = interval,
                                 accumulate_time = accumulate_time)

  dat_wide <- tidyr::pivot_wider(dplyr::bind_rows(blocks_time),
                                 names_from = "type",
                                 values_from = "value")


  if (rename) {
    new_col_names <- c("od600" = "od600_600",
                       "lum" = "lum_lum")
    new_col_names <-
      new_col_names[new_col_names %in% colnames(dat_wide)]

    if (length(new_col_names) > 0) {
      for (i in seq_along(new_col_names)) {
        old_name <- new_col_names[i]
        new_name <- names(new_col_names)[i]
        colnames(dat_wide)[colnames(dat_wide) == old_name] = new_name
      }

      # dat_wide <- dplyr::rename(dat_wide, new_col_names)
    }
  }

  dplyr::arrange(dat_wide, .data$time, .data$well)
}

#' Read Biotek Output CSV Files
#'
#' @param file The filepath to a `.csv` file, exported from a Biotek plate
#' reader.
#' @param time_average Logical, whether to average the time points across
#'  different observations (LUM / OD) and pivot them to their own columns.
#' @param second_wl Whether to include the second wavelength when reading in
#'    data blocks, or to keep just the label and the first wavelength.
#'
#' @return tibble::tibble()
#' @export
#'
#' @examples
#' file_data <- system.file(
#'   "extdata",
#'   "2024-02-29_vio_GFP_main.csv",
#'   package = "wellr"
#' )
#'
#' plate_read_biotek(file_data)
#' plate_read_biotek(file_data, second_wl = TRUE)
plate_read_biotek <- function(file, time_average = TRUE, second_wl = FALSE) {
  .check_file_exists(file)
  plate_read_biotek2(file, average_time = time_average, second_wl = second_wl)
}

#' Read the `wavelength` data blocks from a _biotek_ `.csv` file.
#'
#' @param file File path to the `.csv` file.
#' @param format_well Whether to format the `well` column of the returned
#'  dataframes.
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
plate_read_biotek_wl <- function(file, format_well = TRUE) {
  .check_file_exists(file)
  # get the lines and drop irrelevant ones
  lines <- .plate_read_lines(file)

  blocks <-
    get_all_blocks(lines, temp = FALSE, format_well = format_well)

  blocks_wl <- get_blocks_wl(blocks)

  dplyr::bind_rows(blocks_wl, .id = "id")
}
