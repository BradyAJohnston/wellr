#' Read a Tecan output file as a .csv
#'
#' @param file File path to the file.
#' @param temp Whether to include the temperature column
#'
#' @keywords internal
.plate_read_tecan_csv <- function(file, temp = FALSE) {
  lines <- readr::read_lines(file)
  groups <- cumsum(is_block_start(lines))
  unique_groups <- unique(groups)
  unique_groups <- unique_groups[unique_groups != 0]

  blocks <- vector(mode = 'list', length = length(unique_groups))

  for (group in unique_groups) {
    # create the mask for reading the data
    mask <- which(groups == group)
    # drop the last row of each group as that picks up the info line for the next
    # block of data to be read
    mask <- mask[1:(length(mask) - 1)]

    dat <- readr::read_csv(
      file = I(stringr::str_c(lines[mask], collapse = "\n")),
      col_types = readr::cols()
    )

    dat <- janitor::clean_names(dat)
    info_line <- lines[mask[1] - 1]
    dat$type <- stringr::str_split(info_line, ',')[[1]][1]
    well_columns <- which(is_well_id(colnames(dat)))
    dat <- tidyr::pivot_longer(
      data = dat,
      cols = dplyr::any_of(well_columns),
      names_to = "well",
      names_transform = well_format
    )
    types <- c("OD600:600" = "od600", "LUMI:Lum" = "lumi")
    dat$type <- unname(types[dat$type])
    blocks[[group]] <- dat
  }

  dat_combined <- dplyr::bind_rows(blocks) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("temp|time"), mean),
      .by = "cycle_nr"
    )

  if (!temp) {
    dat_combined <- dplyr::select(dat_combined, -dplyr::matches("temp"))
  }

  tidyr::pivot_wider(dat_combined, values_from = "value", names_from = "type")
}


#' Read a tecan output file as a .xlsx
#'
#' @param file Path to the file
#' @param temp Whether to include the temperature column
#'
#' @keywords internal
.plate_read_tecan_xslx <- function(file, temp = FALSE) {
  dat <- suppressMessages({
    readxl::read_excel(file, col_names = FALSE, .name_repair = "unique")
  })
  dat <- janitor::clean_names(dat)

  vec <- stringr::str_detect(dplyr::pull(dat, .data$x1), "Cycle Nr")
  vec <- dplyr::if_else(is.na(vec), FALSE, vec)
  vec <- cumsum(vec)
  vec <- c(vec[-1], max(vec))

  dat <- dat |>
    dplyr::mutate(
      chunk = vec,
    ) |>
    tidyr::nest(.by = .data$chunk) |>
    dplyr::filter(.data$chunk != 0) |>
    dplyr::mutate(
      signal = purrr::map_chr(.data$data, function(x) x[1, 1, drop = TRUE]),
      data = purrr::map(.data$data, dplyr::slice, -1),
      data = purrr::map(.data$data, janitor::row_to_names, row_number = 1)
    ) |>
    tidyr::unnest(.data$data) |>
    janitor::clean_names() |>
    tidyr::drop_na(.data$cycle_nr)

  dat <- dat |>
    tidyr::pivot_longer(
      cols = which(is_well_id(colnames(dat))),
      names_to = "well",
      names_transform = wellr::well_format,
      values_transform = as.numeric,
    ) |>
    tidyr::drop_na(.data$value) |>
    dplyr::select(-"chunk") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("cycle_nr|time_s|temp"),
        as.numeric
      ),
      signal = stringr::str_extract(.data$signal, ".+(?=:)")
    )

  dat <- dat |>
    tidyr::drop_na() |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("time|temp"), mean),
      .by = "cycle_nr"
    ) |>
    tidyr::pivot_wider(values_from = "value", names_from = "signal") |>
    janitor::clean_names()

  if (temp) {
    dat
  } else {
    dat <- dplyr::select(dat, -dplyr::matches("temp"))
    dat
  }
}

.check_file_exists <- function(file, env = rlang::caller_env()) {
  if (!file.exists(file)) {
    cli::cli_abort(
      c(
        "!" = 'File {.file {file}} doesn\'t exist!',
        "i" = "Maybe you mistyped something?"
      ),
      .envir = env
    )
  }
}

#' Read the output of Tecan Plate Readers
#'
#' @param file File path to the `.xlsx` or `.csv` file.
#' @param temp Logical, whether to include the temperature column.
#' @param plate The size of the plate, such as 96 or 384, or its number of
#'   rows and columns. If `NULL`, the plate size recorded in the file is used,
#'   or else the smallest standard plate that holds all the wells.
#'
#' @return a `tibble::tibble()` of the values from the file. The `well` column
#'   holds [well()] IDs that know the size of the plate.
#' @export
#' @examples
#'
#' # read a .csv tecan file
#' fl <- system.file(
#'   'extdata', 'tecanON1.csv',
#'   package = 'wellr'
#' )
#'
#' plate_read_tecan(fl)
#'
#' # read a .xlsx tecan file
#' fl <- system.file(
#'   'extdata', 'tecanON1.xlsx',
#'   package = 'wellr'
#' )
#'
#' plate_read_tecan(fl)
#'

plate_read_tecan <- function(file, temp = FALSE, plate = NULL) {
  .check_file_exists(file)

  ext <- tools::file_ext(file)
  if (ext == "csv") {
    read_func <- .plate_read_tecan_csv
  } else if (ext == "xlsx") {
    read_func <- .plate_read_tecan_xslx
  } else {
    cli::cli_abort("Unable to format of file: {.file {file}}")
  }

  dat <- read_func(file = file, temp = temp)
  as_plate_wells(dat, plate, .tecan_recorded_plate(file, ext))
}

# The plate recorded in a Tecan file, on the row starting "Plate".
.tecan_recorded_plate <- function(file, ext) {
  if (ext == "csv") {
    return(recorded_plate_size(readr::read_lines(file), "Plate,"))
  }
  sheet <- suppressMessages(
    readxl::read_excel(file, col_names = FALSE, col_types = "text")
  )
  rows <- which(sheet[[1]] %in% "Plate")
  if (length(rows) == 0) {
    return(NULL)
  }
  values <- unlist(sheet[rows[[1]], ], use.names = FALSE)
  plate_size_from_text(paste(values[!is.na(values)], collapse = " "))
}
