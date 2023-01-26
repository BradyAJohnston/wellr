#' @noRd

.nest_data_chunks <- function(dat, signals = "LUM|OD") {
  dat |>
    dplyr::rename("signal" = 1) |>
    janitor::clean_names() |>
    dplyr::mutate("signal" = vctrs::vec_fill_missing(.data$signal)) |>
    dplyr::filter(stringr::str_detect(.data$signal, signals)) |>
    tidyr::drop_na() |>
    dplyr::group_by(.data$signal,
      chunk = cumsum(stringr::str_detect(
        .data$x2, stringr::fixed("time", ignore_case = TRUE)
      ))
    ) |>
    tidyr::nest() |>
    dplyr::group_by(.data$signal) |>
    dplyr::mutate(
      signal_chunk = as.numeric(stringr::str_extract(.data$signal, "(?<=\\_)(\\d)")),
      signal_chunk_chunk = dplyr::row_number(),
      signal = stringr::str_extract(.data$signal, ".+(?=\\_)") |>
        stringr::str_to_lower()
    ) |>
    dplyr::select(
      .data$signal,
      .data$chunk,
      .data$signal_chunk,
      .data$signal_chunk_chunk,
      .data$data
    )
}

#' @noRd
#'
.chunk_pivot <- function(chunk) {
  chunk |>
    janitor::row_to_names(1) |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      -c(1, 2),
      names_to = "well",
      names_transform = wellr::well_format,
      values_to = "value",
      values_transform = as.numeric
    ) |>
    dplyr::select(-dplyr::starts_with("t_")) |>
    dplyr::mutate(time = as.numeric(lubridate::hms(.data$time)))
}

#' @noRd
#'
.chunk_unnest <- function(data, time_average = TRUE) {
  data <- data |>
    dplyr::mutate(data = purrr::map(.data$data, .chunk_pivot)) |>
    dplyr::arrange(.data$signal, .data$signal_chunk) |>
    tidyr::unnest(.data$data) |>
    tidyr::pivot_wider(
      id_cols = c(.data$time, .data$signal),
      names_from = .data$well,
      values_from = .data$value
    ) |>
    dplyr::group_by(.data$signal) |>
    dplyr::mutate(
      time = .data$time + cumsum(ifelse(
        .data$time < dplyr::lag(.data$time, default = 0), dplyr::lag(.data$time), 0
      )),
      time_point = dplyr::row_number()
    ) |>
    dplyr::mutate() |>
    tidyr::pivot_longer(-c(.data$time, .data$signal, .data$time_point), names_to = "well")

  if (time_average) {
    data |>
      dplyr::group_by(.data$time_point) |>
      dplyr::mutate(time = mean(.data$time))
  } else {
    data |>
      dplyr::ungroup()
  }
}

#' @noRd
#'
.signal_wider <- function(data) {
  data |>
    dplyr::group_by(.data$time_point) |>
    dplyr::mutate(time = mean(.data$time)) |>
    tidyr::pivot_wider(
      names_from = .data$signal,
      values_from = .data$value
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$time_point)
}

#' Read Biotek Output CSV Files
#'
#' @param file The filepath to a `.csv` file, exported from a Biotek plate
#' reader.
#'
#' @return tibble::tibble()
#' @export
#'
#' @examples
#' file_data <- system.file(
#'   "extdata",
#'   "20220929_1steptimer20.csv",
#'   package = "wellr"
#' )
#'
#' plate_read_biotek(file_data)
plate_read_biotek <- function(file) {
  dat <-
    readr::read_csv(file, col_types = readr::cols(), col_names = FALSE)

  dat <- dat |>
    .nest_data_chunks() |>
    .chunk_unnest()

  dat |>
    .signal_wider()

}


#' Read a `{plater}` Formatted Metadata File
#'
#' @param file File path to a `{plater}` formatted `.csv` file which contains
#' metadata information.
#' @param sep The separator for the `.csv` file, defaults to ",".
#'
#' @return a tibble::tibble()
#' @export
#'
#' @examples
#'
#' file_meta <- system.file(
#'   "extdata",
#'   "20220929_1steptimer20_metainfo.csv",
#'   package = "wellr"
#' )
#'
#' plate_read_meta(file_meta)
plate_read_meta <- function(file, sep = ",") {
  plater::read_plate(file, well_ids_column = "well", sep = sep)
}

#' Add `{plater{` Formatted Metadata to a Dataset
#'
#' @param data A tibble::tibble() which contains a 'well' column, that will have
#' the metadata added to it.
#' @param file File patch to a `{plater}` formatted `.csv` file which contains
#' the metadat information.
#'
#' @return a tibble::tibble()
#' @export
#'
#' @examples
#' file_data <- system.file(
#'   "extdata",
#'   "20220929_1steptimer20.csv",
#'   package = "wellr"
#' )
#' file_meta <- system.file(
#'   "extdata",
#'   "20220929_1steptimer20_metainfo.csv",
#'   package = "wellr"
#' )
#'
#' plate_read_biotek(file_data) |>
#'   plate_add_meta(file_meta)
plate_add_meta <- function(data, file) {
  data |>
    dplyr::left_join(plate_read_meta(file), by = "well")
}
