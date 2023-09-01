#' @noRd

.is_signal <- function(x) {
  reg <- "([:upper:]|[:digit:])+(?=\\_)"
  stringr::str_detect(x, reg)
}


#' @noRd

.nest_data_chunks <- function(dat) {
  dat %>%
    dplyr::rename("signal" = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate("signal" = vctrs::vec_fill_missing(.data$signal)) %>%
    dplyr::filter(.is_signal(.data$signal)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$signal,
                    chunk = cumsum(stringr::str_detect(
                      .data$x2, stringr::fixed("time", ignore_case = TRUE)
                    ))) %>%
    tidyr::nest() %>%
    dplyr::group_by(.data$signal) %>%
    dplyr::mutate(
      signal_chunk = as.numeric(stringr::str_extract(.data$signal, "(?<=\\_)(\\d)")),
      signal_chunk_chunk = dplyr::row_number(),
      signal = stringr::str_extract(.data$signal, ".+(?=\\_)") %>%
        stringr::str_to_lower()
    ) %>%
    dplyr::select(
      "signal",
      "chunk",
      "signal_chunk",
      "signal_chunk_chunk",
      "data"
    )
}

#' @noRd
#'
.chunk_pivot <- function(chunk) {
  chunk %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(
      -c(1, 2),
      names_to = "well",
      names_transform = wellr::well_format,
      values_to = "value",
      values_transform = as.numeric
    ) %>%
    dplyr::select(-dplyr::starts_with("t_")) %>%
    dplyr::mutate(time = as.numeric(lubridate::hms(.data$time)))
}

#' @noRd
#'
.chunk_unnest <- function(data, time_average = TRUE) {
  data <- data %>%
    dplyr::mutate(data = purrr::map(.data$data, .chunk_pivot)) %>%
    dplyr::arrange(.data$signal, .data$signal_chunk)

  adjust_times <- data %>%
    dplyr::group_by(.data$signal, .data$signal_chunk) %>%
    dplyr::reframe(max_time = purrr::map_dbl(data, function(x) max(x$time))) %>%
    dplyr::group_by(.data$signal) %>%
    unique() %>%
    dplyr::mutate(adjust = cumsum(dplyr::lag(.data$max_time, default = 0)),
                  id = paste(.data$signal, .data$signal_chunk, sep = "_")) %>%
    dplyr::pull(.data$adjust, .data$id)


  data <- data %>%
    dplyr::mutate(data = purrr::map2(
      .data$data,
      paste(.data$signal, .data$signal_chunk, sep = "_"),
      \(x, y) dplyr::mutate(x, time = .data$time + adjust_times[y])
    )) %>%
    tidyr::unnest("data") %>%
    tidyr::pivot_wider(
      id_cols = c("time", "signal"),
      names_from  = "well",
      values_from = "value"
    ) %>%
    dplyr::group_by(.data$signal) %>%
    dplyr::mutate(time_point = dplyr::row_number()) %>%
    dplyr::mutate() %>%
    tidyr::pivot_longer(
      cols = -c("time", "signal", "time_point"),
      names_to = "well"
      )

  if (time_average) {
    data %>%
      dplyr::group_by(.data$time_point) %>%
      dplyr::mutate(time = mean(.data$time))
  } else {
    data %>%
      dplyr::ungroup()
  }
}

#' @noRd
#'
.signal_wider <- function(data) {
  data %>%
    dplyr::group_by(.data$time_point) %>%
    dplyr::mutate(time = mean(.data$time)) %>%
    tidyr::pivot_wider(names_from  = "signal",
                       values_from = "value") %>%
    dplyr::ungroup() %>%
    dplyr::select(-"time_point")
}

#' Read Biotek Output CSV Files
#'
#' @param file The filepath to a `.csv` file, exported from a Biotek plate
#' reader.
#' @param time_average Logical, whether to aveage the time points across different observations (LUM / OD) and pivot them mto their own columns.
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
plate_read_biotek <- function(file, time_average = TRUE) {
  dat <-
    readr::read_csv(file, col_types = readr::cols(), col_names = FALSE)

  dat <- dat %>%
    .nest_data_chunks() %>%
    .chunk_unnest(time_average = time_average)

  if (time_average) {
    dat %>%
      .signal_wider()
  } else {
    dat
  }


}
