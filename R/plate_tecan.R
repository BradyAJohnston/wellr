#' Read the output of Tecan Plate Readers
#'
#' @param file File path to the `.xlsx` or `.csv` file.
#' @param temp Logical, whether to include the temperature column.
#'
#' @return a `tibble::tibble()` of the values from the file.
#' @export
#'
plate_read_tecan <- function(file, temp = FALSE) {
  dat <- suppressMessages({readxl::read_excel(file, col_names = FALSE, .name_repair = "unique")})
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
      signal = purrr::map_chr(.data$data, \(x) x[1, 1, drop = TRUE]),
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

  if (temp) {
    dat
  } else {
    dat <- dplyr::select(dat, -dplyr::matches("temp"))
    dat
  }

}


