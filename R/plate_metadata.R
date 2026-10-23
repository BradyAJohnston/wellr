#' Read a `{plater}` Formatted Metadata File
#'
#' @param file File path to a `{plater}` formatted `.csv` file which contains
#' metadata information.
#' @param sep The separator for the `.csv` file, defaults to ",".
#' @param plate The size of the plate, such as 96 or 384, or its number of
#'   rows and columns. If `NULL`, the smallest standard plate that holds all
#'   the wells is used.
#'
#' @return a tibble::tibble(). The `well` column holds [well()] IDs that know
#'   the size of the plate.
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
plate_read_meta <- function(file, sep = ",", plate = NULL) {
  dat <- plater::read_plate(file, well_ids_column = "well", sep = sep)
  as_plate_wells(dat, plate)
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
  # Read the metadata on the same plate as the data, so the joined `well`
  # column keeps its plate.
  plate <- if (is_well(data$well)) well_plate_dims(data$well)
  data |>
    dplyr::left_join(plate_read_meta(file, plate = plate), by = "well")
}
