

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
  lifecycle::deprecate_warn(
    when = "0.3.2",
    with = "plate_read_meta()",
    what = "read_meta()",
    always = TRUE,
    details = c(
      x = "This function will be removed in {wellr} v0.5.0"
    )

    )
  plate_read_meta(file)
}
