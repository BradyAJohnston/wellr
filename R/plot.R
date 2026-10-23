#' Plot a Plate Map
#'
#' Draws every well of the plate, coloured by `value`. Wells with no data are
#' drawn in grey, so the whole plate is shown even when only some wells were
#' used.
#'
#' @param data Dataframe containing data.
#' @param well Name of the column with well IDs.
#' @param value Name of the column with the values to plot.
#' @param colour Colour of the well borders.
#' @param plate The size of the plate, such as 96 or 384, or its number of rows
#'   and columns. If `NULL`, the plate of [well()] IDs is used, or else the
#'   smallest standard plate that holds all the wells.
#'
#' @importFrom rlang .data
#' @return ggplot2 object.
#' @export
#'
#' @examples
#' dat <- wellr::well_plate()[, "well"]
#' dat$value <- rnorm(96)
#' well_plot(dat, well, value)
#'
#' # Only some wells used: the rest of the plate is drawn empty
#' well_plot(dat[1:30, ], well, value)
well_plot <- function(data, well, value, colour = "black", plate = NULL) {
  check_data_frame(data)
  wells <- dplyr::pull(data, {{ well }})
  dims <- plate_dims_for(wells, plate)
  rows <- LETTERS[seq_len(dims[[1]])]

  data$row <- well_row(wells, plate = dims)
  data$col <- well_to_col_num(wells)

  # Drawn behind the data, and without the data's columns so that it appears
  # in every facet.
  plate_grid <- expand.grid(
    col = seq_len(dims[[2]]),
    row = factor(rows, levels = rows)
  )

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$col,
      y = .data$row,
      fill = {{ value }}
    )
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(x = .data$col, y = .data$row),
      data = plate_grid,
      fill = "grey90",
      colour = colour,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_tile(colour = colour) +
    ggplot2::scale_x_continuous(
      name = NULL,
      expand = ggplot2::expansion(),
      breaks = seq_len(dims[[2]]),
      position = "top"
    ) +
    ggplot2::scale_y_discrete(
      name = NULL,
      expand = ggplot2::expansion(),
      limits = rev(rows)
    ) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::coord_equal() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", linewidth = 1)
    )
}
