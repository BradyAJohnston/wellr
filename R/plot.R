#' Plot a Plate Map
#'
#' @param data Dataframe containing data.
#' @param well Name of the column with well IDs.
#' @param value Name of the column with the values to plot.
#' @param colour Colour of the well borders.
#'
#' @importFrom rlang .data
#' @return ggplot2 object.
#' @export
#'
#' @examples
#' dat <- wellr::well_plate()
#' dat$value <- rnorm(96)
#' well_plot(dat, well, value)

well_plot <- function(data, well, value, colour = "black") {

  data <- dplyr::mutate(
    data,
    row  = wellr::well_to_row_num({{ well }}),
    row  = forcats::fct_inorder(factor(.data$row)),
    row  = forcats::fct_rev(.data$row),
    well = wellr::well_to_col_num({{ well }})
  )

  ggplot2::ggplot(data, ggplot2::aes(
    x = .data$col,
    y = .data$row,
    fill = {{ value }}
  )) +
    ggplot2::geom_tile(colour = "black") +

    ggplot2::scale_x_continuous(
      name = NULL,
      expand = ggplot2::expansion(),
      breaks = scales::breaks_width(1),
      labels = ~c(LETTERS, paste("A", LETTERS))[.x],
      position = "top"
      ) +
    ggplot2::scale_y_discrete(
      name = NULL,
      expand = ggplot2::expansion()
      ) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", size = 1)
    )

}


