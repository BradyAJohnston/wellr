test_that("well_plot() draws the whole plate", {
  data <- well_plate()[1:30, "well"]
  data$value <- seq_len(30)

  built <- ggplot2::ggplot_build(well_plot(data, well, value))
  expect_equal(nrow(built$data[[1]]), 96)
  expect_equal(nrow(built$data[[2]]), 30)
  expect_equal(built$layout$panel_params[[1]]$y$get_limits(), rev(LETTERS[1:8]))
})

test_that("well_plot() uses the plate argument and colour", {
  data <- tibble::tibble(well = c("A01", "B02"), value = 1:2)

  built <- ggplot2::ggplot_build(well_plot(
    data,
    well,
    value,
    colour = "white",
    plate = 384
  ))
  expect_equal(nrow(built$data[[1]]), 384)
  expect_equal(unique(built$data[[2]]$colour), "white")
})

test_that("well_plot() shows the whole plate in every facet", {
  data <- well_plate()[1:10, "well"]
  data <- rbind(data, data)
  data$value <- seq_len(20)
  data$group <- rep(c("a", "b"), each = 10)

  plot <- well_plot(data, well, value) + ggplot2::facet_wrap(~group)
  built <- ggplot2::ggplot_build(plot)
  expect_equal(nrow(built$layout$layout), 2)
  expect_equal(nrow(built$data[[1]]), 2 * 96)
})
