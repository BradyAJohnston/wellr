test_that("well() formats IDs and records the plate", {
  x <- well(c("a1", "H12", NA), plate = 96)

  expect_s3_class(x, "wellr_well")
  expect_equal(as.character(x), c("A01", "H12", NA))
  expect_equal(well_plate_size(x), 96)
  expect_equal(well_plate_dims(x), c(8L, 12L))
  expect_equal(well_plate_dims(well("A1", plate = c(5, 5))), c(5L, 5L))
})

test_that("as_well() keeps IDs and infers the plate", {
  expect_equal(as.character(as_well(c("A1", "B02"))), c("A1", "B02"))
  expect_equal(well_plate_size(as_well(c("A01", "H12"))), 96)
  expect_equal(well_plate_size(as_well(c("A01", "P24"))), 384)
  expect_equal(well_plate_size(as_well(c("A01", "B03"))), 6)

  x <- well("A01", plate = 384)
  expect_identical(as_well(x), x)
})

test_that("well IDs behave like character vectors", {
  x <- well(c("A1", "B2", "H12"))

  expect_type(x, "character")
  expect_equal(x == "B02", c(FALSE, TRUE, FALSE))
  expect_equal(x %in% c("A01", "H12"), c(TRUE, FALSE, TRUE))
  expect_equal(stringr::str_detect(x, "^A"), c(TRUE, FALSE, FALSE))
  expect_equal(paste0(x, "_1"), c("A01_1", "B02_1", "H12_1"))
  expect_equal(levels(factor(x)), c("A01", "B02", "H12"))
})

test_that("well IDs combine with character and other plates as character", {
  x <- well("A1")

  expect_s3_class(vctrs::vec_c(x, well("B2")), "wellr_well")
  expect_type(vctrs::vec_c(x, "B02"), "character")
  expect_equal(class(vctrs::vec_c(x, well("B2", plate = 384))), "character")

  data <- tibble::tibble(well = well(c("A1", "B2")), value = 1:2)
  meta <- tibble::tibble(well = c("A01", "B02"), label = c("x", "y"))
  expect_equal(dplyr::left_join(data, meta, by = "well")$label, c("x", "y"))
})

test_that("well IDs sort in plate order", {
  x <- as_well(c("B1", "A10", "A2"))
  expect_equal(as.character(sort(x)), c("A2", "A10", "B1"))
})

test_that("well IDs work in ggplot2 as a discrete scale", {
  data <- tibble::tibble(well = well(c("A1", "B2")), value = 1:2)
  plot <- ggplot2::ggplot(data, ggplot2::aes(well, value)) +
    ggplot2::geom_col()
  built <- ggplot2::ggplot_build(plot)

  expect_s3_class(built$layout$panel_scales_x[[1]], "ScaleDiscrete")
})

test_that("conversions use the plate of well IDs", {
  x <- well(c("A01", "B01", "P24"), plate = 384)

  expect_equal(well_to_index(x), c(1, 25, 384))
  expect_equal(well_to_index(x, plate = 384), well_to_index(x))
  expect_equal(well_to_index(c("A01", "B01")), c(1, 13))
  expect_equal(well_plate_size(well_from_index(1:10, plate = 384)), 384)
  expect_equal(well_plate_size(well_format(x)), 384)
})

test_that("readers use the plate recorded in the file", {
  biotek_384 <- plate_read_biotek(system.file(
    "extdata",
    "20220929_1steptimer20.csv",
    package = "wellr"
  ))
  biotek_96 <- plate_read_biotek_wl(system.file(
    "extdata",
    "2024-02-29_vio_GFP_main.csv",
    package = "wellr"
  ))
  tecan_csv <- plate_read_tecan(system.file(
    "extdata",
    "tecanON1.csv",
    package = "wellr"
  ))
  tecan_xlsx <- plate_read_tecan(system.file(
    "extdata",
    "tecanON1.xlsx",
    package = "wellr"
  ))

  expect_equal(well_plate_size(biotek_384$well), 384)
  expect_equal(well_plate_size(biotek_96$well), 96)
  expect_equal(well_plate_size(tecan_csv$well), 96)
  expect_equal(well_plate_size(tecan_xlsx$well), 96)
})

test_that("readers take the plate size as an argument", {
  file <- system.file("extdata", "tecanON1.csv", package = "wellr")
  expect_equal(well_plate_size(plate_read_tecan(file, plate = 384)$well), 384)
  expect_snapshot(plate_read_tecan(file, plate = 24), error = TRUE)
})

test_that("plate_add_meta() keeps the plate of the data", {
  data <- plate_read_biotek(system.file(
    "extdata",
    "20220929_1steptimer20.csv",
    package = "wellr"
  ))
  meta <- system.file(
    "extdata",
    "20220929_1steptimer20_metainfo.csv",
    package = "wellr"
  )
  expect_equal(well_plate_size(plate_add_meta(data, meta)$well), 384)
})

test_that("well_mat_to_df() labels wells by the shape of the matrix", {
  df <- well_mat_to_df(matrix(seq_len(384), nrow = 16, byrow = TRUE))

  expect_equal(
    as.character(df$well[c(1, 24, 25, 384)]),
    c("A01", "A24", "B01", "P24")
  )
  expect_equal(df$value[c(24, 25)], c(24, 25))
  expect_equal(well_plate_size(df$well), 384)
})

test_that("errors informatively", {
  expect_snapshot(error = TRUE, {
    well("foo")
    well("Q1", plate = 96)
    well("A1", plate = 100)
    well_plate_size("A01")
    well("A1") + 1
    abs(well("A1"))
  })
})
