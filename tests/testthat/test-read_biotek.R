test_that("Test the Reading of Biotek Files", {
  files <- tibble::tibble(
    file = c("20220929_1steptimer20.csv",
             "20221004_1steptimer20.csv",
             "20221012_1steptimer20.csv"),
    meta = c(
      "20220929_1steptimer20_metainfo.csv",
      "20221004_1steptimer20_metainfo.csv",
      "20221012_1steptimer20_metainfo.csv"
    ),
    first_lum = c(3, 1, 7),
    first_od = c(0.09, 0.109, 0.144),
    first_time = c(357.5, 357.5, 357.5)
  )

  purrr::map(seq_along(files$file), function(i) {
    dat <- plate_read_biotek(
      system.file("extdata", files$file[i], package = "wellr")
      )
    dat <- plate_add_meta(data = dat,
                          file = system.file("extdata", files$meta[i], package = "wellr"))

    expect_equal(files$first_lum[i],
                 dat$lum[1])
    expect_equal(files$first_od[i],
                 dat$od600[1])
    expect_equal(files$first_time[i],
                 dat$time[1])


  })
})

test_that("Detection of signal rows.", {

  signals <- c("LUM_1:Lum", "OD600_1:600", "OD600_2:600", "LUM_2:Lum", "LUM_2:")

  non_signals <-
    c(
      "Software Version",
      "Experiment File Path:",
      "Protocol File Path:",
      "Plate Number",
      "Date",
      "Time",
      "Reader Type:",
      "Reader Serial Number:",
      "Reading Type",
      "Procedure Details",
      "Plate Type",
      "Eject plate on completion",
      "Set Temperature",
      "Start Kinetic",
      "Shake",
      "Read",
      "Read",
      "End Kinetic",
      "Plate Out/In",
      "Start Kinetic",
      "Shake",
      "Read",
      "Read",
      "End Kinetic",
      "Plate Out/In"
    )

  sapply(signals, function(x) expect_true(.is_signal(x)))
  sapply(non_signals, function(x) expect_false(.is_signal(x)))
})

