test_that("Test the Reading of Biotek Files", {
    file = "20220929_1steptimer20.csv"
    meta = "20220929_1steptimer20_metainfo.csv"

    dat <- plate_read_biotek(system.file("extdata", file, package = "wellr"))
    dat <- plate_add_meta(
      data = dat,
      file = system.file("extdata", meta, package = "wellr")
    )

    expect_snapshot(head(dat))
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
