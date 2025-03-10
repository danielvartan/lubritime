test_that("extract_seconds() | General test", {
  extract_seconds(lubridate::dhours(1)) |> expect_equal(3600)

  extract_seconds(as.difftime(3600, units = "secs")) |>
    expect_equal(3600)

  extract_seconds(x = hms::hms(3600)) |> expect_equal(3600)

  extract_seconds(as.POSIXct("2020-01-01 01:00:00", tz = "UTC")) |>
    expect_equal(3600)

  extract_seconds(as.POSIXlt("2020-01-01 01:00:00", tz = "UTC")) |>
    expect_equal(3600)

  extract_seconds(
    lubridate::as.interval(lubridate::dhours(1), lubridate::origin)
  ) |>
    expect_equal(3600)
})
