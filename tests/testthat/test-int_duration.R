test_that("int_duration() | General test", {
  lubridate::interval(
    lubridate::ymd_hms("2023-01-01 22:00:00", tz = "UTC"),
    lubridate::ymd_hms("2023-01-02 02:00:00", tz = "UTC"),
    tzone = "UTC"
  ) |>
    int_duration() |>
    expect_equal(lubridate::dhours(4))

  c(
    lubridate::interval(
      lubridate::ymd_hms("2023-01-01 22:00:00", tz = "UTC"),
      lubridate::ymd_hms("2023-01-02 02:00:00", tz = "UTC"),
      tzone = "UTC"
    ),
    lubridate::interval(
      lubridate::ymd_hms("1990-01-01 06:00:00", tz = "UTC"),
      lubridate::ymd_hms("1990-01-01 12:00:00", tz = "UTC"),
      tzone = "UTC"
    )
  ) |>
    int_duration() |>
    expect_equal(c(lubridate::dhours(4), lubridate::dhours(6)))
})

test_that("int_duration() | Error test", {
  # prettycheck::assert_interval(int)
  int_duration(int = 1) |> expect_error()
})
