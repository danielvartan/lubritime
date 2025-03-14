test_that("int_mean() | General test", {
  lubridate::interval(
    lubridate::ymd_hms("2023-01-01 22:00:00", tz = "UTC"),
    lubridate::ymd_hms("2023-01-02 02:00:00", tz = "UTC"),
    tzone = "UTC"
  ) |>
    int_mean() |>
    expect_equal(lubridate::ymd_hms("2023-01-02 00:00:00", tz = "UTC"))

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
    int_mean() |>
    expect_equal(
      c(
        lubridate::ymd_hms("2023-01-02 00:00:00", tz = "UTC"),
        lubridate::ymd_hms("1990-01-01 09:00:00", tz = "UTC")
      )
    )
})

test_that("int_mean() | Error test", {
  # prettycheck::assert_interval(int)
  int_mean(int = 1) |> expect_error()
})
