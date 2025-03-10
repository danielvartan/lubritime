test_that("int_overlap() | General test", {
  int_1 <- lubridate::interval(
    start = lubridate::as_datetime("2020-01-01 00:00:00"),
    end = lubridate::as_datetime("2021-01-01 00:00:00"),
  )

  int_2 <- lubridate::interval(
    start = lubridate::as_datetime("2020-05-01 00:00:00"),
    end = lubridate::as_datetime("2021-05-01 00:00:00"),
  )

  int_3 <- lubridate::interval(
    start = lubridate::as_datetime("2021-01-01 00:00:00"),
    end = lubridate::as_datetime("2025-01-01 00:00:00"),
  )

  int_overlap(int_1, int_2) |>
    expect_equal(lubridate::interval(
      start = lubridate::as_datetime("2020-05-01 00:00:00"),
      end = lubridate::as_datetime("2021-01-01 00:00:00"),
    ))

  int_overlap(int_1, c(int_2, int_3)) |>
    expect_equal(c(
      lubridate::interval(
        start = lubridate::as_datetime("2020-05-01 00:00:00"),
        end = lubridate::as_datetime("2021-01-01 00:00:00"),
      ),
      lubridate::interval(
        start = lubridate::as_datetime("2021-01-01 00:00:00"),
        end = lubridate::as_datetime("2021-01-01 00:00:00"),
      )
    ))
})

test_that("int_overlap() | Error test", {
  # prettycheck::assert_interval(int_1)
  int_overlap(int = 1) |> expect_error()

  # prettycheck::assert_interval(int_2)
  int_overlap(int = lubridate::interval()) |> expect_error()
})
