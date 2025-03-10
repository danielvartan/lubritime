test_that("assign_date() | General test", {
  assign_date(
    hms::parse_hm("02:10"),
    hms::parse_hm("05:30")
  ) |>
    expect_equal(
      lubridate::interval(
        lubridate::as_datetime("1970-01-01 02:10:00"),
        lubridate::as_datetime("1970-01-01 05:30:00")
      )
    )

  assign_date(
    c(hms::parse_hm("05:40"), hms::parse_hm("21:30")),
    c(hms::parse_hm("18:05"), hms::parse_hm("10:30"))
  ) |>
    expect_equal(
      c(
        lubridate::interval(
          lubridate::as_datetime("1970-01-01 05:40:00"),
          lubridate::as_datetime("1970-01-01 18:05:00")
        ),
        lubridate::interval(
          lubridate::as_datetime("1970-01-01 21:30:00"),
          lubridate::as_datetime("1970-01-02 10:30:00")
        )
      )
    )
})

test_that("assign_date() | `ambiguity` test", {
  expect_equal(
    assign_date(
      lubridate::as_datetime("1985-01-15 12:00:00"),
      lubridate::as_datetime("2020-09-10 12:00:00"),
      ambiguity = 0
    ),
    lubridate::interval(
      lubridate::as_datetime("1970-01-01 12:00:00"),
      lubridate::as_datetime("1970-01-01 12:00:00")
    )
  )

  expect_equal(
    assign_date(
      lubridate::as_datetime("1985-01-15 12:00:00"),
      lubridate::as_datetime("2020-09-10 12:00:00"),
      ambiguity = 24
    ),
    lubridate::interval(
      lubridate::as_datetime("1970-01-01 12:00:00"),
      lubridate::as_datetime("1970-01-02 12:00:00")
    )
  )

  expect_equal(
    assign_date(
      lubridate::as_datetime("1985-01-15 12:00:00"),
      lubridate::as_datetime("2020-09-10 12:00:00"),
      ambiguity = NA
    ),
    lubridate::as.interval(NA)
  )
})

test_that("assign_date() | Error test", {
  assign_date(
    start = 1,
    end = hms::hms(1),
    ambiguity = 0
  ) |>
    expect_error("Assertion on 'start' failed")

  assign_date(
    start = hms::hms(-1),
    end = hms::hms(1),
    ambiguity = 0
  ) |>
    expect_error()

  assign_date(
    start = hms::hms(86401),
    end = hms::hms(1),
    ambiguity = 0
  ) |>
    expect_error()

  assign_date(
    start = hms::hms(1),
    end = 1,
    ambiguity = 0
  ) |>
    expect_error("Assertion on 'end' failed")

  assign_date(
    start = hms::hms(1),
    end = hms::hms(-1),
    ambiguity = 0
  ) |>
    expect_error()

  assign_date(
    start = hms::hms(1),
    end = hms::hms(86401),
    ambiguity = 0
  ) |>
    expect_error()

  assign_date(
    start = hms::hms(1),
    end = c(hms::hms(1), hms::hms(1)),
    ambiguity = 0
  ) |>
    expect_error()

  assign_date(
    start = hms::hms(1),
    end = hms::hms(1),
    ambiguity = "x"
  ) |>
    expect_error("Assertion on 'ambiguity' failed")
})
