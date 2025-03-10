test_that("link_to_timeline() | General test", {
  link_to_timeline(
    hms::parse_hm("18:00"),
    threshold = hms::parse_hms("12:00:00")
  ) |>
    expect_equal(lubridate::ymd_hms("1970-01-01 18:00:00"))

  link_to_timeline(
    hms::parse_hm("18:00"),
    threshold = hms::parse_hms("19:00:00")
  ) |>
    expect_equal(lubridate::ymd_hms("1970-01-01 18:00:00"))

  link_to_timeline(
    lubridate::ymd_hms("2000-05-04 06:00:00"),
    threshold = hms::parse_hms("12:00:00")
  ) |>
    expect_equal(lubridate::ymd_hms("1970-01-01 06:00:00"))

  link_to_timeline(
    c(
      lubridate::ymd_hms("2020-01-01 18:00:00"),
      lubridate::ymd_hms("2020-01-01 06:00:00")
    ),
    threshold = hms::parse_hms("12:00:00")
  ) |>
    expect_equal(c(
      lubridate::ymd_hms("1970-01-01 18:00:00"),
      lubridate::ymd_hms("1970-01-02 06:00:00")
    ))

  link_to_timeline(
    c(
      lubridate::ymd_hms("2020-01-01 18:00:00"),
      lubridate::ymd_hms("2020-01-01 06:00:00")
    ),
    threshold = NULL
  ) |>
    expect_equal(c(
      lubridate::ymd_hms("1970-01-01 18:00:00"),
      lubridate::ymd_hms("1970-01-01 06:00:00")
    ))
})

test_that("link_to_timeline() | Error test", {
  # checkmate::assert_multi_class(x, c("hms", "POSIXt"))
  link_to_timeline(
    x = 1,
    threshold = hms::parse_hms("12:00:00")
  ) |>
    expect_error("Assertion on 'x' failed")

  # prettycheck::assert_hms( ...
  link_to_timeline(
    x = hms::parse_hms("12:00:00"),
    threshold = 1
  ) |>
    expect_error()
})
