test_that("flat_posixt_date() | General test", {
  flat_posixt_date(
    x = lubridate::dmy_hms("17/04/1995 12:00:00"),
    base = as.Date("1970-01-01"),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_equal(lubridate::ymd_hms("1970-01-01 12:00:00"))

  flat_posixt_date(
    x = lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST"),
    base = as.Date("1970-01-01"),
    force_tz = FALSE,
    tz = "UTC"
  ) |>
    expect_equal(lubridate::ymd_hms("1970-01-01 12:00:00", tz = "EST"))

  flat_posixt_date(
    x = lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST"),
    base = as.Date("2000-01-01"),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_equal(lubridate::ymd_hms("2000-01-01 12:00:00"))
})

test_that("flat_posixt_date() | Error test", {
  # prettycheck::assert_posixt(x, null_ok = FALSE)
  flat_posixt_date(
    x = 1,
    base = as.Date("1970-01-01"),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_error()

  # checkmate::assert_date(base, len = 1, all.missing = FALSE)
  flat_posixt_date(
    x = lubridate::as_datetime(1),
    base = "",
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_error()

  flat_posixt_date(
    x = lubridate::as_datetime(1),
    base = c(as.Date("1970-01-01"), as.Date("1970-01-01")),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_error()

  flat_posixt_date(
    x = lubridate::as_datetime(1),
    base = as.Date(NA),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_error()

  # checkmate::assert_flag(force_tz)
  flat_posixt_date(
    x = lubridate::as_datetime(1),
    base = as.Date("1970-01-01"),
    force_tz = 1,
    tz = "UTC"
  ) |>
    expect_error()

  # checkmate::assert_choice(tz, OlsonNames())
  flat_posixt_date(
    x = lubridate::as_datetime(1),
    base = as.Date("1970-01-01"),
    force_tz = TRUE,
    tz = ""
  ) |>
    expect_error()
})

test_that("flat_posixt_hour() | General test", {
  flat_posixt_hour(
    x = as.POSIXct("2020-01-01 05:55:55", tz = "UTC"),
    base = hms::parse_hms("00:01:00"),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_equal(lubridate::as_datetime("2020-01-01 00:01:00", tz = "UTC"))
})

test_that("flat_posixt_hour() | Error test", {
  # prettycheck::assert_posixt(x, null_ok = FALSE)
  flat_posixt_hour(
    x = "",
    base = hms::parse_hms("00:00:00"),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_error()

  # assert_hms(base, any_missing = FALSE)
  flat_posixt_hour(
    x = Sys.time(),
    base = "",
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_error()

  flat_posixt_hour(
    x = Sys.time(),
    base = c(hms::hms(0), NA),
    force_tz = TRUE,
    tz = "UTC"
  ) |>
    expect_error()
})
