test_that("flat_posixct() | general test", {
  expect_equal(flat_posixt(
    posixt = lubridate::dmy_hms("17/04/1995 12:00:00"),
    base = as.Date("1970-01-01"), force_tz = TRUE,  tz = "UTC"
  ),
  lubridate::ymd_hms("1970-01-01 12:00:00")
  )

  expect_equal(flat_posixt(
    posixt = lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST"),
    base = as.Date("1970-01-01"), force_tz = FALSE,  tz = "UTC"
  ),
  lubridate::ymd_hms("1970-01-01 12:00:00", tz = "EST")
  )

  expect_equal(flat_posixt(
    posixt = lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST"),
    base = as.Date("2000-01-01"), force_tz = TRUE,  tz = "UTC"
  ),
  lubridate::ymd_hms("2000-01-01 12:00:00")
  )
})

test_that("flat_posixct() | error test", {
  # assert_posixt(posixt, null.ok = FALSE)
  expect_error(flat_posixt(
    posixt = 1, base = as.Date("1970-01-01"), force_tz = TRUE, tz = "UTC"
  ),
  "Assertion on 'posixt' failed"
  )

  # prettycheck:::assert_date(base, len = 1, all.missing = FALSE)
  expect_error(flat_posixt(
    posixt = lubridate::as_datetime(1), base = "", force_tz = TRUE,
    tz = "UTC"
  ),
  "Assertion on 'base' failed"
  )

  expect_error(flat_posixt(
    posixt = lubridate::as_datetime(1),
    base = c(as.Date("1970-01-01"), as.Date("1970-01-01")),
    force_tz = TRUE, tz = "UTC"
  ),
  "Assertion on 'base' failed"
  )

  expect_error(flat_posixt(
    posixt = lubridate::as_datetime(1),  base = as.Date(NA),
    force_tz = TRUE, tz = "UTC"
  ),
  "Assertion on 'base' failed"
  )

  # prettycheck:::assert_flag(force_tz)
  expect_error(flat_posixt(
    posixt = lubridate::as_datetime(1), base = as.Date("1970-01-01"),
    force_tz = 1, tz = "UTC"
  ),
  "Assertion on 'force_tz' failed"
  )

  # prettycheck:::assert_choice(tz, OlsonNames())
  expect_error(flat_posixt(
    posixt = lubridate::as_datetime(1), base = as.Date("1970-01-01"),
    force_tz = TRUE, tz = ""
  ),
  "Assertion on 'tz' failed"
  )
})

test_that("flat_posixt_date() | general test", {
  expect_equal(flat_posixt_date(
    posixt = as.POSIXct("2020-01-01 05:55:55", tz = "UTC"),
    base = as.Date("1975-01-01")
  ),
  lubridate::as_datetime("1975-01-01 05:55:55", tz = "UTC")
  )
})

test_that("flat_posixt_date() | error test", {
  # assert_posixt(posixt, null.ok = FALSE)
  expect_error(flat_posixt_date(posixt = "", base = as.Date("1970-01-01")),
               "Assertion on 'posixt' failed")

  # prettycheck:::assert_date(base, len = 1, any.missing = FALSE)
  expect_error(
    flat_posixt_date(posixt = Sys.time(), base = ""),
    "Assertion on 'base' failed"
  )

  expect_error(flat_posixt_date(
    posixt = Sys.time(), base = c(Sys.Date(), NA)
  ),
  "Assertion on 'base' failed"
  )
})

test_that("flat_posixt_hour() | general test", {
  expect_equal(flat_posixt_hour(
    posixt = as.POSIXct("2020-01-01 05:55:55", tz = "UTC"),
    base = hms::parse_hms("00:01:00")
  ),
  lubridate::as_datetime("2020-01-01 00:01:00", tz = "UTC")
  )
})

test_that("flat_posixt_hour() | error test", {
  # assert_posixt(posixt, null.ok = FALSE)
  expect_error(flat_posixt_hour(
    posixt = "", base = hms::parse_hms("00:00:00")
  ),
  "Assertion on 'posixt' failed"
  )

  # assert_hms(base, any.missing = FALSE)
  expect_error(
    flat_posixt_hour(posixt = Sys.time(), base = ""),
    "Assertion on 'base' failed"
  )

  expect_error(flat_posixt_hour(
    posixt = Sys.time(), base = c(hms::hms(0), NA)
  ),
  "Assertion on 'base' failed"
  )
})
