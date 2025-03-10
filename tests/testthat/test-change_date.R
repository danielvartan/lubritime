test_that("change_date() | general test", {
  expect_equal(change_date(as.Date("1970-01-01"), "2000-01-01"),
               as.Date("2000-01-01"))
  expect_equal(change_date(lubridate::as_datetime(0), as.Date("1990-01-01")),
               lubridate::ymd_hms("1990-01-01 00:00:00"))
})

test_that("change_date() | error test", {
  expect_error(change_date(1, ""), "Assertion on 'x' failed")
  expect_error(change_date(as.Date("1970-01-01"), 1),
               "Assertion on 'date' failed")
})

test_that("change_day() | general test", {
  expect_equal(change_day(as.Date("1970-01-01"), 10), as.Date("1970-01-10"))
  expect_equal(change_day(lubridate::as_datetime(0), 25),
               lubridate::ymd_hms("1970-01-25 00:00:00"))
})

test_that("change_day() | general test", {
  # checkmate::assert_multi_class(x, c("Date", "POSIXct", "POSIXlt"))
  1 |>
    change_day(1) |>
    expect_error("Assertion on 'x' failed")

  # prettycheck::assert_numeric(day, lower = 1, upper = 31)
  as.Date("1970-01-01") |>
    change_day("") |>
    expect_error()

  # prettycheck::assert_identical(x, day, type = "length")
  as.Date("1970-01-01") |>
    change_day(1:2) |>
    expect_error()

  # if (any(lubridate::month(x) %in% c(4, 6, 9, 11), na.rm = TRUE) ...
  as.Date("1970-04-01") |>
    change_day(31) |>
    expect_error()

  #  if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) ...
  as.Date("1970-02-01") |>
    change_day(31) |>
    expect_error()

  # if (any(lubridate::month(x) == 2 & lubridate::leap_year(x), ...
  as.Date("1972-02-01") |>
    change_day(31) |>
    expect_error()
})
