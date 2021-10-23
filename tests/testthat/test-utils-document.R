test_that("flat_posixct() | general test", {
    expect_equal(flat_posixt(lubridate::dmy_hms("17/04/1995 12:00:00"),
                             base = as.Date("1970-01-01"),
                             force_tz = TRUE),
                 lubridate::ymd_hms("1970-01-01 12:00:00"))
    expect_equal(flat_posixt(lubridate::dmy_hms("17/04/1995 12:00:00",
                                                tz = "EST"),
                             base = as.Date("1970-01-01"),
                             force_tz = FALSE),
                 lubridate::ymd_hms("1970-01-01 12:00:00", tz = "EST"))
    expect_equal(flat_posixt(lubridate::dmy_hms("17/04/1995 12:00:00",
                                                tz = "EST"),
                             base = as.Date("2000-01-01"),
                             force_tz = TRUE),
                 lubridate::ymd_hms("2000-01-01 12:00:00"))
})

test_that("flat_posixct() | error test", {
    expect_error(flat_posixt(1), "Assertion on 'posixt' failed")
    expect_error(flat_posixt(lubridate::as_datetime(1), base = ""),
                 "Assertion on 'base' failed")
    expect_error(flat_posixt(lubridate::as_datetime(1), force_tz = 1),
                 "Assertion on 'force_tz' failed")
})

test_that("midday_change() | general test", {
    expect_equal(midday_change(hms::parse_hm("18:00")),
                 lubridate::ymd_hms("1970-01-01 18:00:00"))
    expect_equal(midday_change(lubridate::ymd_hms("2000-05-04 06:00:00")),
                 lubridate::ymd_hms("1970-01-02 06:00:00"))
    expect_equal(midday_change(c(lubridate::ymd_hms("2020-01-01 18:00:00"),
                                 lubridate::ymd_hms("2020-01-01 06:00:00"))),
                 c(lubridate::ymd_hms("1970-01-01 18:00:00"),
                   lubridate::ymd_hms("1970-01-02 06:00:00")))
})

test_that("midday_change() | error test", {
    expect_error(midday_change(1), "Assertion on 'time' failed")
})

test_that("interval_mean() | general test", {
    expect_equal(interval_mean(hms::parse_hm("22:00"), hms::parse_hm("06:00")),
                 hms::hms(26 * 3600))
    expect_equal(interval_mean(hms::parse_hm("00:00"), hms::parse_hm("10:00")),
                 hms::parse_hm("05:00"))
})

test_that("interval_mean() | error test", {
    expect_error(interval_mean(1, hms::hms(1)), "Assertion on 'start' failed")
    expect_error(interval_mean(hms::hms(1), 1), "Assertion on 'end' failed")
    expect_error(interval_mean(hms::hms(1), hms::hms(1), ambiguity = 1),
                 "Assertion on 'ambiguity' failed")
})

test_that("extract_seconds() | general test", {
    expect_equal(extract_seconds(lubridate::dhours(1)), 3600)
    expect_equal(extract_seconds(as.difftime(3600, units = "secs")), 3600)
    expect_equal(extract_seconds(hms::hms(3600)), 3600)
    expect_equal(extract_seconds(
        as.POSIXct("2020-01-01 01:00:00", tz = "UTC")),
        3600)
    expect_equal(extract_seconds(
        as.POSIXlt("2020-01-01 01:00:00", tz = "UTC")),
        3600)
    expect_equal(extract_seconds(
        lubridate::as.interval(lubridate::dhours(1), lubridate::origin)),
        3600)
})

test_that("extract_seconds() | error test", {
    expect_error(extract_seconds(1), "Assertion on 'x' failed")
})

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
    expect_error(change_day(1, 1), "Assertion on 'x' failed")
    expect_error(change_day(as.Date("1970-01-01"), ""),
                 "Assertion on 'day' failed")

    expect_error(change_day(as.Date("1970-04-01"), 31),
                 "You can't assign more than 30 days to April, June, ")
    expect_error(change_day(as.Date("1970-02-01"), 31),
                 "You can't assign more than 28 days to February in non-leap ")
    expect_error(change_day(as.Date("1972-02-01"), 31),
                 "You can't assign more than 29 days to February in a leap ")
})
