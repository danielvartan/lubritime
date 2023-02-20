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
