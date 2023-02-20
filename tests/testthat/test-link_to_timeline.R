test_that("midday_change() | general test", {
    expect_equal(midday_change(
        time = hms::parse_hm("18:00")
    ),
    lubridate::ymd_hms("1970-01-01 18:00:00")
    )

    expect_equal(midday_change(
        time = lubridate::ymd_hms("2000-05-04 06:00:00")
    ),
    lubridate::ymd_hms("1970-01-02 06:00:00")
    )

    expect_equal(midday_change(
        time = c(lubridate::ymd_hms("2020-01-01 18:00:00"),
                 lubridate::ymd_hms("2020-01-01 06:00:00")
        )
    ),
    c(lubridate::ymd_hms("1970-01-01 18:00:00"),
      lubridate::ymd_hms("1970-01-02 06:00:00")
    ))
})

test_that("midday_change() | error test", {
    # checkmate::assert_multi_class(time, c("hms", "POSIXct", "POSIXlt"))
    expect_error(midday_change(time = 1), "Assertion on 'time' failed")
})
