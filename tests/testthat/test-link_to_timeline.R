test_that("link_to_timeline() | general test", {
    link_to_timeline(
        x = hms::parse_hm("18:00"), threshold = hms::parse_hms("12:00:00")
    ) %>%
        expect_equal(lubridate::ymd_hms("1970-01-01 18:00:00"))

    link_to_timeline(
        x = hms::parse_hm("18:00"), threshold = hms::parse_hms("19:00:00")
    ) %>%
        expect_equal(lubridate::ymd_hms("1970-01-02 18:00:00"))

    link_to_timeline(
        x = lubridate::ymd_hms("2000-05-04 06:00:00"),
        threshold = hms::parse_hms("12:00:00")
    ) %>%
        expect_equal(lubridate::ymd_hms("1970-01-02 06:00:00"))

    link_to_timeline(
        x = c(
            lubridate::ymd_hms("2020-01-01 18:00:00"),
            lubridate::ymd_hms("2020-01-01 06:00:00")
            ),
        threshold = hms::parse_hms("12:00:00")
    ) %>%
        expect_equal(c(
            lubridate::ymd_hms("1970-01-01 18:00:00"),
            lubridate::ymd_hms("1970-01-02 06:00:00")
            ))
})

test_that("link_to_timeline() | error test", {
    # checkmate::assert_multi_class(x, c("hms", "POSIXt"))
    link_to_timeline(
        x = 1, threshold = hms::parse_hms("12:00:00")
        ) %>%
        expect_error("Assertion on 'x' failed")

    # gutils:::assert_hms(
    #     threshold, lower = hms::hms(0), upper = hms::parse_hms("23:59:59")
    # )
    link_to_timeline(
        x = hms::parse_hms("12:00:00"), threshold = 1
    ) %>%
        expect_error("Assertion on 'threshold' failed")
})
