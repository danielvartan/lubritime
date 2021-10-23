# Sort tests by type or use the alphabetical order.

test_that("*_numeric_() | general test", {
    expect_true(test_numeric_(as.integer(1)))
    expect_true(test_numeric_(as.double(1)))
    expect_true(test_numeric_(as.numeric(1)))
    expect_true(test_numeric_(c(1, NA), any.missing = TRUE))
    expect_true(test_numeric_(NULL, null.ok = TRUE))
    expect_false(test_numeric_(lubridate::dhours()))
    expect_false(test_numeric_(letters))
    expect_false(test_numeric_(datasets::iris))
    expect_false(test_numeric_(c(1, NA), any.missing = FALSE))
    expect_false(test_numeric_(NULL, null.ok = FALSE))
    expect_false(test_numeric_(1, lower = 2))
    expect_false(test_numeric_(1, upper = 0))

    checkmate::expect_string(check_numeric_(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_numeric_(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    checkmate::expect_string(check_numeric_(1, lower = 2),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_numeric_(1, upper = 0),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_numeric_(c("a", "b")),
                             "Must be of type 'numeric', not 'character'")
    expect_true(check_numeric_(c(1, 1)))
    expect_true(check_numeric_(NULL, null.ok = TRUE))

    expect_equal(assert_numeric_(c(1, 1)), c(1, 1))
    expect_error(assert_numeric_(c("a", "b")),
                 "Assertion on 'c\\(\"a\", \"b\"\\)' failed")
})

test_that("*_duration() | general test", {
    expect_true(test_duration(lubridate::dhours(1)))
    expect_true(test_duration(c(lubridate::dhours(1), NA), any.missing = TRUE))
    expect_true(test_duration(NULL, null.ok = TRUE))
    expect_false(test_duration("a"))
    expect_false(test_duration(1))
    expect_false(test_duration(lubridate::hours()))
    expect_false(test_duration(hms::hms(1)))
    expect_false(test_duration(datasets::iris))
    expect_false(test_duration(c(lubridate::dhours(1), NA),
                               any.missing = FALSE))
    expect_false(test_duration(NULL, null.ok = FALSE))
    expect_false(test_duration(lubridate::dhours(1),
                               lower = lubridate::dhours(2)))
    expect_false(test_duration(lubridate::dhours(1),
                               upper = lubridate::dhours(0)))

    checkmate::expect_string(check_duration(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_duration(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    checkmate::expect_string(check_duration(lubridate::dhours(1),
                                            lower = lubridate::dhours(2)),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_duration(lubridate::dhours(1),
                                            upper = lubridate::dhours(0)),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_duration(c(1, 1)),
                             "Must be of type 'Duration', not 'numeric'")
    expect_true(check_duration(c(lubridate::dhours(1),
                                 lubridate::dhours(1))))
    expect_true(check_duration(NULL, null.ok = TRUE))

    expect_equal(assert_duration(c(lubridate::dhours(1),
                                   lubridate::dhours(1))),
                 c(lubridate::dhours(1), lubridate::dhours(1)))
    expect_error(assert_duration(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_hms() | general test", {
    expect_true(test_hms(hms::hms(1)))
    expect_true(test_hms(c(hms::hms(1), NA), any.missing = TRUE))
    expect_true(test_hms(NULL, null.ok = TRUE))
    expect_false(test_hms("a"))
    expect_false(test_hms(1))
    expect_false(test_hms(lubridate::hours()))
    expect_false(test_hms(lubridate::dhours()))
    expect_false(test_hms(datasets::iris))
    expect_false(test_hms(c(lubridate::dhours(1), NA),
                          any.missing = FALSE))
    expect_false(test_hms(NULL, null.ok = FALSE))
    expect_false(test_hms(hms::hms(1),
                          lower = hms::hms(2)))
    expect_false(test_hms(hms::hms(1),
                          upper = hms::hms(0)))

    checkmate::expect_string(check_hms(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_hms(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    checkmate::expect_string(check_hms(hms::hms(1),
                                       lower = hms::hms(2)),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_hms(hms::hms(1),
                                       upper = hms::hms(0)),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_hms(c(1, 1)),
                             "Must be of type 'hms', not 'numeric'")
    expect_true(check_hms(c(hms::hms(1), hms::hms(1))))
    expect_true(check_hms(NULL, null.ok = TRUE))

    expect_equal(assert_hms(c(hms::hms(1), hms::hms(1))),
                 c(hms::hms(1), hms::hms(1)))
    expect_error(assert_hms(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_posixt() | general test", {
    expect_true(test_posixt(lubridate::as_datetime(1)))
    expect_true(test_posixt(as.POSIXlt(lubridate::as_datetime(1))))
    expect_true(test_posixt(c(lubridate::as_datetime(1), NA),
                            any.missing = TRUE))
    expect_true(test_posixt(NULL, null.ok = TRUE))
    expect_false(test_posixt("a"))
    expect_false(test_posixt(1))
    expect_false(test_posixt(lubridate::hours()))
    expect_false(test_posixt(hms::hms(1)))
    expect_false(test_posixt(datasets::iris))
    expect_false(test_posixt(c(lubridate::as_datetime(1), NA),
                               any.missing = FALSE))
    expect_false(test_posixt(NULL, null.ok = FALSE))
    expect_false(test_posixt(lubridate::as_datetime(1),
                             lower = lubridate::as_datetime(2)))
    expect_false(test_posixt(lubridate::as_datetime(1),
                             upper = lubridate::as_datetime(0)))

    checkmate::expect_string(check_posixt(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_posixt(NULL, null.ok = FALSE),
                             "'NULL' cannot have 'NULL' values")
    checkmate::expect_string(check_posixt(lubridate::as_datetime(1),
                                          lower = lubridate::as_datetime(2)),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_posixt(lubridate::as_datetime(1),
                                          upper = lubridate::as_datetime(0)),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_posixt(c(1, 1)),
                             "Must be of type 'POSIXct' or 'POSIXlt', ")
    expect_true(check_posixt(c(lubridate::as_datetime(1),
                               lubridate::as_datetime(1))))
    expect_true(check_posixt(NULL, null.ok = TRUE))

    expect_equal(assert_posixt(c(lubridate::as_datetime(1),
                                 lubridate::as_datetime(1))),
                 c(lubridate::as_datetime(1), lubridate::as_datetime(1)))
    expect_error(assert_posixt(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_temporal() | general test", {
    expect_true(test_temporal(lubridate::dhours()))
    expect_true(test_temporal(lubridate::hours()))
    expect_true(test_temporal(as.difftime(1, units = "secs")))
    expect_true(test_temporal(hms::hms(1)))
    expect_true(test_temporal(as.Date("2000-01-01")))
    expect_true(test_temporal(lubridate::as_datetime(1)))
    expect_true(test_temporal(as.POSIXlt(lubridate::as_datetime(1))))
    expect_true(test_temporal(lubridate::as.interval(
        lubridate::dhours(), lubridate::as_datetime(0))))
    expect_true(test_temporal(NULL, null.ok = TRUE))
    expect_false(test_temporal(1))
    expect_false(test_temporal(letters))
    expect_false(test_temporal(datasets::iris))
    expect_false(test_temporal(lubridate::dhours(), rm = "Duration"))
    expect_false(test_temporal(lubridate::hours(), rm = "Period"))
    expect_false(test_temporal(as.difftime(1, units = "secs"), rm = "difftime"))
    expect_false(test_temporal(hms::hms(1), rm = "hms"))
    expect_false(test_temporal(as.Date("2000-01-01"), rm = "Date"))
    expect_false(test_temporal(lubridate::as_datetime(1), rm = "POSIXct"))
    expect_false(test_temporal(as.POSIXlt(lubridate::as_datetime(1)),
                rm = "POSIXlt"))
    expect_false(test_temporal(lubridate::as.interval(
        lubridate::dhours(), lubridate::as_datetime(0)), rm = "Interval"))
    expect_false(test_temporal(c(1, NA), any.missing = FALSE))
    expect_false(test_temporal(NULL, null.ok = FALSE))

    checkmate::expect_string(check_temporal(c(1, 1)),
                             pattern = "Must be a temporal object ")
    checkmate::expect_string(check_temporal(c(1, NA), any.missing = FALSE),
                             pattern = "'c\\(1, NA\\)' cannot have missing ")
    checkmate::expect_string(check_temporal(NULL, null.ok = FALSE),
                             pattern = "'NULL' cannot have 'NULL' values")
    expect_true(check_temporal(c(lubridate::hours(1), lubridate::hours(1))))
    expect_true(check_temporal(NULL, null.ok = TRUE))

    expect_equal(assert_temporal(c(lubridate::hours(1), lubridate::hours(1))),
                 c(lubridate::hours(1), lubridate::hours(1)))
    expect_error(assert_temporal(c(1, 1)), "Assertion on 'c\\(1, 1\\)' failed")
})
