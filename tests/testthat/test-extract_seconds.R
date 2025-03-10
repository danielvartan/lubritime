test_that("extract_seconds() | General test", {
  expect_equal(extract_seconds(x = lubridate::dhours(1)), 3600)
  expect_equal(extract_seconds(x = as.difftime(3600, units = "secs")), 3600)
  expect_equal(extract_seconds(x = hms::hms(3600)), 3600)

  expect_equal(extract_seconds(
    x = as.POSIXct("2020-01-01 01:00:00", tz = "UTC")
  ),
  3600
  )

  expect_equal(extract_seconds(
    x = as.POSIXlt("2020-01-01 01:00:00", tz = "UTC")
  ),
  3600
  )

  expect_equal(extract_seconds(
    x = lubridate::as.interval(lubridate::dhours(1), lubridate::origin)
  ),
  3600
  )
})

test_that("extract_seconds() | Error test", {
  # checkmate::assert_multi_class(x, classes)
  expect_error(extract_seconds(x = 1), "Assertion on 'x' failed")
})
