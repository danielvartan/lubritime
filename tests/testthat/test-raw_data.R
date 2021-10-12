test_that("raw_data() | general test", {
    checkmate::expect_character(raw_data())
})

test_that("raw_data() | error test", {
    expect_error(raw_data(file = 1), "Assertion on 'file' failed")
    expect_error(raw_data(raw_data()[1]))
})
