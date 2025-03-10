test_that("fix_hms() | general test", {
  hms::parse_hm("24:00") |>
    fix_hms() |>
    expect_equal(hms::parse_hm("00:00"))

  hms::parse_hm(c("02:00", "24:00", "18:00")) |>
    fix_hms() |>
    expect_equal(hms::parse_hm(c("02:00", "00:00", "18:00")))
})

test_that("fix_hms() | error test", {
  # prettycheck::assert_hms(x)
  1 |> fix_hms() |> expect_error()
})
