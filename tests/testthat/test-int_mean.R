# `prettycheck` is interfering with the tests.

# test_that("int_mean() | general test", {
#   expect_equal(int_mean(
#     start = hms::parse_hm("22:00"), end = hms::parse_hm("06:00"),
#     ambiguity = 24
#   ),
#   hms::hms(26 * 3600)
#   )
#
#   expect_equal(int_mean(
#     start = hms::parse_hm("00:00"), end = hms::parse_hm("10:00"),
#     ambiguity = 24
#   ),
#   hms::parse_hm("05:00")
#   )
# })
#
# test_that("int_mean() | error test", {
#   # prettycheck:::assert_multi_class(start, classes)
#   expect_error(int_mean(
#     start = 1, end = hms::hms(1), ambiguity = 24
#   ),
#   "Assertion on 'start' failed"
#   )
#
#   # prettycheck:::assert_multi_class(end, classes)
#   expect_error(int_mean(
#     start = hms::hms(1), end = 1, ambiguity = 24
#   ), "Assertion on 'end' failed"
#   )
#
#   # prettycheck:::assert_choice(ambiguity, c(0, 24 , NA))
#   expect_error(int_mean(
#     start = hms::hms(1), end = hms::hms(1), ambiguity = 1
#   ),
#   "Assertion on 'ambiguity' failed"
#   )
# })
