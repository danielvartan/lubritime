#' Compute the overlapping interval between two `Interval` objects
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `int_overlap()` computes the overlapping interval between two
#' [`Interval`][lubridate::interval] objects.
#'
#' See the *Details* section for more information.
#'
#' @details
#'
#' The overlapping interval is the time span where both intervals intersect.
#' If the intervals do not overlap, the function returns an `NA-NA` `Interval`.
#' The illustration below demonstrates the concept:
#'
#' ```
#'           +______________________+_________+
#'           |                      |         |
#'           |           Interval 1 |         |
#'           +______________________+_________+
#'           |//////////////////////|
#'           |//// Overlap Area ////|
#'   +_______+//////////////////////+
#'   |       |                      |
#'   |       |  Interval 2          |
#'   +_______+______________________+
#' ```
#'
#' @param int_1,int_2 [`Interval`][lubridate::interval] vectors. If
#'   they have a length of `1``, they will be recycled to the length of the
#'   longest.
#'
#' @return An [`Interval`][lubridate::interval] vector with the overlapping
#'   period between `int_1` and `int_2`.
#'
#' @family Interval functions
#' @export
#'
#' @examples
#' int_1 <- lubridate::interval(
#'   start = lubridate::as_datetime("2020-01-01 00:00:00"),
#'   end = lubridate::as_datetime("2021-01-01 00:00:00"),
#' )
#'
#' int_2 <- lubridate::interval(
#'   start = lubridate::as_datetime("2020-05-01 00:00:00"),
#'   end = lubridate::as_datetime("2021-05-01 00:00:00"),
#' )
#'
#' int_3 <- lubridate::interval(
#'   start = lubridate::as_datetime("2021-01-01 00:00:00"),
#'   end = lubridate::as_datetime("2025-01-01 00:00:00"),
#' )
#'
#' int_overlap(int_1, int_2)
#' #> [1] 2020-05-01 UTC--2021-01-01 UTC # Expected
#'
#' int_overlap(int_1, c(int_2, int_3))
#' #> [1] 2020-05-01 UTC--2021-01-01 UTC # Expected
#' #> [2] 2021-01-01 UTC--2021-01-01 UTC
int_overlap <- function(int_1, int_2) {
  prettycheck::assert_interval(int_1)
  prettycheck::assert_interval(int_2)

  if (length(int_1) == 1) int_1 <- rep(int_1, length(int_2))
  if (length(int_2) == 1) int_2 <- rep(int_2, length(int_1))

  prettycheck::assert_identical(int_1, int_2, type = "length")

  # The timezone is inherited from the `start` interval
  dplyr::case_when(
    lubridate::int_overlaps(int_1, int_2) ~ lubridate::interval(
      start = dplyr::if_else(
        lubridate::int_start(int_1) >= lubridate::int_start(int_2),
        lubridate::int_start(int_1), lubridate::int_start(int_2)
      ),
      end = dplyr::if_else(
        lubridate::int_end(int_1) <= lubridate::int_end(int_2),
        lubridate::int_end(int_1), lubridate::int_end(int_2)
      )
    ),
    TRUE ~ lubridate::as.interval(NA)
  )
}
