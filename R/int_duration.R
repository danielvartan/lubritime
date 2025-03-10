#' Compute the duration of an `Interval` object
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `int_duration()` computes the duration of an
#' [`Interval`][lubridate::interval] object
#'
#' @return A [`Duration`][lubridate::duration()] vector.
#'
#' @inheritParams int_mean
#' @family Interval functions
#' @export
#'
#' @examples
#' lubridate::interval(
#'   lubridate::ymd_hms("2023-01-01 22:00:01", tz = "UTC"),
#'   lubridate::ymd_hms("2023-01-02 02:00:01", tz = "UTC"),
#'   tzone = "UTC"
#' ) |>
#'   int_duration()
#' #> [1] "14400s (~4 hours)" # Expected
#'
#' lubridate::interval(
#'   lubridate::ymd_hms("2023-01-01 22:00:01", tz = "UTC"),
#'   lubridate::ymd_hms("2023-01-02 02:00:01", tz = "UTC"),
#'   tzone = "UTC"
#' ) |>
#'   int_duration() |>
#'   lubridate::as.period()
#' #> [1] "4H 0M 0S" # Expected
#'
#' c(
#'   lubridate::interval(
#'     lubridate::ymd_hms("2023-01-01 22:00:01", tz = "UTC"),
#'     lubridate::ymd_hms("2023-01-02 02:00:01", tz = "UTC"),
#'     tzone = "UTC"
#'   ),
#'   lubridate::interval(
#'     lubridate::ymd_hms("1990-01-01 06:00:00", tz = "UTC"),
#'     lubridate::ymd_hms("1990-01-01 12:00:00", tz = "UTC"),
#'     tzone = "UTC"
#'   )
#' ) |>
#'   int_duration()
#' #> [1] "14400s (~4 hours)" "21600s (~6 hours)" # Expected
int_duration <- function(int = NULL) {
  prettycheck::assert_interval(int)

  int |> lubridate::as.duration()
}
