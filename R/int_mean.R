#' Compute the mean of an `Interval` object
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `int_mean()` computes the mean of an [`Interval`][lubridate::interval]
#' object.
#'
#' @param int An [`Interval`][lubridate::interval()] vector.
#'
#' @return  An [`Interval`][lubridate::interval()] vector.
#'
#' @family Interval functions
#' @export
#'
#' @examples
#' lubridate::interval(
#'   lubridate::ymd_hms("2023-01-01 22:00:01", tz = "UTC"),
#'   lubridate::ymd_hms("2023-01-02 02:00:01", tz = "UTC"),
#'   tzone = "UTC"
#' ) |>
#'   int_mean()
#' #> [1] "2023-01-02 00:00:01 UTC" # Expected
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
#'   int_mean()
#' #> [1] "2023-01-02 00:00:01 UTC" "1990-01-01 09:00:00 UTC" # Expected
int_mean <- function(int) {
  prettycheck::assert_interval(int)

  start <- lubridate::int_start(int)
  mean_duration <- (as.numeric(int) / 2) |> lubridate::seconds()

  start + mean_duration
}
