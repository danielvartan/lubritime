#' Compute the duration of an interval
#'
#' This function computes the duration of an interval given its start and end
#' points.
#'
#' @param start A [`POSIXct`][base::as.POSIXct()] object representing the start
#'   of the interval.
#' @param end A [`POSIXct`][base::as.POSIXct()] object representing the end of
#'  the interval.
#'
#' @return A [`Period`][lubridate::period()] object representing the duration of
#'  the interval.
#'
#' @family Interval functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' int_duration(
#'   start = lubridate::dmy_hms("15/01/2023 09:57:35", tz = "UTC"),
#'   end = lubridate::dmy_hms("18/01/2023 23:59:35", tz = "UTC")
#' )
#' #> [1] "3d 14h 2m 0s" # Expected
#'
#' ## Vector example
#'
#' int_duration(
#'   start = lubridate::dmy_hms(
#'     c("15/01/2023 09:57:35", "18/01/2023 09:57:35"),
#'     tz = "UTC"
#'   ),
#'   end = lubridate::dmy_hms(
#'     c("18/01/2023 23:59:35", "20/01/2023 23:59:35"),
#'     tz = "UTC"
#'   )
#' )
#' #> [1] "3d 14h 2m 0s" "2d 14h 2m 0s" # Expected
int_duration <- function(start, end) {
  checkmate::assert_posixct(start)
  checkmate::assert_posixct(end)

  lubridate::interval(start, end, tzone = lubridate::tz(start)) |>
    lubridate::as.period()
}
