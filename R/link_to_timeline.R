#' Link local times to a timeline
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `link_to_timeline()` links local times (e.g., 10:00, 22:00) onto a two-day
#' timeline based on a specified threshold.
#'
#' This function is particularly useful for time arithmetic when dealing with
#' circular time representations. For information about circular time,
#' see [`cycle_time()`][lubritime::cycle_time()] Details section.
#'
#' @details
#'
#' `link_to_timeline()` will link local times onto a two-day timeline based on
#' the `threshold` time and the
#' [Unix epoch](https://en.wikipedia.org/wiki/Unix_time) (`1970-01-01`).
#' For example, if the threshold time is `12:00:00`, all times **before** this
#' threshold will be linked *on the day after the Unix epoch* (`1970-01-02`) and
#' all times **after** this threshold will be linked *to the Unix epoch*
#' (`1970-01-01`).
#'
#' The exception is when all times are before the threshold. In this case, the
#' function will just return all times linked to the Unix epoch (`1970-01-01`).
#'
#' Please note that the Unix Epoch is the origin of time for the
#' [`POSIXct`][base::DateTimeClasses] class. This means that
#' `as.POSIXct("1970-01-01 00:00:00", tz = "UTC") |> as.numeric()`
#' is equal to `0`.
#'
#' @param x A [`hms`][hms::hms()] or [`POSIXt`][base::as.POSIXct()] object.
#'   If `x` is a `POSIXt` object, only the time part will be used.
#' @param threshold (Optional) A [`hms`][hms::hms()] object representing the
#'   threshold time (Default: `hms::parse_hm("12:00")`).
#'
#' @returns A [`POSIXt`][base::as.POSIXct()] object with the same time as `x`,
#'  but linked to a timeline based on `threshold` and the
#'  [Unix epoch](https://en.wikipedia.org/wiki/Unix_time). See the Details
#'  section for more information.
#'
#' @family circular time functions
#' @export
#'
#' @examples
#' hms::parse_hm("18:00") |>
#'   link_to_timeline(threshold = hms::parse_hms("12:00:00"))
#' #> [1] "1970-01-01 18:00:00 UTC" # Expected
#'
#' as.POSIXct("2020-01-01 05:00:00", tz = "UTC") |>
#'   link_to_timeline(threshold = hms::parse_hms("12:00:00"))
#' #> [1] "1970-01-01 05:00:00 UTC" # Expected
#'
#' c(hms::parse_hm("18:00"), hms::parse_hm("06:00")) |>
#'   link_to_timeline(threshold = hms::parse_hms("12:00:00"))
#' #> [1] "1970-01-01 18:00:00 UTC" "1970-01-02 06:00:00 UTC" # Expected
#'
#' c(as.POSIXct("2020-01-01 20:00:00", tz = "UTC"),
#'   as.POSIXct("2020-01-01 05:00:00", tz = "UTC")) |>
#'   link_to_timeline()
#' #> [1] "1970-01-01 20:00:00 UTC" "1970-01-02 05:00:00 UTC" # Expected
#'
#' ## Using a Different Threshold
#'
#' c(as.POSIXct("2020-01-01 20:00:00", tz = "UTC"),
#'   as.POSIXct("2020-01-01 01:00:00", tz = "UTC")) |>
#'   link_to_timeline(threshold = hms::parse_hms("02:00:00"))
#' #> [1] "1970-01-01 20:00:00 UTC" "1970-01-02 01:00:00 UTC" # Expected
#'
#' c(hms::parse_hm("06:00"), hms::parse_hm("18:00")) |>
#'   link_to_timeline(threshold = hms::parse_hms("02:00:00"))
#' #> [1] "1970-01-01 06:00:00 UTC" "1970-01-01 18:00:00 UTC" # Expected
#'
#' ## With All Values Below or Above the Threshold
#' ## (Returns on day one (`1970-01-01`))
#'
#' c(hms::parse_hm("03:00"), hms::parse_hm("06:00")) |>
#'   link_to_timeline(threshold = hms::parse_hms("12:00:00"))
#' #> [1] "1970-01-01 03:00:00 UTC" "1970-01-01 06:00:00 UTC" # Expected
#'
#' c(hms::parse_hm("06:00"), hms::parse_hm("18:00")) |>
#'   link_to_timeline(threshold = hms::parse_hms("02:00:00"))
#' #> [1] "1970-01-01 06:00:00 UTC" "1970-01-01 18:00:00 UTC" # Expected
#'
#' ## Doing Time Arithmetic
#'
#' #                8h        (Day 1)         8h
#' # <----|-----------------------|-----------------------|----->
#' #    06:00                   14:00                   22:00h
#'
#' c(hms::parse_hm("06:00"), hms::parse_hm("22:00")) |>
#'   mean() |>
#'   hms::as_hms()
#' #> 14:00:00 # Expected
#'
#' #   (Day 1)        4h                    4h         (Day 2)
#' # <----|-----------------------|-----------------------|----->
#' #    22:00                   02:00                   06:00h
#'
#' c(hms::parse_hm("22:00"), hms::parse_hm("06:00")) |>
#'   link_to_timeline(threshold = hms::parse_hms("12:00:00")) |>
#'   mean() |>
#'   hms::as_hms()
#' #> 02:00:00 # Expected
link_to_timeline <- function(x, threshold = hms::parse_hm("12:00")) {
  checkmate::assert_multi_class(x, c("hms", "POSIXt"))

  prettycheck::assert_hms(
    threshold,
    lower = hms::hms(0),
    upper = hms::parse_hms("23:59:59"),
    null_ok = TRUE
  )

  x <-
    x |>
    as.POSIXct() |>
    flat_posixt_date()

  if (!is.null(threshold)) {
    if (
      all(
        hms::as_hms(min(x, na.rm = TRUE)) < threshold &
          hms::as_hms(max(x, na.rm = TRUE)) < threshold,
        na.rm = TRUE
      )
    ) {
      x
    } else {
      dplyr::case_when(
        hms::as_hms(x) < threshold ~ lubridate::`day<-`(x, 2),
        TRUE ~ x
      )
    }
  } else {
    x
  }
}
