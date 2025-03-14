#' Find the shorter or longer interval between two hours
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `shorter_int()` returns the shorter interval between two
#' [`hms`][hms::hms()] or [`POSIXt`][base::DateTimeClasses] object hours.
#'
#' `longer_int()` do the inverse of `shorter_int()`, i.e., returns the
#' longer interval between two hours.
#'
#' `shorter_duration()` and `longer_duration()` return the interval time span
#' of `shorter_int()` and `longer_int()` as
#' [`Duration`][lubridate::duration()] objects.
#'
#' @details
#'
#' ## The two intervals problem
#'
#' Given two hours, `x` and `y`, in a two-day timeline, without date references,
#' there will be always two possible intervals between them, as illustrated
#' below.
#'
#' To figure out what interval is the  shorter or the longer,
#' `shorter_int()` and `longer_int()` verify two scenarios: 1. When
#' `x` comes before `y`; and 2. when `x` comes after `y`. This only works if `x`
#' value is smaller than `y`, therefore, the function will make sure to swap `x`
#' and `y` values if the latter assumption is not true.
#'
#' Because `shorter_int()` objective is to find the shorter interval, if
#' `x` and `y` are equal, the shorter interval will have a length of 0 hours,
#' resulting in an interval from `x` to `x`. But, if `longer_int()` is used
#' instead, the latter condition will return a interval with 24 hours of length
#' (from `x` to `x` + 1 day).
#'
#' In cases when `x` and `y` distance themselves by 12 hours, there will be no
#' shorter or longer interval (they will have equal length). In these cases,
#' `shorter_int()` and `longer_int()` will return the same value
#' (an interval of 12 hours).
#'
#' ```
#'              day 1                        day 2
#'      x                  y         x                  y
#'    06:00              22:00     06:00              22:00
#' -----|------------------|---------|------------------|----->
#'               16h           8h             16h
#'           longer int.  shorter int.   longer int.
#'
#'               day 1                      day 2
#'      y                   x       y                   x
#'    13:00               08:00   13:00               08:00
#' -----|-------------------|-------|-------------------|----->
#'               19h           5h            19h
#'           longer int.  shorter int.  longer int.
#'
#'     x,y             x,y             x,y             x,y
#'      x               y               x               y
#'    10:00           10:00           10:00           10:00
#' -----|---------------|---------------|---------------|----->
#'     0h              0h              0h              0h
#'             24h             24h             24h
#'
#'               day 1                      day 2
#'      y               x               y               x
#'    12:00           00:00           12:00           00:00
#' -----|---------------|---------------|---------------|----->
#'             12h             12h             12h
#' ```
#'
#' ## Base date and timezone
#'
#' `shorter_int()` and `longer_int()` use the
#' [Unix epoch](https://en.wikipedia.org/wiki/Unix_time) (1970-01-01) date as
#' the start date for creating intervals.
#'
#' The output will always have `"UTC"` set as timezone. Learn more about
#' time zones in [`?timezone`][base::timezone].
#'
#' ## `POSIXt` objects
#'
#' [`POSIXt`][base::DateTimeClasses] objects passed as argument to `x` or `y`
#' will be stripped of their dates. Only the time will be considered.
#'
#' Both [`POSIXct`][base::as.POSIXct()] and [`POSIXlt`][base::as.POSIXlt()] are
#' objects that inherits the class [`POSIXt`][base::DateTimeClasses]. Learn more
#' about it in [`?DateTimeClasses`][base::DateTimeClasses].
#'
#' ## `NA` values
#'
#' `shorter_int()` or `longer_int()` will return an
#' [`Interval`][lubridate::interval()] `NA`-`NA` if `x` or `y` are `NA`.
#'
#' `shorter_duration()` or `longer_duration()` will return a
#' [`Duration`][lubridate::duration()] `NA`  if `x` or `y` are `NA`.
#'
#' @param x,y An [`hms`][hms::hms()] or [`POSIXt`][base::DateTimeClasses]
#'   object.
#'
#' @return
#' - For `shorter_int()` or `longer_int()`, an
#' [`Interval`][lubridate::interval()] vector with the shorter or longer
#' interval between `x` and `y`.
#' - For `shorter_duration()` or `longer_duration()`, a
#' [`Duration`][lubridate::duration()] vector with the shorter or longer
#' duration between `x` and `y`.
#'
#' @family circular time functions
#' @export
#'
#' @examples
#' x <- hms::parse_hm("23:00")
#' y <- hms::parse_hm("01:00")
#'
#' shorter_int(x, y)
#' #> [1] 1970-01-01 23:00:00 UTC--1970-01-02 01:00:00 UTC # Expected
#'
#' shorter_duration(x, y)
#' #> [1] "7200s (~2 hours)" # Expected
#'
#' longer_int(x, y)
#' #> [1] 1970-01-01 01:00:00 UTC--1970-01-01 23:00:00 UTC # Expected
#'
#' longer_duration(x, y)
#' #> [1] "79200s (~22 hours)" # Expected
#'
#' x <- lubridate::as_datetime("1985-01-15 12:00:00")
#' y <- lubridate::as_datetime("2020-09-10 12:00:00")
#'
#' shorter_int(x, y)
#' #> [1] 1970-01-01 12:00:00 UTC--1970-01-01 12:00:00 UTC # Expected
#'
#' shorter_duration(x, y)
#' #> [1] "0s" # Expected
#'
#' longer_int(x, y)
#' #> [1] 1970-01-01 12:00:00 UTC--1970-01-02 12:00:00 UTC # Expected
#'
#' longer_duration(x, y)
#' #> [1] "86400s (~1 days)" # Expected
#'
#' x <- c(hms::parse_hm("15:30"), hms::parse_hm("21:30"))
#' y <- c(hms::parse_hm("19:30"), hms::parse_hm("04:00"))
#'
#' shorter_int(x, y)
#' #> [1] 1970-01-01 15:30:00 UTC--1970-01-01 19:30:00 UTC # Expected
#' #> [2] 1970-01-01 21:30:00 UTC--1970-01-02 04:00:00 UTC # Expected
#'
#' shorter_duration(x, y)
#' #> [1] [1] "14400s (~4 hours)"   "23400s (~6.5 hours)" # Expected
#'
#' longer_int(x, y)
#' #> [1] 1970-01-01 19:30:00 UTC--1970-01-02 15:30:00 UTC # Expected
#' #> [2] 1970-01-01 04:00:00 UTC--1970-01-01 21:30:00 UTC # Expected
#'
#' longer_duration(x, y)
#' #> [1] "72000s (~20 hours)"   "63000s (~17.5 hours)" # Expected
shorter_int <- function(x, y) {
  int_build(x, y, method = "shorter")
}

#' @rdname shorter_int
#' @export
longer_int <- function(x, y) {
  int_build(x, y, method = "longer")
}

#' @rdname shorter_int
#' @export
shorter_duration <- function(x, y) {
  shorter_int(x, y) |> lubridate::as.duration()
}

#' @rdname shorter_int
#' @export
longer_duration <- function(x, y) {
  longer_int(x, y) |> lubridate::as.duration()
}

int_build <- function(x, y, method = "shorter") {
  method_choices <- c("shorter", "longer")

  checkmate::assert_multi_class(x, c("hms", "POSIXt"))
  prettycheck::assert_numeric(
    as.numeric(hms::as_hms(x)),
    lower = 0,
    upper = 86400
  )
  checkmate::assert_multi_class(y, c("hms", "POSIXt"))
  prettycheck::assert_numeric(
    as.numeric(hms::as_hms(y)),
    lower = 0,
    upper = 86400
  )
  prettycheck::assert_identical(x, y, type = "length")
  checkmate::assert_choice(method, method_choices)

  x <-
    x |>
    hms::as_hms() |>
    as.POSIXct() |>
    flat_posixt_date()

  y <-
    y |>
    hms::as_hms() |>
    as.POSIXct() |>
    flat_posixt_date()

  list2env(rutils::swap_if(x, y, x > y), envir = environment())

  x1_y1_interval <- lubridate::interval(x, y)
  y1_x2_interval <- lubridate::interval(y, x + lubridate::days())

  if (method == "shorter") {
    out <- dplyr::case_when(
      is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
      x == y ~ lubridate::as.interval(lubridate::hours(0), x),
      x1_y1_interval <= y1_x2_interval ~ x1_y1_interval,
      x1_y1_interval > y1_x2_interval ~ y1_x2_interval,
    )
  } else if (method == "longer") {
    out <- dplyr::case_when(
      is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
      x == y ~ lubridate::as.interval(lubridate::hours(24), x),
      x1_y1_interval >= y1_x2_interval ~ x1_y1_interval,
      x1_y1_interval < y1_x2_interval ~ y1_x2_interval,
    )
  }

  if (any(x1_y1_interval == y1_x2_interval, na.rm = TRUE)) {
    flags <- which(x1_y1_interval == y1_x2_interval) # nolint

    cli::cli_alert_warning(paste0(
      "Element{?s} {rutils:::single_quote_(as.character(flags))} of 'x' ",
      "and 'y' have intervals equal to 12 hours, i.e., ",
      "there's no shorter or longer interval ",
      "between the two hours (they are equal). Only one ",
      "possible interval was returned."
    ))
  }

  out
}
