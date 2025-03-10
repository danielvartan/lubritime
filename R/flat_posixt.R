#' Adjust the date or time of a `POSIXt` vector to a specific date or hour
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `flat_posixt_*` functions adjust the date or time of a
#' [`POSIXt`][base::DateTimeClasses] vector to a specific date or hour, with
#' `UTC` as the default timezone.
#'
#' These functions are particularly useful for performing time arithmetic on
#' time objects that lack a date reference (e.g., [`hms`][hms::hms] objects).
#'
#' @param x A [`posixt`][base::DateTimeClasses] vector.
#' @param base A [`Date`][base::Date] (for `flat_posixt_date()` or
#'  [`hms`][hms::hms] (for `flat_posixt_hour()`) value (Default:
#' `as.Date("1970-01-01")`
#' ([Unix Epoch](https://en.wikipedia.org/wiki/Unix_time))
#'  or `hms::parse_hm("00:00")`).
#' @param force_tz A [`logical`][base::logical] flag indicating whether to
#'   force the timezone of the output to `tz` (Default: `TRUE`).
#' @param tz A [`character`][base::character] string indicating the timezone
#'   to use when `force_tz` is `TRUE` (Default: `"UTC"`).
#'
#' @return A [`posixt`][base::DateTimeClasses] vector.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' as.POSIXct("2020-01-01 05:55:55", tz = "America/Sao_Paulo") |>
#'   flat_posixt_date()
#' #> [1] "1970-01-01 05:55:55 UTC" # Expected
#'
#' c(
#'   as.POSIXct("2020-01-01 05:55:55", tz = "America/Sao_Paulo"),
#'   as.POSIXct("2020-01-01 18:40:05", tz = "America/Sao_Paulo")
#' ) |>
#'  flat_posixt_date()
#' #> [1] "1970-01-01 05:55:55 UTC" "1970-01-01 18:40:05 UTC" # Expected
#'
#' as.POSIXct("2020-01-01 05:55:55", tz = "America/Sao_Paulo") |>
#'  flat_posixt_hour(base = hms::parse_hm("00:01"))
#' #> [1] "2020-01-01 00:01:00 UTC" # Expected
#'
#' c(
#'  as.POSIXct("2020-01-01 05:55:55", tz = "America/Sao_Paulo"),
#'  as.POSIXct("2020-01-01 18:40:05", tz = "America/Sao_Paulo")
#' ) |>
#'  flat_posixt_hour(base = hms::parse_hm("00:01"))
#' #> [1] "2020-01-01 00:01:00 UTC" "2020-01-01 00:01:00 UTC" # Expected
flat_posixt_date <- function(
    x, #nolint
    base = as.Date("1970-01-01"),
    force_tz = TRUE,
    tz = "UTC"
  ) {
  prettycheck::assert_posixt(x, null_ok = FALSE)
  checkmate::assert_date(base, len = 1, all.missing = FALSE)
  checkmate::assert_flag(force_tz)
  checkmate::assert_choice(tz, OlsonNames())

  out <-
    x |>
    lubridate::`date<-`(base) |>
    c()

  if (isTRUE(force_tz)) {
    out |> lubridate::force_tz(tz)
  } else {
    out
  }
}

#' @rdname flat_posixt_date
#' @export
flat_posixt_hour <- function(
    x, #nolint
    base = hms::parse_hm("00:00"),
    force_tz = TRUE,
    tz = "UTC"
  ) {
  prettycheck::assert_posixt(x)
  prettycheck::assert_hms(base, any_missing = FALSE)
  prettycheck::assert_length(base, len = 1)
  checkmate::assert_flag(force_tz)
  checkmate::assert_choice(tz, OlsonNames())

  out <-
    x |>
    lubridate::date() |>
    paste0(" ", base) |>
    lubridate::as_datetime(tz = lubridate::tz(x))

  if (isTRUE(force_tz)) {
    out |> lubridate::force_tz(tz)
  } else {
    out
  }
}
