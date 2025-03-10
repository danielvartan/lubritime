#' Convert a numerical object to a specific unit
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `convert_to_unit()` allows you to convert a numerical object to different
#' kind of units.
#'
#' See the *Units* section for more information about the units that
#' `convert_to_unit()` can handle.
#'
#' @param x A [`numeric`][base::is.numeric()] object.
#' @param from A string indicating the unit of `x` (see
#'   the *Units* section to learn more) (default: `"seconds"`).
#' @param to (optional) a string indicating the desire output unit
#'   (see the *Units* section to learn more) (default: `"hours"`).
#'
#' @section Units:
#'
#' Here are the units that `convert_to_unit()` can handle for now:
#'
#' - years.
#' - weeks.
#' - days.
#' - hours.
#' - minutes.
#' - seconds.
#' - milliseconds.
#' - microseconds.
#' - nanoseconds.
#' - degrees.
#' - radians.
#'
#' @return A [`numeric`][base::as.numeric()] object representing the decimals
#'   hours `x`.
#'
#' @family parsing/conversion functions
#' @export
#'
#' @examples
#' convert_to_unit(60, "seconds", "minutes")
#' #> [1] 1
#'
#' convert_to_unit(1, "minute", "hours")
#' #> [1] 0.01666667
#'
#' convert_to_unit(1, "hour", "days")
#' #> [1] 0.0416666
#'
#' convert_to_unit(1, "day", "weeks")
#' #> [1] 0.1428571
#'
#' convert_to_unit(7, "days", "weeks")
#' #> [1] 1
#'
#' convert_to_unit(1, "week", "days")
#' #> [1] 7
convert_to_unit <- function(x, from = "seconds", to = "hours") {
  prettycheck::assert_numeric(x)

  from <- normalize_unit(from)
  to <- normalize_unit(to)

  year_week_warning(to)

  x <- x |> convert_to_seconds(from)

  switch(
    to,
    years = x / as.numeric(lubridate::dyears()),
    months = x / as.numeric(lubridate::dmonths()),
    weeks = x / as.numeric(lubridate::dweeks()),
    days = x / as.numeric(lubridate::ddays()),
    hours = x / as.numeric(lubridate::dhours()),
    minutes = x / as.numeric(lubridate::dminutes()),
    seconds = x,
    milliseconds = x * 1000,
    microseconds = x * 1000000,
    nanoseconds = x * 1000000000,
    degrees = x * deg_second,
    radians = x * rad_second
  )
}

convert_to_seconds <- function(x, unit) {
  prettycheck::assert_numeric(x)

  unit <- normalize_unit(unit)

  year_week_warning(unit)

  switch(
    unit,
    years = x * as.numeric(lubridate::dyears()),
    months = x * as.numeric(lubridate::dmonths()),
    weeks = x * as.numeric(lubridate::dweeks()),
    days = x * as.numeric(lubridate::ddays()),
    hours = x * as.numeric(lubridate::dhours()),
    minutes = x * as.numeric(lubridate::dminutes()),
    seconds = x,
    milliseconds = x / 1000,
    microseconds = x / 1000000,
    nanoseconds = x / 1000000000,
    degrees = x / deg_second,
    radians = x / rad_second
  )
}

convert_temporal_obj_to_seconds <- function(x,ignore_date = TRUE) { #nolint
  prettycheck::assert_temporal(x)

  if (lubridate::is.duration(x) ||
        lubridate::is.period(x) ||
        hms::is_hms(x) ||
        lubridate::is.interval(x)) {
    as.numeric(x)
  } else if (class(x)[1] == "difftime") {
    as.numeric(hms::as_hms(x))
  } else if (lubridate::is.Date(x)) {
    if (isTRUE(ignore_date)) {
      as.numeric(0)
    } else {
      as.numeric(x) * as.numeric(lubridate::ddays())
    }
  } else if (lubridate::is.POSIXt(x)) {
    if (isTRUE(ignore_date)) {
      as.numeric(hms::as_hms(x))
    } else {
      as.numeric(x)
    }
  }
}

normalize_unit <- function(unit) {
  out <- switch(
    unit,
    year = "years",
    years = "years",
    y = "years",
    month = "months",
    months = "months",
    m = "months",
    week = "weeks",
    weeks = "weeks",
    w = "weeks",
    day = "days",
    days = "days",
    d = "days",
    hour = "hours",
    hours = "hours",
    h = "hours",
    H = "hours",
    minute = "minutes",
    minutes = "minutes",
    min = "minutes",
    M = "minutes",
    second = "seconds",
    seconds = "seconds",
    sec = "seconds",
    s = "seconds",
    S = "seconds",
    millisecond = "milliseconds",
    milliseconds = "milliseconds",
    ms = "milliseconds",
    microsecond = "microseconds",
    microseconds = "microseconds",
    us = "microseconds",
    nanosecond = "nanoseconds",
    nanoseconds = "nanoseconds",
    ns = "nanoseconds",
    degree = "degrees",
    degrees = "degrees",
    deg = "degrees",
    radian = "radians",
    radians = "radians",
    rad = "radians"
  )

  if (length(out) == 0) {
    out <- temporal_units[
      stringdist::amatch(
        unit,
        temporal_units,
        maxDist = 1
      )
    ]

    if (length(out) == 0 || unit == "") {
      if (unit == "") unit <- '""'

      cli::cli_abort("Can't find the {.strong {cli::col_red(unit)}} unit.")
    } else {
      out <- out |> normalize_unit()
    }
  }

  out
}

year_week_warning <- function(unit) {
  checkmate::assert_string(unit)

  if (unit == "years" || unit == "months") {
    cli::cli_alert_warning(
      paste0(
        "Please note that the units ",
        "{.strong {cli::col_red('years')}}", " and ",
        "{.strong {cli::col_red('months')}}", " ",
        "can have a different number of days depending on the context.", " ",
        "This function assumes that a year has ",
        "{.strong 365.25}", " ",
        "days and a month has", " ",
        "{.strong 30.4375}", " ",
        "days."
      )
    )

    invisible()
  } else {
    invisible()
  }
}

temporal_units <- c(
  c("year", "years", "y"),
  c("month", "months", "m"),
  c("week", "weeks", "w"),
  c("day", "days", "d"),
  c("hour", "hours", "h", "H"),
  c("minute", "minutes", "min", "M"),
  c("second", "seconds", "sec", "s", "S"),
  c("millisecond", "milliseconds", "ms"),
  c("microsecond", "microseconds", "us"),
  c("nanosecond", "nanoseconds", "ns"),
  c("degree", "degrees", "deg"),
  c("radian", "radians", "rad")
)

## rad_second <- (2 * pi) / 24 / 60 / 60 # rad equivalent to 1 second
rad_second <- pi / 43200

## deg_second <- 15 / (60 * 60) # degree equivalent to 1 second
deg_second <- 15 / 3600
