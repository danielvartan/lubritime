#' Extract seconds from a time object
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `extract_seconds()` extracts the seconds from a time object.
#'
#' @details
#'
#' ## `difftime` Objects
#'
#' [`difftime`][base::DateTimeClasses] objects are first converted to
#' [`hms`][hms::hms], which represent time in seconds. That way
#' there is no conflict with the `units` attribute.
#'
#' ## `Period` Objects
#'
#' [`Period`][lubridate::period] objects are a special type of object
#' developed by the [lubridate][lubridate::lubridate-package] team that
#' represents "human units", ignoring possible timeline irregularities. This
#' means that 1 day as a `Period` can have different time spans when considering
#' timeline irregularities.
#'
#' Since the time span of a [`Period`][lubridate::period()] object can
#' fluctuate, `extract_seconds()` does not accept this type of
#' object. You can transform it to a [`Duration`][lubridate::duration()] object
#' and still use the function, but be aware that this can produce errors.
#'
#' ## `POSIXt` Objects
#'
#' [`POSIXt`][base::DateTimeClasses] objects are converted to
#' [`hms`][hms::hms], stripping away the date component and retaining only the
#' time.
#'
#' ## `Interval` Objects
#'
#' For [`Interval`][lubridate::interval] objects the function extracts only
#' the duration of the time span.
#'
#' @param x A [`numeric`][base::numeric] vector.
#'
#' @return A [`numeric`][base::numeric] vector.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' extract_seconds(lubridate::ddays(1))
#' #> [1] 86400 # Expected
#'
#' extract_seconds(lubridate::as.difftime(1, units = "hours"))
#' #> [1] 3600 # Expected
#'
#' extract_seconds(hms::as_hms("01:00:00"))
#' #> [1] 3600 # Expected
#'
#' extract_seconds(lubridate::as_datetime("2020-01-01 00:00:00"))
#' #> [1] 0 # Expected
#'
#' extract_seconds(
#'   lubridate::interval(
#'     start = lubridate::as_datetime("2020-01-01 00:00:00"),
#'     end = lubridate::as_datetime("2021-01-01 00:00:00")
#'   )
#' )
#' #> [1] 31622400 # Expected
extract_seconds <- function(x) {
  UseMethod("extract_seconds")
}

#' @rdname extract_seconds
#' @export
extract_seconds.Duration <- function(x) {
  as.numeric(x)
}

#' @rdname extract_seconds
#' @export
extract_seconds.difftime <- function(x) {
  x |> hms::as_hms() |> as.numeric()
}

#' @rdname extract_seconds
#' @export
extract_seconds.POSIXt <- function(x) {
  x |> hms::as_hms() |> as.numeric()
}

#' @rdname extract_seconds
#' @export
extract_seconds.Interval <- function(x) {
  as.numeric(x)
}
