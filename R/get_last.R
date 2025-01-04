#' Get last week or month
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `get_last_week()` and `get_last_month()` return the last week or month
#' interval, respectively, based on a reference date.
#'
#' @param date (optional) A [`Date`][base::as.Date()] object with the reference
#'   date (default: [Sys.Date()]).
#'
#' @returns An [`Interval`][lubridate::interval()] object with the last week or
#'   month.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' get_last_week(as.Date("2024-12-06"))
#' #> [1] 2024-11-24 UTC--2024-11-30 UTC # Expected
#'
#' get_last_month(as.Date("2024-12-06"))
#' #> [1] 2024-11-01 UTC--2024-11-30 UTC # Expected
#'
#' ## Vector example
#'
#' get_last_week(as.Date(c("2024-12-06", "2024-12-15")))
#' #> [1] 2024-11-24 UTC--2024-11-30 UTC # Expected
#' #' [1] 2024-12-08 UTC--2024-12-14 UTC # Expected
#'
#' get_last_month(as.Date(c("2024-12-06", "2024-11-15")))
#' #> [1] 2024-11-01 UTC--2024-11-30 UTC  # Expected
#' #> [2] 2024-10-01 UTC--2024-10-31 UTC # Expected
get_last_week <- function(date = Sys.Date()) {
  prettycheck:::assert_date(date)

  lubridate::interval(
    start =
      lubridate::floor_date(date, unit = "week") -
      lubridate::period(1, "week"),
    end = lubridate::floor_date(date, unit = "week") - lubridate::days(1)
  )
}

#' @rdname get_last_week
#' @export
get_last_month <- function(date = Sys.Date()) {
  prettycheck:::assert_date(date)

  lubridate::interval(
    start =
      lubridate::floor_date(date, unit = "month") -
      lubridate::period(1, "month"),
    end = lubridate::floor_date(date, unit = "month") -
      lubridate::period(1, "day")
  )
}
