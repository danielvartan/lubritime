#' Change `Date` components
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `change_*()` allows you to change the day, month, year, or date of a
#' [`Date`][base::Date], [`POSIXct`][base::DateTimeClasses], or
#' [`POSIXlt`][base::DateTimeClasses] object.
#'
#' @param x A [`Date`][base::Date], [`POSIXct`][base::DateTimeClasses], or
#'   [`POSIXlt`][base::DateTimeClasses] object.
#' @param day A [`numeric`][base::as.numeric()] object representing the day of
#'   the month.
#' @param month A [`numeric`][base::as.numeric()] object representing the month
#'  of the year.
#' @param year A [`numeric`][base::as.numeric()] object representing the year.
#' @param date A [`character`][base::as.character()] or [`Date`][base::Date]
#'   object representing the new date.
#'
#' @return A [`Date`][base::Date], [`POSIXct`][base::DateTimeClasses], or
#'   [`POSIXlt`][base::DateTimeClasses] object with the new day, month, year,
#'   or date.
#'
#' @family parsing/conversion functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' change_day(as.Date("2021-01-01"), 15)
#' #> [1] "2021-01-15" # Expected
#'
#' change_month(as.POSIXct("2021-01-01 12:00:00", tz = "UTC"), 12)
#' #> [1] "2021-12-01 12:00:00 UTC" # Expected
#'
#' change_year(as.POSIXlt("2021-01-01 12:00:00", tz = "UTC"), 2022)
#' #> [1] "2022-01-01 12:00:00 UTC" # Expected
#'
#' change_date(
#'   x = as.POSIXlt("2021-01-01 12:00:00", tz = "UTC"),
#'   date = as.Date("2000-01-01")
#' )
#' #> [1] "2000-01-01 12:00:00 UTC" # Expected
#'
#' ## Vector example
#'
#' x <- as.Date(c("2021-01-01", "2021-01-01"))
#' change_day(x, 15)
#' #> [1] "2021-01-15" "2021-01-15" # Expected
#'
#' x <- as.POSIXct(c("2021-01-01 12:00:00", "2021-01-01 12:00:00"), tz = "UTC")
#' change_month(x, 10:11)
#' #> [1] "2021-10-01 12:00:00 UTC" "2021-11-01 12:00:00 UTC" # Expected
#'
#' x <- as.POSIXlt(c("2021-01-01 12:00:00", "2021-01-01 12:00:00"), tz = "UTC")
#' change_year(x, 2022)
#' #> [1] "2022-01-01 12:00:00 UTC" "2022-01-01 12:00:00 UTC" # Expected
#'
#' x <- as.POSIXlt(c("2021-01-01 12:00:00", "2021-01-01 12:00:00"), tz = "UTC")
#' change_date(x, as.Date("2000-01-01"))
#' #> [1] "2000-01-01 12:00:00 UTC" "2000-01-01 12:00:00 UTC" # Expected
change_date <- function(x, date) {
  prettycheck:::assert_multi_class(x, c("Date", "POSIXct", "POSIXlt"))
  prettycheck:::assert_multi_class(date, c("character", "Date"))

  if (length(date) == 1) date <- rep(date, length(x))
  prettycheck:::assert_identical(x, date, type = "length")

  lubridate::`date<-`(x, date)
}

#' @rdname change_day
#' @export
change_day <- function(x, day) {
  prettycheck:::assert_multi_class(x, c("Date", "POSIXct", "POSIXlt"))
  prettycheck:::assert_numeric(day, lower = 1, upper = 31)

  if (length(day) == 1) day <- rep(day, length(x))
  prettycheck:::assert_identical(x, day, type = "length")

  if (any(lubridate::month(x) %in% c(4, 6, 9, 11), na.rm = TRUE)
      && day > 30) {
    cli::cli_abort(paste0(
      "You can't assign more than 30 days to April, June, ",
      "September, or November."
    ))
  }

  if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) && day > 28) {
    cli::cli_abort(paste0(
      "You can't assign more than 28 days to February in ",
      "non-leap years."
    ))
  }

  if (any(lubridate::month(x) == 2 & lubridate::leap_year(x), na.rm = TRUE) &&
      day > 29) {
    cli::cli_abort(paste0(
      "You can't assign more than 29 days to February in a leap year."
    ))
  }

  lubridate::`day<-`(x, day)
}

#' @rdname change_day
#' @export
change_month <- function(x, month) {
  prettycheck:::assert_multi_class(x, c("Date", "POSIXct", "POSIXlt"))
  prettycheck:::assert_numeric(month, lower = 1, upper = 12)

  if (length(month) == 1) month <- rep(month, length(x))
  prettycheck:::assert_identical(x, month, type = "length")

  lubridate::`month<-`(x, month)
}

#' @rdname change_day
#' @export
change_year <- function(x, year) {
  prettycheck:::assert_multi_class(x, c("Date", "POSIXct", "POSIXlt"))

  if (length(year) == 1) year <- rep(year, length(x))
  prettycheck:::assert_identical(x, year, type = "length")

  lubridate::`year<-`(x, year)
}
