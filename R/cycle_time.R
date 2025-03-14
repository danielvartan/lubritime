#' Cycle time objects
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `cycle_time()` cycles time span objects in a predetermined cycle length,
#' adapting linear time objects into a circular time frame.
#'
#' @details
#'
#' ## Linear versus circular time
#'
#' Time can have different "shapes".
#'
#' If the objective is to measure the duration (time span) of an event, time is
#' usually measured considering a linear frame, with a fixed point of
#' [origin](https://en.wikipedia.org/wiki/Origin_(mathematics)). In this
#' context, the time value distance itself to infinity in relation to the
#' origin.
#'
#' ```
#'                                    B
#'                              |----------|
#'                                         A
#'                              |---------------------|
#'  - inf                                                inf +
#' <----------------------------|----------|----------|------->
#'  s                           0          5          10     s
#'                            origin
#'
#' A + B = 10 + 5 = 15s
#' ```
#'
#' But that's not the only possible "shape" of time, as it can also be measured
#' in other contexts.
#'
#' In a "time of day" context, the time will be linked to the rotation of the
#' earth, "resetting" when a new rotation cycle starts. That brings a different
#' kind of shape to time: a circular shape. With this shape the time value
#' encounters the origin at the beginning and end of each cycle.
#'
#' ```
#'                - <--- h ---> +
#'                     origin
#'                 . . . 0 . . .
#'              .                 .
#'             .                   .
#'            .                     .
#'           .                       .
#'          .                         .
#'          18                        6
#'          .                         .
#'           .                       .
#'            .                     .
#'             .                   .
#'              .                 .
#'                 . . . 12 . . .
#'
#' 18 + 6 = 0h
#' ```
#'
#' If we transpose this circular time frame to a linear one, it would look like
#' this:
#'
#'
#' ```
#' <----|---------------|---------------|---------------|----->
#'     0h              12h              0h             12h
#'   origin                           origin
#' ```
#'
#' Note that now the origin is not fixed, but cyclical.
#'
#' `cycle_time()` operates by converting linear time objects using a circular
#' approach relative to the cycle length (e.g, `cycle = 86400` (1 day)).
#'
#' ## Fractional time
#'
#' `cycle_time()` uses the [`%%`][base::Arithmetic] operator to cycle values.
#' Hence, it can be subject to catastrophic loss of accuracy if `time` is
#' fractional and much larger than `cycle`. A warning is given if this is
#' detected.
#'
#' [`%%`][base::Arithmetic] is a `builtin` R function that operates like this:
#'
#' ```
#' function(a, b) {
#'     a - floor(a / b) * b
#' }
#' ````
#'
#' ## Negative time cycling
#'
#' If `time` have a negative value and `reverse == FALSE`, `cycle_time()` will
#' perform the cycle considering the absolute value of `time` and return the
#' result with a negative signal.
#'
#' However, If `time` have a negative value and `reverse == TRUE` (default),
#' `cycle_time()` will perform the cycle in reverse, relative to its origin.
#'
#' Example: If you have a -30h time span in a reversed cycle of 24h, the result
#' will be 18h. By removing the full cycles of -30h you will get -6h (-30 + 24),
#' and -6h relative to the origin will be 18h.
#'
#' ```
#'                - <--- h ---> +
#'                     origin
#'                 . . . 0 . . .
#'               .                 .
#'             .                   .
#'            .                     .
#'           .                       .
#'          .                         .
#'     (-6) 18                        6 (-18)
#'          .                         .
#'           .                       .
#'            .                     .
#'             .                   .
#'              .                 .
#'                 . . . 12 . . .
#'                     (-12)
#' ```
#'
#' ## `Period` objects
#'
#' [`Period`][lubridate::period()] objects are a special type of object
#' developed by the [lubridate][lubridate::lubridate-package] team that
#' represents "human units", ignoring possible timeline irregularities. That is
#' to say that 1 day as [`Period`][lubridate::period()] can have different time
#' spans, when looking to a timeline after a irregularity event.
#'
#' Since the time span of a [`Period`][lubridate::period()] object can
#' fluctuate, `cycle_time()` don't accept this kind of object. You can transform
#' it to a [`Duration`][lubridate::duration()] object and still use the
#' function, but beware that this can produce errors.
#'
#' Learn more about [`Period`][lubridate::period()] objects in the [Dates and
#' times](https://r4ds.had.co.nz/dates-and-times.html#periods) chapter of
#' Wickham & Grolemund book (n.d.).
#'
#' @param time An object belonging to one of the following classes:
#'   [`numeric`][base::numeric()], [`Duration`][lubridate::duration()],
#'   [`difftime`][base::as.difftime()], or [`hms`][hms::hms()].
#' @param cycle A [`numeric`][base::numeric()] or
#'   [`Duration`][lubridate::duration()] object of length `1`, equal or greater
#'   than `0`, indicating the cycle length in seconds. See the Details section
#'   to learn more.
#' @param reverse (optional) a [`logical`][logical()] value indicating if the
#'   function must use a reverse cycle for negative values in `time`. See
#'   the Details section to learn more (default: `TRUE`).
#'
#' @return The same type of object of `time` cycled with the `cycle` parameter.
#'
#' @family circular time functions
#' @template references_a
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' time <- lubridate::dhours(25)
#' cycle <- lubridate::ddays(1)
#' cycle_time(time, cycle)
#' #> [1] "3600s (~1 hours)" # Expected
#'
#' time <- lubridate::dhours(-25)
#' cycle <- lubridate::ddays(1)
#' reverse <- FALSE
#' cycle_time(time, cycle, reverse)
#' #> [1] "-3600s (~-1 hours)" # Expected
#'
#' time <- lubridate::dhours(-25)
#' cycle <- lubridate::ddays(1)
#' reverse <- TRUE
#' cycle_time(time, cycle, reverse)
#' #> [1] "82800s (~23 hours)" # Expected
#'
#' ## Vector example
#'
#' time <- c(lubridate::dmonths(24), lubridate::dmonths(13))
#' cycle <- lubridate::dyears(1)
#' cycle_time(time, cycle)
#' #> [1] "0s"                     "2629800s (~4.35 weeks)" # Expected
#'
#' time <- c(lubridate::dmonths(24), lubridate::dmonths(-13))
#' cycle <- lubridate::dyears(1)
#' reverse <- FALSE
#' cycle_time(time, cycle, reverse)
#' #> [1] "0s"                       "-2629800s (~-4.35 weeks)" # Expected
#'
#' time <- c(lubridate::dmonths(24), lubridate::dmonths(-13))
#' cycle <- lubridate::dyears(1)
#' reverse <- TRUE
#' cycle_time(time, cycle, reverse)
#' #> [1] "0s"                       "28927800s (~47.83 weeks)" # Expected
cycle_time <- function(time, cycle, reverse = TRUE) {
  UseMethod("cycle_time")
}

#' @rdname cycle_time
#' @export
cycle_time.numeric <- function(time, cycle, reverse = TRUE) {
  time |> cycle_time_build(cycle, reverse)
}

#' @rdname cycle_time
#' @export
cycle_time.Duration <- function(time, cycle, reverse = TRUE) {
  time |>
    cycle_time_build(cycle, reverse) |>
    lubridate::dseconds()
}

#' @rdname cycle_time
#' @export
cycle_time.difftime <- function(time, cycle, reverse = TRUE) {
  out <- time
  units(out) <- "secs"

  out <-
    out |>
    cycle_time_build(cycle, reverse) |>
    lubridate::seconds() |>
    lubridate::as.difftime(units = "secs")

  units(out) <- units(time)

  out
}

#' @rdname cycle_time
#' @export
cycle_time.hms <- function(time, cycle, reverse = TRUE) {
  time |>
    cycle_time_build(cycle, reverse) |>
    hms::hms()
}

cycle_time_build <- function(time, cycle, reverse) {
  checkmate::assert_multi_class(cycle, c("numeric", "Duration"))
  checkmate::assert_number(as.numeric(cycle), lower = 0, null.ok = FALSE)
  checkmate::assert_flag(reverse)

  cycle <- cycle |> as.numeric()

  sign <- time |>
    as.numeric() |>
    sign()

  out <- time |>
    as.numeric() |>
    abs() %>% # Don't change
    `%%`(cycle)

  if (isTRUE(reverse)) {
    dplyr::if_else(sign < 0, cycle - out, out)
  } else {
    dplyr::if_else(sign < 0, - out, out)
  }
}
