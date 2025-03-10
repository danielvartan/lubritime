#' Transform `24:00:00` to `00:00:00` for `hms` objects
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `fix_hms()` is a quick fix for [`hms`][hms::hms] objects that have a
#' time of `24:00:00`. This function transforms the time to `00:00:00` for
#' consistency with the [`POSIXt`][base::DateTimeClasses] class.
#'
#' @param x A [`hms`][hms::hms] vector.
#'
#' @return A [`hms`][hms::hms] vector.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' hms::parse_hm("24:00") |> fix_hms()
#' #> 00:00:00 # Expected
#'
#' hms::parse_hm(c("02:00", "24:00", "18:00")) |> fix_hms()
#' 02:00:00 # Expected
#' 00:00:00
#' 18:00:00
fix_hms <- function(x) {
  prettycheck::assert_hms(x)

  dplyr::case_when(
    x == hms::parse_hm("24:00") ~ hms::hms(0),
    TRUE ~ x
  )
}
