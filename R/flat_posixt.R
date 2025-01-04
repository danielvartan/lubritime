flat_posixt <- function(
    x,
    base = as.Date("1970-01-01"),
    force_tz = TRUE,
    tz = "UTC"
  ) {
  prettycheck:::assert_posixt(x, null_ok = FALSE)
  prettycheck:::assert_date(base, len = 1, all.missing = FALSE)
  prettycheck:::assert_flag(force_tz)
  prettycheck:::assert_choice(tz, OlsonNames())

  lubridate::date(x) <- base

  if (isTRUE(force_tz)) {
    lubridate::force_tz(x, tz)
  } else {
    x
  }
}

flat_posixt_date <- function(x, base = as.Date("1970-01-01")) {
  prettycheck:::assert_posixt(x, null_ok = FALSE)
  prettycheck:::assert_date(base, len = 1, any.missing = FALSE)

  x |>
    lubridate::`date<-`(base) |>
    c()
}

flat_posixt_hour <- function(x, base = hms::parse_hms("00:00:00")) {
  prettycheck:::assert_posixt(x)
  prettycheck:::assert_hms(base, any_missing = FALSE)

  x |>
    lubridate::date() |>
    paste0(" ", base) |>
    lubridate::as_datetime(tz = lubridate::tz(x))
}
