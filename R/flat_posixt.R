flat_posixt <- function(posixt, base = as.Date("1970-01-01"),
                        force_tz = TRUE, tz = "UTC") {
  prettycheck:::assert_posixt(posixt, null_ok = FALSE)
  prettycheck:::assert_date(base, len = 1, all.missing = FALSE)
  prettycheck:::assert_flag(force_tz)
  prettycheck:::assert_choice(tz, OlsonNames())

  lubridate::date(posixt) <- base

  if (isTRUE(force_tz)) {
    lubridate::force_tz(posixt, tz)
  } else {
    posixt
  }
}

flat_posixt_date <- function(posixt, base = as.Date("1970-01-01")) {
  prettycheck:::assert_posixt(posixt, null_ok = FALSE)
  prettycheck:::assert_date(base, len = 1, any.missing = FALSE)

  posixt |> lubridate::`date<-`(base) |> c()
}

flat_posixt_hour <- function(posixt, base = hms::parse_hms("00:00:00")) {
  prettycheck:::assert_posixt(posixt)
  prettycheck:::assert_hms(base, any_missing = FALSE)

  posixt |>
    lubridate::date() |>
    paste0(" ", base) |>
    lubridate::as_datetime(tz = lubridate::tz(posixt))
}
