flat_posixt <- function(posixt, base = as.Date("1970-01-01"),
                        force_tz = TRUE, tz = "UTC") {
  rutils:::assert_posixt(posixt, null.ok = FALSE)
  checkmate::assert_date(base, len = 1, all.missing = FALSE)
  checkmate::assert_flag(force_tz)
  checkmate::assert_choice(tz, OlsonNames())

  lubridate::date(posixt) <- base

  if (isTRUE(force_tz)) {
    lubridate::force_tz(posixt, tz)
  } else {
    posixt
  }
}

flat_posixt_date <- function(posixt, base = as.Date("1970-01-01")) {
  rutils:::assert_posixt(posixt, null.ok = FALSE)
  checkmate::assert_date(base, len = 1, any.missing = FALSE)

  posixt |> lubridate::`date<-`(base) |> c()
}

flat_posixt_hour <- function(posixt, base = hms::parse_hms("00:00:00")) {
  rutils:::assert_posixt(posixt)
  rutils:::assert_hms(base, any.missing = FALSE)

  posixt |>
    lubridate::date() |>
    paste0(" ", base) |>
    lubridate::as_datetime(tz = lubridate::tz(posixt))
}
