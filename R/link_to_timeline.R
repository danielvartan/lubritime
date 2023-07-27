link_to_timeline <- function(x, threshold = hms::parse_hms("12:00:00")) {
  checkmate::assert_multi_class(x, c("hms", "POSIXt"))
  gutils:::assert_hms(
    threshold, lower = hms::hms(0), upper = hms::parse_hms("23:59:59")
  )

  if (hms::is_hms(x)) x <- as.POSIXct(x)
  x <- flat_posixt(x)

  dplyr::case_when(
    hms::as_hms(x) < threshold ~ lubridate::`day<-`(x, 2),
    TRUE ~ x
  )
}

midday_change <- function(x) {
  lifecycle::deprecate_soft(
    "2023-05-02", "midday_change()", "link_to_timeline()"
  )

  link_to_timeline(x, threshold = hms::parse_hms("12:00:00"))
}
