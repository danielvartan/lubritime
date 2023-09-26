int_mean <- function(start, end, ambiguity = 24) {
  classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt")

  checkmate::assert_multi_class(start, classes)
  checkmate::assert_multi_class(end, classes)
  checkmate::assert_choice(ambiguity, c(0, 24, NA))

  start <- cycle_time(hms::hms(extract_seconds(start)),
                      cycle = lubridate::ddays())
  end <- cycle_time(hms::hms(extract_seconds(end)),
                    cycle = lubridate::ddays())
  interval <- rutils:::shush(assign_date(start, end, ambiguity = ambiguity))
  mean <- as.numeric(start) + (as.numeric(interval) / 2)

  hms::hms(mean)
}
