extract_seconds <- function(x) {
  classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt",
               "Interval")

  checkmate::assert_multi_class(x, classes)

  if (lubridate::is.POSIXt(x) || lubridate::is.difftime(x)) {
    x |> hms::as_hms() |> as.numeric()
  } else {
    as.numeric(x)
  }
}
