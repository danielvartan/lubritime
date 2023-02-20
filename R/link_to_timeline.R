midday_change <- function(time) {
    checkmate::assert_multi_class(time, c("hms", "POSIXct", "POSIXlt"))

    if (hms::is_hms(time)) time <- as.POSIXct(time)
    time <- flat_posixt(time)

    dplyr::case_when(
        lubridate::hour(time) < 12 ~ lubridate::`day<-`(time, 2),
        TRUE ~ time
    )
}
