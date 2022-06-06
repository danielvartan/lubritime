flat_posixt_date <- function(posixt, base = as.Date("1970-01-01")) {
    gutils:::assert_posixt(posixt, null.ok = FALSE)
    checkmate::assert_date(base, len = 1, any.missing = FALSE)

    posixt %>% lubridate::`date<-`(base) %>% c()
}

flat_posixt_hour <- function(posixt, base = hms::parse_hms("00:00:00")) {
    gutils:::assert_posixt(posixt)
    gutils:::assert_hms(base, any.missing = FALSE)

    posixt %>%
        lubridate::date() %>%
        paste0(" ", base) %>%
        lubridate::as_datetime(tz = lubridate::tz(posixt))
}

midday_change <- function(time) {
    checkmate::assert_multi_class(time, c("hms", "POSIXct", "POSIXlt"))

    if (hms::is_hms(time)) time <- as.POSIXct(time)
    time <- flat_posixt(time)

    dplyr::case_when(
        lubridate::hour(time) < 12 ~ lubridate::`day<-`(time, 2),
        TRUE ~ time
    )
}

interval_mean <- function(start, end, ambiguity = 24) {
    classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(start, classes)
    checkmate::assert_multi_class(end, classes)
    checkmate::assert_choice(ambiguity, c(0, 24 , NA))

    start <- cycle_time(hms::hms(extract_seconds(start)),
                        cycle = lubridate::ddays())
    end <- cycle_time(hms::hms(extract_seconds(end)),
                      cycle = lubridate::ddays())
    interval <- shush(assign_date(start, end, ambiguity = ambiguity))
    mean <- as.numeric(start) + (as.numeric(interval) / 2)

    hms::hms(mean)
}

extract_seconds <- function(x) {
    classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt",
                 "Interval")

    checkmate::assert_multi_class(x, classes)

    if (lubridate::is.POSIXt(x) || lubridate::is.difftime(x)) {
        as.numeric(hms::as_hms(x))
    } else {
        as.numeric(x)
    }
}

shush <- function(x, quiet = TRUE) {
    if (isTRUE(quiet)) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }
}
