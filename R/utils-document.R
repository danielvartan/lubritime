flat_posixt <- function(posixt, base = as.Date("1970-01-01"),
                        force_tz = TRUE, tz = "UTC") {
    gutils:::assert_posixt(posixt, null.ok = FALSE)
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

midday_change <- function(time) {
    checkmate::assert_multi_class(time, c("hms", "POSIXct", "POSIXlt"))

    if (hms::is_hms(time)) time <- as.POSIXct(time)
    time <- flat_posixt(time)

    dplyr::case_when(
        lubridate::hour(time) < 12 ~ change_day(time, 2),
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
    interval <- gutils:::shush(assign_date(start, end, ambiguity = ambiguity))
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

change_date <- function(x, date) {
    classes <- c("Date", "POSIXct", "POSIXlt")
    checkmate::assert_multi_class(x, classes)

    classes <- c("character", "Date")
    checkmate::assert_multi_class(date, classes)
    gutils:::assert_length_one(date)

    lubridate::date(x) <- date

    x
}

change_day <- function(x, day) {
    classes <- c("Date", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_number(day, lower = 1, upper = 31)

    if (any(lubridate::month(x) %in% c(4, 6, 9, 11), na.rm = TRUE)
        && day > 30) {
        cli::cli_abort(paste0(
            "You can't assign more than 30 days to April, June, ",
            "September, or November."
        ))
    }

    if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) && day > 28) {
        cli::cli_abort(paste0(
            "You can't assign more than 28 days to February in ",
            "non-leap years."
        ))
    }

    if (any(lubridate::month(x) == 2 & lubridate::leap_year(x), na.rm = TRUE) &&
        day > 29) {
        cli::cli_abort(paste0(
            "You can't assign more than 29 days to February in a leap year."
        ))
    }

    lubridate::day(x) <- day

    x
}

