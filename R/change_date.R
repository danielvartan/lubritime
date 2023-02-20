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
