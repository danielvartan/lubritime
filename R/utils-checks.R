# `*_numeric_()` was created as a workaround to deal with cases like
# `is.numeric(lubridate::duration())`. See
# https://github.com/tidyverse/lubridate/issues/942 to learn more.

test_numeric_ <- function(x, lower = -Inf, upper = Inf, any.missing = TRUE,
                          null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    classes <- c("integer", "double", "numeric")

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (checkmate::test_subset(class(x)[1], classes) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        checkmate::test_subset(class(x)[1], classes)
    }
}

check_numeric_ <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                           null.ok = FALSE,
                         name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    classes <- c("integer", "double", "numeric")

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(gutils:::single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(gutils:::single_quote_(name), " cannot have 'NULL' values")
    } else if (checkmate::test_subset(class(x)[1], classes) &&
               !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (checkmate::test_subset(class(x)[1], classes) &&
               !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_numeric_(x)) {
        paste0("Must be of type 'numeric', not ", gutils:::class_collapse(x))
    } else {
        TRUE
    }
}

assert_numeric_ <- checkmate::makeAssertionFunction(check_numeric_)

test_duration <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                          null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (lubridate::is.duration(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        lubridate::is.duration(x)
    }
}

check_duration <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                           null.ok = FALSE,
                           name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(gutils:::single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(gutils:::single_quote_(name), " cannot have 'NULL' values")
    } else if (lubridate::is.duration(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.duration(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_duration(x)) {
        paste0("Must be of type 'Duration', not ", gutils:::class_collapse(x))
    } else {
        TRUE
    }
}

assert_duration <- checkmate::makeAssertionFunction(check_duration)

test_hms <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                          null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (hms::is_hms(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        hms::is_hms(x)
    }
}

check_hms <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                           null.ok = FALSE,
                           name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(gutils:::single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(gutils:::single_quote_(name), " cannot have 'NULL' values")
    } else if (hms::is_hms(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (hms::is_hms(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_hms(x)) {
        paste0("Must be of type 'hms', not ", gutils:::class_collapse(x))
    } else {
        TRUE
    }
}

assert_hms <- checkmate::makeAssertionFunction(check_hms)

test_posixt <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                     null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (lubridate::is.POSIXt(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        lubridate::is.POSIXt(x)
    }
}

check_posixt <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                      null.ok = FALSE,
                      name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(gutils:::single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(gutils:::single_quote_(name), " cannot have 'NULL' values")
    } else if (lubridate::is.POSIXt(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.POSIXt(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_posixt(x)) {
        paste0("Must be of type 'POSIXct' or 'POSIXlt', not ",
               gutils:::class_collapse(x))
    } else {
        TRUE
    }
}

assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

test_temporal <- function(x, any.missing = TRUE, null.ok = FALSE, rm = NULL) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)
    checkmate::assert_character(rm, any.missing = FALSE, null.ok = TRUE)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else {
        classes <- c("Duration", "Period", "difftime", "hms", "Date", "POSIXct",
                     "POSIXlt", "Interval")

        if (!is.null(rm)) {
            rm <- paste0("^", rm, "$", collapse = "|")
            classes <- gutils:::str_subset_(classes, rm, negate = TRUE)
        }

        checkmate::test_subset(class(x)[1], classes)
    }
}

check_temporal <- function(x, any.missing = TRUE, null.ok = FALSE,
                       name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(gutils:::single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(gutils:::single_quote_(name), " cannot have 'NULL' values")
    } else if (!test_temporal(x)) {
        paste0("Must be a temporal object (see 'test_temporal()'), ",
               "not ", gutils:::class_collapse(x))
    } else {
        TRUE
    }
}

assert_temporal <- checkmate::makeAssertionFunction(check_temporal)
