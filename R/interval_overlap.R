interval_overlap <- function(int1, int2) {
    checkmate::assert_class(int1, "Interval")
    checkmate::assert_numeric(as.numeric(int1), len = 1)
    checkmate::assert_class(int2, "Interval")
    checkmate::assert_numeric(as.numeric(int2), min.len = 1)

    overlap <- function(int1, int2) {
        if (isFALSE(lubridate::int_overlaps(int1, int2))) {
            return(gutils::na_as(int1))
        } else{
            int_start <- dplyr::if_else(
                lubridate::int_start(int1) >= lubridate::int_start(int2),
                lubridate::int_start(int1), lubridate::int_start(int2))

            int_end <- dplyr::if_else(
                lubridate::int_end(int1) <= lubridate::int_end(int2),
                lubridate::int_end(int1), lubridate::int_end(int2))

            lubridate::interval(int_start, int_end)
        }
    }

    ## Similar to watching a planet travelling in front of its star

    #                         (int1) (the "star")
    #                 +______________________+_________+
    #                 |                      |         |
    #                 |  This interval is stationary   |
    #                 +______________________+_________+
    #                 |                      |
    #                 |<////////////////////>|
    #                 |           ?          |
    #         +_______+______________________+
    #         |       |                      |
    #         |       |                      |
    #         +_______+______________________+
    #              (int2) (the "planet")
    # ----------------------------------------------------------->
    # -                                                          +

    out <- mapply(overlap, int1, int2, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    do.call("c", out)
}
