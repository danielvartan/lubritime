int_overlap <- function(int_1, int_2) {
  checkmate::assert_class(int_1, "Interval")
  checkmate::assert_numeric(as.numeric(int_1), len = 1)
  checkmate::assert_class(int_2, "Interval")
  checkmate::assert_numeric(as.numeric(int_2), min.len = 1)

  ## Similar to watching a planet travelling in front of its star

  #                         (int_1) (the "star")
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
  #              (int_2) (the "planet")
  # ----------------------------------------------------------->
  # -                                                          +

  out <- mapply(
    get_int_overlap, int_1, int_2, SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  do.call("c", out)
}

get_int_overlap <- function(int_1, int_2) {
  if (isFALSE(lubridate::int_overlaps(int_1, int_2))) {
    return(gutils::na_as(int_1))
  } else {
    int_start <- dplyr::if_else(
      lubridate::int_start(int_1) >= lubridate::int_start(int_2),
      lubridate::int_start(int_1), lubridate::int_start(int_2)
    )

    int_end <- dplyr::if_else(
      lubridate::int_end(int_1) <= lubridate::int_end(int_2),
      lubridate::int_end(int_1), lubridate::int_end(int_2)
    )

    lubridate::interval(int_start, int_end)
  }
}
