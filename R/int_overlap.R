# int_1 <- lubridate::interval(
#   start = lubridate::as_datetime("2020-01-01 00:00:00"),
#   end = lubridate::as_datetime("2021-01-01 00:00:00"),
# )
#
# int_2 <- lubridate::interval(
#   start = lubridate::as_datetime("2020-05-01 00:00:00"),
#   end = lubridate::as_datetime("2021-05-01 00:00:00"),
# )

# int_overlap(int_1, int_2)

int_overlap <- function(int_1, int_2) {
  checkmate::assert_class(int_1, "Interval")
  prettycheck::assert_numeric(as.numeric(int_1), len = 1)
  checkmate::assert_class(int_2, "Interval")
  prettycheck::assert_numeric(as.numeric(int_2), min_len = 1)

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
  if (isTRUE(lubridate::int_overlaps(int_1, int_2))) {
    int_start <- dplyr::if_else(
      lubridate::int_start(int_1) >= lubridate::int_start(int_2),
      lubridate::int_start(int_1), lubridate::int_start(int_2)
    )

    int_end <- dplyr::if_else(
      lubridate::int_end(int_1) <= lubridate::int_end(int_2),
      lubridate::int_end(int_1), lubridate::int_end(int_2)
    )

    lubridate::interval(int_start, int_end)
  } else {
    rutils::na_as(int_1)
  }
}
