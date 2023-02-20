fix_hms <- function(x) {
    checkmate::assert_class(x, "hms")

    dplyr::case_when(
        x == hms::parse_hm("24:00") ~ hms::hms(0),
        TRUE ~ x
    )
}
