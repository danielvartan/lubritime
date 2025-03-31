# Remove `glue` dependency
single_quote <- function(x) paste0("'", x, "'")

# Borrowed from the `rutils` R package --- github.com/danielvartan/rutils
swap_if <- function(x, y, condition = TRUE) {
  prettycheck::assert_identical(x, y, type = "class")
  prettycheck::assert_identical(x, y, condition, type = "length")
  checkmate::assert_logical(condition)

  first_arg <- x
  second_arg <- y

  x <- dplyr::if_else(condition, second_arg, first_arg)
  y <- dplyr::if_else(condition, first_arg, second_arg)

  list(x = x, y = y)
}
