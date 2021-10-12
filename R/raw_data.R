#' Get paths to `lubritime` raw data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `raw_data()` returns the raw data paths of the `lubritime` package.
#'
#' @param file (optional) a `character` object indicating the raw data file
#'   name(s). If `NULL`, all raw data file names will be printed (default:
#'   `NULL`).
#'
#' @return
#'
#' * If `file = NULL`, a `character` object with all file names available.
#' * If `file != NULL`, a string with the file name path.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## To list all raw data file names available
#'
#' raw_data()
#'
#' ## To get the file path from a specific raw data
#'
#' raw_data(raw_data()[1])}
raw_data <- function(file = NULL) {
    checkmate::assert_character(file, min.len = 1, null.ok = TRUE)

    if (is.null(file)) {
        list.files(system.file("extdata", package = "lubritime"))
    } else {
        system.file("extdata", file, package = "lubritime", mustWork = TRUE)
    }
}
