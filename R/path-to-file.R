#' Get file path to `afribats.csv` and `afribats_sf.gpkg` files
#'
#' afribat comes bundled with one csv and one gpkg file in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' The data in each file is the almost the same. In gpkg file, all missing positions coordinates were removed.
#'
#'
#' @param path Name of file in quotes with extension;
#' `"afribats.csv"` and `"afribats_sf.gpkg"` will work.
#' If `NULL`, the example files will be listed.
#' @export
#' @examples
#' # load libraries
#' library(sf)
#' library(readr)
#'
#' path_to_file("afribats.csv")
#' head(read_csv(path_to_file("afribats.csv")))
#' head(read_sf(path_to_file("afribats_sf.gpkg")))
#'
#' @source This function is adapted from `readxl::readxl_example()`.
path_to_file <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "afribat"))
  } else {
    system.file("extdata", path, package = "afribat", mustWork = TRUE)
  }
}
