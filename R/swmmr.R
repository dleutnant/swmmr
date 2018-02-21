#' \code{swmmr} package
#'
#' R Interface for US EPA's SWMM
#'
#' @docType package
#' @name swmmr
#' @importFrom purrr %>%
#' @importFrom purrr %||%
#' @importFrom utils tail
NULL

# inspired by https://github.com/jennybc
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                         "Date",
                                                         "Time",
                                                         "DateTime",
                                                         "value",
                                                         "Name",
                                                         "pos",
                                                         "id",
                                                         "polygon_is_closed",
                                                         "X-Coord", 
                                                         "Y-Coord",
                                                         "geometry",
                                                         "Project",
                                                         "LID Unit",
                                                         "group",
                                                         "data",
                                                         "Variable",
                                                         "Value"))


#' checks if required package is available
#' @keywords internal
check_pkg_avail <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Package ", x," needed for this function to work. Please install it.",
         call. = FALSE)
  }
}