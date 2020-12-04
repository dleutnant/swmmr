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
  
  # make sure the dev version of ggplot2 is installed (which has geom_sf)
  if (x == "ggplot2") {
    vc <- list(op = ">=", version = "2.2.1.9000")
  } else {
    vc <- NULL
  }

  if (!requireNamespace(x, quietly = TRUE, versionCheck = vc)) {
    clean_stop("Package ", ifelse(!is.null(vc), paste(x, vc$op, vc$version), x),
         " needed for this function to work. Please install it.")
  }
  
}