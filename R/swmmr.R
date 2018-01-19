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
                                                         "geometry"))