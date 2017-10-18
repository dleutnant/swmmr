#' \code{swmmr} package
#'
#' R Interface for US EPA's SWMM
#'
#' @docType package
#' @name swmmr
#' @importFrom purrr %>%
#' @importFrom purrr %||%
NULL

# inspired by https://github.com/jennybc
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c(".", 
                                                         "X-Coord", 
                                                         "Y-Coord",
                                                         "geometry"))